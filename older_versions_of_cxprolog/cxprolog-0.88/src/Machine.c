/*
 *   This file is part of the CxProlog system

 *   Machine.c
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"


/* MACHINE REGISTERS */

Hdl P = &NotRunning, CP, H, HB, TR, S ;
FinalizerPt F ;
EnvironmentPt E ;
ChoicePointPt B, B0, R ;
Pt C, CC, CH, D, Z, X[maxX] ;
Pt ZT ; /* Reserved for ZPushTermWithEnv */
Hdl stacksBegin0, stacksEnd0 ;
Hdl stacksBegin, stacksEnd ;
Hdl trailBegin, trailEnd ;
Hdl saveTR, saveH ;
Size hostSpeed ;
int N ;

void MachineRun()
{
	if( sysTrace_flag == 3 )
		SysTraceMachineRun() ;
	else
		for(;;) {
			InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
			InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
			InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
			InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		}
	InternalError("MachineRun") ;
}

void CheckHost()
{
	if( cWord(ScratchBegin()) < 0 )
		FatalError("This machine is backwards") ;
	if( sizeof(Word) != sizeof(Pt) )
		FatalError("sizeof(Word) != sizeof(Pt)") ;
	if( cWord(&stacksBegin0) % sizeof(Word) != 0 )
		Warning("Machine registers are not aligned") ;
	if( GetTag(ScratchBegin()) != 0 )
		FatalError("Tags are clobbered by Pt values") ;
	if( cWord(ScratchBegin()) % sizeof(Word) != 0 )
		FatalError("Memory areas are not aligned") ;
}

void ZCheckHostSpeed()
{
	double tt = CpuTime() ;
	hostSpeed = 0 ;
	HSave() ;
	do {
		CharPt s = "a(P,T,O):-a(O),a(0,P),a(O,T,P,L,R),a(T,P,L,R),a(0,T)." ;
		TermAsStr(ZTermFromStr(s)) ;
		HRestore() ;
		hostSpeed++ ;
	} while( CpuTime() - tt < 0.03 ) ;
	if( hostSpeed == 0 ) hostSpeed = 1 ;
}

static Bool IsGlobalRecord(Pt t)
{
	return IsRecord(t) && IsGlobalRef(XPt(t)) ;
}

/* SHOW MACHINE STATE */

static void ShowWord(Pt t)
{
	if( IsVar(t) ) {
		if( t == nil )
			Write("NULL\n") ;
		elif( IsLocalRef(t) )
			Write("LOCALVAR %lx\n", t) ;
		elif( IsGlobalRef(t) )
			Write("GLOBALVAR %lx\n", t) ;
		elif( IsTrailRef(t) )
			Write("TRAILREF %8lx\n", t) ;
		elif( FunctorCheck(t) )
			Write("FUNCTOR %s\n", FunctorNameArity(cFunctorPt(t))) ;
		elif( UnitCheck(t) )
			Write("UNIT %s\n", UnitSignature(cUnitPt(t))) ;
		else Write("CODE? %lx\n", t) ;
	}
	elif( IsAtomic(t) )
		Write("ATOMIC %s\n", TermAsStr(t)) ;
	elif( IsExtra(t) )
		Write("EXTRA %s\n", TermAsStr(t)) ;
	elif( IsInstruction(t) ) {
		CharPt name, types ;
		if( GetInstInfo(t, &name, &types) )
			Write("INSTRUCTION %s\n", name) ;
		else Write("UNKNOWN INSTRUCTION\n") ;
	}
	elif( IsStruct(t) )
		Write("STRUCT %s\n", TermAsStr(t)) ;
	elif( IsList(t) )
		Write("LIST %s\n", TermAsStr(t)) ;
	else
		Write("?????? %lx\n", t) ;
}

static void ShowRange(CharPt n, Hdl a, Hdl z)
{
	Hdl h ;
	Write("%s:\n", n) ;
	if( a <= z )
		for( h = a ; h < z ; h++ ) {
			Write("    %8lx: ", h) ;
			ShowWord(*h) ;
		}
	else
		for( h = z - 1 ; h >= a ; a-- ) {
			Write("    %8lx: ", h) ;
			ShowWord(*h) ;
		}
	Write("------ %s stack:\n", n) ;
}



/* TRAIL */

/* The trail contain references to bound logic variables located at the
   local stack and to bound logic variables located at the global stack.
   Also contains trailed ivars, represented by pairs (saved_value, atom).
*/

void TrailIVar(AtomPt atom, Pt oldValue)
{
	PushTrail(oldValue) ;
	PushTrail(TagAtom(atom)) ; 
}

void TrailSave()	/* use only on indivisible operations */
{
	saveTR = TR ;
	HB = stacksEnd ; /* this avoid a problem with IsToTrailVar */
}

void TrailRestore()
{
	register Pt t ;
	while( TR != saveTR ) {
		t = Pop(TR) ;
		if( IsAtom(t) ) /* is this a trailed ivar? */
			IVarReversibleRestore(XAtom(t), Pop(TR)) ;
		else ResetVar(t) ;
	}
	HB = Bf(H) ; /* restore HB */
}

static void TrailTidy(void) /* not used */
{
	register Hdl h = Bf(TR) ;
	InternalError("TrailTidy") ;
	while( h < TR )
		if( IsToTrailVar(*h) ) h++ ;
		else *h = Pop(TR) ;
}

static void MachineStateRellocForTrail(Size trailOffset,
										Size finalizerOffset)
{
	register Hdl h ;

/* Copy the trail vars */
	CopyWords(trailBegin+trailOffset, trailBegin, TR-trailBegin) ;

/* Copy the cut finalizers */
	CopyWords(cHdl(F)+finalizerOffset, cHdl(F), trailEnd-cHdl(F)) ;

/* Rellocate all the trail-refs in the local stack (CPs e DFs)  */
	for( h = TopOfLocalStack() ; h < stacksEnd ; h++ )
		if( IsTrailRef(*h) ) *h += trailOffset ;

/* Rellocate the trail/finalizers registers */
	TR += trailOffset ;
	saveTR += trailOffset ;
	F = cFinalizerPt(cHdl(F) + finalizerOffset) ;
}

void TrailExpand()
{
	Size trailSize, newTrailSize ;
	Hdl newTrailBegin, newTrailEnd ;

/* Compute sizes */
	trailSize = trailEnd - trailBegin ;
	newTrailSize = 2 * trailSize ;
	MemoryGrowWarning("trail", trailSize, newTrailSize, nil) ;

/* Allocate the new trail and rellocate the machine state  */
	newTrailBegin = PrimitiveAllocate(newTrailSize) ;
	newTrailEnd = newTrailBegin + newTrailSize ;
	MachineStateRellocForTrail(newTrailBegin - trailBegin,
								newTrailEnd - trailEnd) ;
	PrimitiveRelease(trailBegin) ;
	trailBegin = newTrailBegin ;
	trailEnd = newTrailEnd ;
}

static void TrailRellocForStacks(Size globalOffset, Size localOffset)
{
	register Hdl h ;

/* Rellocate the trailed vars */
	for( h = trailBegin ; h < TR ; h++ ) {
		if( IsLocalRef(*h) ) *h += localOffset ;
		elif( IsGlobalRef(*h) ) *h += globalOffset ;
		elif( h+1 < TR && IsAtom(h[1]) ) h++ ; /* is this a trailed ivar? */
		else InternalError("TrailRellocForStacks") ;
	}

/* Rellocate the cut finalizers */
	for( h = cHdl(F) ; h < trailEnd ; h++ ) {
		if( IsLocalRef(*h) ) *h += localOffset ;
		elif( IsGlobalRef(*h) || IsGlobalRecord(*h) ) *h += globalOffset ;
	}
}

static void TrailAtomGCMark()
{	/* handle saved values of trailed ivars */
	register Hdl h ;
	for( h = trailBegin ; h < TR ; h++ )
		if( h+1 < TR && IsAtom(h[1]) ) { /* is this a trailed ivar? */
			TermAtomGCMark(*h) ;
			h++ ;
		}
}




/* X REGISTERS */

/* The X registers contain references to local vars, references to global
   vars, references to terms in the global stack.  */

static void XRegsShow()
{
	ShowRange("x registers", X, X + maxX) ;
}

static void XRegsRellocForStacks(Size globalOffset, Size localOffset)
{
	register int i ;
	dotimes(i, maxX) {
		if( IsVar(Xc(i)) ) {
			if( IsLocalRef(Xc(i)) ) Xc(i) += localOffset ;
			elif( IsGlobalRef(Xc(i)) ) Xc(i) += globalOffset ;
		}
		elif( IsAtomic(Xc(i)) || IsExtra(Xc(i)) ) ;
		elif( IsRecord(Xc(i)) ) Xc(i) += globalOffset ;
		else InternalError("XRegsRellocForStacks") ;
	}
}



/* OTHER REGISTERS */

static void OtherRegsRellocForStacks(Size globalOffset, Size localOffset)
{
	if( IsGlobalRecord(Z) ) Z += globalOffset ; /* must be first */
	if( IsGlobalRecord(ZT) ) ZT += globalOffset ;

	if( IsRecord(C) ) C += globalOffset ;
	if( IsRecord(CC) ) CC += globalOffset ;
	if( IsRecord(CH) ) CH += globalOffset ;

	E = cVoidPt(cPt(E) + localOffset) ;
	B = cVoidPt(cPt(B) + localOffset) ;
	B0 = cVoidPt(cPt(B0) + localOffset) ;
	if( R != nil )
		R = cVoidPt(cPt(R) + localOffset) ;

	H += globalOffset ;
	saveH += globalOffset ;
	HB += globalOffset ;
	S += globalOffset ;
}



/* LOCAL STACK */

/* The local stack contains:
     Environments
        local vars (Y regs),
            references to local vars (Y),
            references to global vars (Y),
            references to terms ??? (Y, CC),
            temporarily undefined values
        reference to Environment (E),
        reference to executable code (CP),
        reference to ChoicePoint (B0)

     ChoicePoints
        saving X regs and "state cells" of non-deterministics - (A regs),
            references to local vars (Y),
            references to global vars (Y),
            references to terms ??? (Y),
        reference to Environment (E),
        references to executable code (CP, P),
        references to ChoicePoint (B, B0),
        references to terms (C, CC, CH),
        reference to the trail (TR),
        reference to the global stack (H)

     DebugFrames.
        references to executable code
           (exitInst, redoInst, retryInst, callCP, redoP),
        reference to predicate (pred),
        reference to clause (currClause),
        references to terms ??? (callC, callCH, frameN),
        reference to the trail (redoTR),
        reference to the DebugFrame (father),
		int DebugEvent event ;
		int Char type ;
		Size lineN ; 
*/

static void LocalStackShow()
{
	ShowRange("local stack", stacksEnd, TopOfLocalStack()) ;
}

static void LocalStackRellocForStacks(Size globalOffset, Size localOffset)
{
	register Hdl a, z, lTop = TopOfLocalStack() ;

	if( localOffset >= 0 )
		for( a = stacksEnd-1, z = a + localOffset ; a >= lTop ; a--, z-- )
		{
			if( IsVar(*a) ) {
				if( IsLocalRef(*a) ) *z = *a + localOffset ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) || IsExtra(*a) || IsInstruction(*a) ) *z = *a ;
			elif( IsRecord(*a) ) *z = *a + globalOffset ;
			else InternalError("LocalStackRellocForStacks (>=0)") ;
		}
	else
		for( a = lTop, z = a + localOffset ; a < stacksEnd ; a++, z++ )
		{
			if( IsVar(*a) ) {
				if( IsLocalRef(*a) ) *z = *a + localOffset ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) || IsExtra(*a) || IsInstruction(*a) ) *z = *a ;
			elif( IsRecord(*a) ) *z = *a + globalOffset ;
			else InternalError("LocalStackRellocForStacks (<0)") ;
		}
}



/* GLOBAL STACK */

/* The local stack contains:
     Global vars
     Functors (of structs)
     Unit references
     Terms in the global stack
     Metacut pointer to choicepoint in the local stack
     Terms in the code area ??? not anymore
*/

void HSave()
{
	saveH = H ;
}

void HRestore()
{
	H = saveH ;
}

Size HGrown()
{
	return H - saveH ;
}

static void GlobalStackShow()
{			
	ShowRange("global stack", stacksBegin, H) ;
}

static void GlobalStackRellocForStacks(Size globalOffset, Size localOffset) {
	register Hdl a, z ;

	if( globalOffset >= 0 )
		for( a = H - 1, z = a + globalOffset ; a >= stacksBegin ; a--, z-- )
		{
			if( IsVar(*a) ) {
				if( *a == nil )
					InternalError("GlobalStackRellocForStacks (Null at GS)") ;
				elif( IsLocalRef(*a) )
					/* Sole instance of global stack pointing to local stack */
					if( a[-1] == cPt(metaCutFunctor) )
						*z = *a + localOffset ;
					else
					 InternalError("GlobalStackRellocForStacks (Local var at GS)") ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) || IsExtra(*a) ) *z = *a ;
			elif( IsRecord(*a) ) *z = *a + globalOffset ;
			else InternalError("GlobalStackRellocForStacks (>=0)") ;
		}
	else
		for( a = stacksBegin, z = a + globalOffset ; a < H ; a++, z++ )
		{
			if( IsVar(*a) ) {
				if( *a == nil )
					InternalError("GlobalStackRellocForStacks (Null at GS 2)") ;
				elif( IsLocalRef(*a) )
					/* Sole instance of global stack pointing to local stack */
					if( a[-1] == cPt(metaCutFunctor) )
						*z = *a + localOffset ;
					else
					 InternalError("GlobalStackRellocForStacks (Local var at GS 2)") ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) || IsExtra(*a) ) *z = *a ;
			elif( IsRecord(*a) ) *z = *a + globalOffset ;
			else InternalError("GlobalStackRellocForStacks (<0 2)") ;
		}
}

Bool IsUnitInUse(UnitPt u)
{
	register Hdl a ;

	for( a = stacksBegin ; a < H ; a++ )
		if( Eq(*a, u) )
			return true ;
	return false ;
}



/* BOTH STACKS */

static void MachineStateRellocForStacks(Size globalOffset, Size localOffset)
{
	LocalStackRellocForStacks(globalOffset, localOffset) ;
	GlobalStackRellocForStacks(globalOffset, localOffset) ;
	TermReadVarTableRellocForStacks(globalOffset, localOffset) ;
	DebuggerRellocForStacks(globalOffset, localOffset) ;
	XRegsRellocForStacks(globalOffset, localOffset) ;
	TrailRellocForStacks(globalOffset, localOffset) ;
	OtherRegsRellocForStacks(globalOffset, localOffset) ;
}

Bool ZStacksExpansion(Size ensureThisExtraSpace, CharPt where)
{
	Size stacksSize, newStacksSize ;
	Hdl newStacksBegin, newStacksEnd ;

	if( stacksBegin != stacksBegin0 ) { /* Only for testRellocation */
		/* Dot('*') ; */
		stacksSize = stacksEnd - stacksBegin ;
		newStacksSize = stacksEnd0 - stacksBegin0 ;
		MachineStateRellocForStacks(stacksBegin0 - stacksBegin,
									stacksEnd0 - stacksEnd) ;
		stacksBegin = stacksBegin0 ; /* Restore whole stacks */
		stacksEnd = stacksEnd0 ;
		if( newStacksSize - stacksSize >= ensureThisExtraSpace )
			return true ;
		/* Dot('+') ; */
	}

	for( stacksSize = stacksEnd - stacksBegin,
		 newStacksSize = stacksSize * 2 ;
				newStacksSize - stacksSize < ensureThisExtraSpace ;
						newStacksSize *= 2) ;
	MemoryGrowWarning("stacks", stacksSize, newStacksSize, where) ;

	newStacksBegin = PrimitiveAllocateAndClear(newStacksSize) ;
	newStacksEnd = newStacksBegin + newStacksSize ;
	MachineStateRellocForStacks(newStacksBegin - stacksBegin,
								newStacksEnd - stacksEnd) ;
	PrimitiveRelease(stacksBegin0) ;
	stacksBegin = stacksBegin0 = newStacksBegin ;
	stacksEnd = stacksEnd0 = newStacksEnd ;
	return true ;
}

Bool ZTestStacksExpansion(CharPt where)
{
	/* Dot('.') ; */
	/* Mesg(where) ; */
	MachineStateRellocForStacks(1, -1) ;
	stacksBegin += 1 ;
	stacksEnd -= 1 ;
	return true ;
}

/* GARBAGE COLLECTION */

static void MachineStateAtomGCMark()
{
	TrailAtomGCMark() ;								/* trailed ivars */
	AtomGCMarkRange(stacksBegin, H) ;				/* global stack */
	AtomGCMarkRange(TopOfLocalStack(), stacksEnd) ;	/* local stack */
	AtomGCMarkRange(X, X + maxX) ;					/* X regs */
}

/*
void MachineStateUnitGCMark()
{
    register ChoicePointPt cp ;
	for( cp = B ; Lt(cp, stacksEnd) ; cp = cp->B )
		if( cp->C != tNilAtom )
			UnitGCMark(cUnitPt(XPt(cp->C)[-1]))
}
*/

/* MACHINE STATE */

void TestRelocationUpdateFlags(int newValue)
{
	if( (testRelocation_flag = newValue) )
		Attention() = true ;
}

void TestGCUpdateFlags(int newValue)
{
	if( (testGCollection_flag = newValue) )
		Attention() = true ;
}

void MachineInit()
{
	InstallAtomGCHandler(MachineStateAtomGCMark) ;
}
