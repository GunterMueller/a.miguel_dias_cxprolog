/*
 *   This file is part of the CxProlog system

 *   Machine.c
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

Hdl P, CP, H, HB, TR, S ;
EnvironmentPt E ;
ChoicePointPt B, B0, R ;
MixReg Q, Z ;
FinalizerPt F ;
Pt C, CH, D, X[maxX] ;
Pt GlobalClock ;
Pt ZT ; /* Reserved for ZPushTermWithEnv */
Hdl stacksBegin0, stacksEnd0 ;
Hdl stacksBegin, stacksEnd ;
Hdl trailBegin, trailEnd ;
Hdl saveTR, saveH ;
static Size hostSpeed ;
int N ;
Bool running = false ;

void MachineRun()
{
	running = true ;
	SysTraceMachineRun() ;
#if USE_THREADED_CODE
	JumpNext() ;
#else
	for(;;) {
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
	}
#endif
	InternalError("MachineRun") ;
}

void CheckHost()
{
	if( sizeof(Word) != sizeof(Pt) )
		FatalError("sizeof(Word) != sizeof(Pt)") ;
	if( cWord(&stacksBegin0) % sizeof(Word) != 0 )
		Warning("Machine registers are not aligned") ;
	if( cWord(ScratchStart()) % sizeof(Word) != 0 )
		FatalError("Memory areas are not aligned") ;
}

/* ZCheckHostSpeed: Roughly evaluates the speed of the host machine.
   The "speed" is the number of times the machine manages to parse and build
   some term from a string and then convert the term back to a string. The
   amount of time given to this is 0.04 sec.
*/

void ZCheckHostSpeed()
{
	double tt = CpuTime() ;
	hostSpeed = 0 ;
	HSave() ;
	do {
		CharPt s = "a(P,T,O):-a(0,P),a(O,T,P,L,R),a(T,P,L,R),a(0,T)." ;
		TermAsStr(ZTermFromStr(s)) ;
		HRestore() ;
		hostSpeed++ ;
	} while( CpuTime() - tt < 0.04 ) ;
	if( hostSpeed == 0 ) hostSpeed = 1 ;
}


/* RelocateHandler Table */

#define maxRelocateStacksHandlers  20

static RelocateStacksProc RelocateStacksHdl[maxRelocateStacksHandlers] ;
static int nRelocateStacksHdl = 0 ;

void InstallRelocateStacksHandler(RelocateStacksProc p)
{
	if( nRelocateStacksHdl == maxRelocateStacksHandlers )
		InternalError("Too many RELOCATE STACK handlers: increase constant") ;
	RelocateStacksHdl[nRelocateStacksHdl++] = p ;
}

static void RelocateStacks(Size globalOffset, Size localOffset)
{
	int i ;
	dotimesrev(i, nRelocateStacksHdl) /* Must be in reverse order */
		RelocateStacksHdl[i](globalOffset, localOffset) ;		
}


/* SHOW MACHINE STATE */

static void ShowWord(Hdl h, Bool terms)
{
	Pt t = *h ;
	Write("  %08lx: ", h) ;
	Write("(%08lx) ", t) ;
	if( IsEmpty(t) )
		Write("EMPTY %lx\n", t) ;
	elif( t == nil )
		Write("NULL\n") ;
	elif( IsVar(t) ) {
		if( IsLocalRef(t) )
			Write("LOCALVAR %lx\n", t) ;
		elif( IsGlobalRef(t) )
			Write("GLOBALVAR %lx\n", t) ;
		elif( IsTrailRef(t) )
			Write("TRAILREF %08lx\n", t) ;
		elif( FunctorCheck(t) )
			Write("FUNCTOR %s\n", FunctorNameArity(cFunctorPt(t))) ;
		elif( UnitCheck(t) )
			Write("UNIT %s\n", UnitSignature(cUnitPt(t))) ;
		else
			Write("CODE %lx\n", t) ;
	}
	elif( IsAtomicStrict(t) )
		Write("ATOMIC %s\n", TermAsStr(t)) ;
	elif( IsExtra(t) )
		Write("EXTRA %s\n", TermAsStr(t)) ;
	elif( IsStruct(t) ) {
		if( terms )
			Write("STRUCT %s  <%lx>\n", TermAsStr(t), XPt(t)) ;
		else
			Write("STRUCT %lx\n", XPt(t)) ;	
	}
	elif( IsList(t) ) {
		if( terms )
			Write("LIST %s  <%lx>\n", TermAsStr(t), XPt(t)) ;
		else
			Write("LIST %lx\n", XPt(t)) ;
	}
	else Write("????? %lx\n", t) ;
}

void ShowMachineRange(CharPt n, Hdl a, Hdl z)
{
	if( a <= z ) {
		Write("%s [%lx, %lx]:\n", n, a, z) ;
		for( ; a < z ; a++ )
			ShowWord(a, false) ;
	}
	else {
		Write("%s [%lx, %lx]::\n", n, z, a) ;
		for( z-- ; z >= a ; z-- )
			ShowWord(z, false) ;
	}
	Write("%s ----------------------\n", n) ;
}

static Bool MachineShow(CharPt s)
{
	if( StrEqual(s, "local") )
		ShowMachineRange(s, TopOfLocalStack(), stacksEnd) ;
	elif( StrEqual(s, "global") )
		ShowMachineRange(s, stacksBegin, H) ;
	elif( StrEqual(s, "trail") )
		ShowMachineRange(s, trailBegin, TR) ;
	elif( StrEqual(s, "finalizers") )
		ShowMachineRange(s, cHdl(F), trailEnd) ;
	elif( StrEqual(s, "x") )
		ShowMachineRange(s, X, FindEmpty(X) + 1) ;
	else return false ;
	return true ;
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

void TrailSave()		/* Use only on indivisible operations */
{
	saveTR = TR ;
	HB = stacksEnd ;	/* Forces trailing of all vars */
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

#if unused
static void TrailTidy(void) /* not used */
{
	register Hdl h = Bf(TR) ;
	InternalError("TrailTidy") ;
	while( h < TR )
		if( IsToTrailVar(*h) ) h++ ;
		else *h = Pop(TR) ;
}
#endif
		
static void RelocateAllForTrailExpand(Size trailOffset, Size finalizerOffset)
{
	register Hdl h ;

/* Copy the trail vars */
	CopyWords(trailBegin+trailOffset, trailBegin, TR-trailBegin) ;

/* Copy the cut finalizers */
	CopyWords(cHdl(F)+finalizerOffset, cHdl(F), trailEnd-cHdl(F)) ;

/* Relocate all the trail-refs in the local stack (CPs e DFs)  */
	for( h = TopOfLocalStack() ; h < stacksEnd ; h++ )
		if( IsTrailRef(*h) ) *h += trailOffset ;

/* Relocate the trail/finalizers registers */
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

/* Allocate the new trail and relocate the machine state  */
	newTrailBegin = PrimitiveAllocateEmpty(newTrailSize, &newTrailEnd) ;
	RelocateAllForTrailExpand(newTrailBegin - trailBegin, newTrailEnd - trailEnd) ;
	PrimitiveRelease(trailBegin) ;
	trailBegin = newTrailBegin ;
	trailEnd = newTrailEnd ;
}

static void RelocateTrail(Size globalOffset, Size localOffset)
{
	register Hdl h ;

/* Relocate the trailed vars */
	for( h = trailBegin ; h < TR ; h++ ) {
		if( IsLocalRef(*h) ) *h += localOffset ;
		elif( IsGlobalRef(*h) ) *h += globalOffset ;
		elif( h+1 < TR && IsAtom(h[1]) ) h++ ; /* is this a trailed ivar? */
		else InternalError("RelocateTrail") ;
	}

/* Relocate the cut finalizers */
	for( h = cHdl(F) ; h < trailEnd ; h++ ) {
		if( IsLocalRef(*h) ) *h += localOffset ;
		elif( IsGlobalRef(*h) || IsGlobalCompound(*h) ) *h += globalOffset ;
	}
}

static void TrailBasicGCMark()
{	/* handle saved values of trailed ivars */
	register Hdl h ;
	for( h = TR-1 ; h > trailBegin ; h-- )
		if( IsAtom(*h) ) { /* is this a trailed ivar? */
			TermBasicGCMark(h[-1]) ;
			h-- ;
		}
}



/* X REGISTERS */

/* The X registers contain references to local vars, references to global
   vars, references to terms in the global stack.  */

static void RelocateXRegs(Size globalOffset, Size localOffset)
{
	register int i ;
	dotimes(i, maxX) {
		if( IsVar(Xc(i)) ) {
			if( IsLocalRef(Xc(i)) ) Xc(i) += localOffset ;
			elif( IsGlobalRef(Xc(i)) ) Xc(i) += globalOffset ;
			else /* nothing */ ;
		}
		elif( IsAtomic(Xc(i)) ) ;
		elif( IsCompound(Xc(i)) ) Xc(i) += globalOffset ;
		else InternalError("RelocateXRegs") ;
	}
}



/* OTHER REGISTERS */

static void RelocateOtherRegs(Size globalOffset, Size localOffset)
{
	if( IsGlobalCompound(Z.t) ) Z.t += globalOffset ; /* must be first */
	if( IsGlobalCompound(ZT) ) ZT += globalOffset ;

	if( IsCompound(C) ) C += globalOffset ;
	if( IsCompound(CH) ) CH += globalOffset ;

	E = cVoidPt(cPt(E) + localOffset) ;
	B = cVoidPt(cPt(B) + localOffset) ;
	B0 = cVoidPt(cPt(B0) + localOffset) ;
	R = cVoidPt(cPt(R) + localOffset) ;

	H += globalOffset ;
	saveH += globalOffset ;
	HB += globalOffset ;
	if( S != nil ) /* read mode */
		S += globalOffset ;
}



/* LOCAL STACK */

/* The local stack contains:
     Environments
        local vars (Y regs),
            references to local vars (Y),
            references to global vars (Y),
            references to terms ??? (Y),
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
        references to terms (C, CH),
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

static void RelocateLocalStack(Size globalOffset, Size localOffset)
{
	register Hdl a, z, lTop = TopOfLocalStack() ;

	if( localOffset >= 0 )
		for( a = stacksEnd - 1, z = a + localOffset ; a >= lTop ; a--, z-- )
		{
			if( IsEmpty(*a) )
				*z = *a ;
			elif( IsVar(*a) ) {
				if( IsLocalRef(*a) ) *z = *a + localOffset ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) || IsInstruction(*a) ) *z = *a ;
			elif( IsCompound(*a) ) *z = *a + globalOffset ;
			else InternalError("RelocateLocalStack (>=0)") ;
		}
	else
		for( a = lTop, z = a + localOffset ; a < stacksEnd ; a++, z++ )
		{
			if( IsEmpty(*a) )
				*z = *a ;
			elif( IsVar(*a) ) {
				if( IsLocalRef(*a) ) *z = *a + localOffset ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) || IsInstruction(*a) ) *z = *a ;
			elif( IsCompound(*a) ) *z = *a + globalOffset ;
			else InternalError("RelocateLocalStack (<0)") ;
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

static void RelocateGlobalStack(Size globalOffset, Size localOffset) {
	register Hdl a, z ;

	if( globalOffset >= 0 )
		for( a = H - 1, z = a + globalOffset ; a >= stacksBegin ; a--, z-- )
		{
			if( IsEmpty(*a) )
				InternalError("RelocateGlobalStack (Empty at GS)") ;
			elif( IsVar(*a) ) {
				if( IsLocalRef(*a) )
					/* Sole instance of global stack pointing to local stack */
					if( a[-1] == cPt(metaCutFunctor) )
						*z = *a + localOffset ;
					else
					 InternalError("RelocateGlobalStack (Local var at GS)") ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) ) *z = *a ;
			elif( IsCompound(*a) ) *z = *a + globalOffset ;
			else InternalError("RelocateGlobalStack (>=0)") ;
		}
	else
		for( a = stacksBegin, z = a + globalOffset ; a < H ; a++, z++ )
		{
			if( IsEmpty(*a) )
				InternalError("RelocateGlobalStack (Empty at GS 2)") ;
			elif( IsVar(*a) ) {
				if( IsLocalRef(*a) )
					/* Sole instance of global stack pointing to local stack */
					if( a[-1] == cPt(metaCutFunctor) )
						*z = *a + localOffset ;
					else
					 InternalError("RelocateGlobalStack (Local var at GS 2)") ;
				elif( IsGlobalRef(*a) ) *z = *a + globalOffset ;
				else *z = *a ;
			}
			elif( IsAtomic(*a) ) *z = *a ;
			elif( IsCompound(*a) ) *z = *a + globalOffset ;
			else InternalError("RelocateGlobalStack (<0 2)") ;
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

Bool ZStacksExpansion(Size ensureThisExtraSpace, CharPt where)
{
	Size stacksSize, newStacksSize ;
	Hdl newStacksBegin, newStacksEnd ;

	if( stacksBegin != stacksBegin0 ) { /* Only for testRelocation */
		/* Dot("*") ; */
		stacksSize = stacksEnd - stacksBegin ;
		newStacksSize = stacksEnd0 - stacksBegin0 ;
		RelocateStacks(stacksBegin0 - stacksBegin, stacksEnd0 - stacksEnd) ;
		stacksBegin = stacksBegin0 ; /* Restore whole stacks */
		stacksEnd = stacksEnd0 ;
		if( newStacksSize - stacksSize >= ensureThisExtraSpace )
			return true ;
		/* Dot("+") ; */
	}

	for( stacksSize = stacksEnd - stacksBegin,
		 newStacksSize = stacksSize * 2 ;
				newStacksSize - stacksSize < ensureThisExtraSpace ;
						newStacksSize *= 2) ;
	MemoryGrowWarning("stacks", stacksSize, newStacksSize, where) ;

	newStacksBegin = PrimitiveAllocateEmpty(newStacksSize, &newStacksEnd) ;
	RelocateStacks(newStacksBegin - stacksBegin, newStacksEnd - stacksEnd) ;
	PrimitiveRelease(stacksBegin0) ;
	stacksBegin = stacksBegin0 = newStacksBegin ;
	stacksEnd = stacksEnd0 = newStacksEnd ;
	return true ;
}

Bool ZTestStacksExpansion(CharPt where)
{
#if 0
	Dot(".") ;
#endif
#if 0
	Mesg(where) ;
#endif
	RelocateStacks(1, -1) ;
	stacksBegin += 1 ;
	stacksEnd += -1 ;
	return true ;
}

/* GARBAGE COLLECTION */

static void MachineStateBasicGCMark()
{
	TrailBasicGCMark() ;								/* trailed ivars */
	BasicGCMarkRange(stacksBegin, H) ;					/* global stack */
	BasicGCMarkRange(TopOfLocalStack(), stacksEnd) ;	/* local stack */
	BasicGCMarkRange(X, X + maxX) ;						/* X regs */
}

/*
void MachineStateUnitGCMark()
{
    register ChoicePointPt cp ;
	for( cp = B ; !EndOfLocalChain(cp) ; cp = cp->B )
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
	EmptyRangeN(X, maxX) ;
	InstallBasicGCHandler("MACHINE", MachineStateBasicGCMark) ;

/* This ordering is mandatory. Don't change it. */
	InstallRelocateStacksHandler(RelocateOtherRegs) ;
	InstallRelocateStacksHandler(RelocateTrail) ;
	InstallRelocateStacksHandler(RelocateXRegs) ;
	InstallRelocateStacksHandler(RelocateGlobalStack) ;
	InstallRelocateStacksHandler(RelocateLocalStack) ;
}


/* CXPROLOG C'BUILTINS */

static void PHostSpeed()
{
	Size speed = hostSpeed ;
	if( testRelocation_flag || testGCollection_flag ) speed /= 200 ;
	MustBe( Unify(X0, MakeInt(speed)) ) ;
}

static void PMShow()
{
	if( !MachineShow(XTestAtomName(X0)) )
		Error("Unknown option") ;
	JumpNext() ;
}

void MachineInit2()
{
	InstallCBuiltinPred("host_speed", 1, PHostSpeed) ;
	InstallCBuiltinPred("mshow", 1, PMShow) ;
}
