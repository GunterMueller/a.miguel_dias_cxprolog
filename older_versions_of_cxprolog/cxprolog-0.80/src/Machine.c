/*
 *   This file is part of the CxProlog system

 *   Machine.c
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* ARGUMENT ACCESS */

#define GetPt()			(*P++)
#define LookPt()		(*P)
#define LookHdl()		cHdl(*P)
#define GetHdl()		cHdl(*P++)
#define GetClause()		cClausePt(*P++)
#define GetWord()		cWord(*P++)
#define LookPred()		cPredicatePt(*P)
#define LookFunctor()	cFunctorPt(*P)
#define SkipInst()		(P++)


/* MACHINE REGISTERS */

Hdl P, CP, H, HB, TR, S ;
EnvironmentPt E ;
ChoicePointPt B, B0 ;
Pt C, CC, CH, D, T, Z, ZT, X[maxX] ;
Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
ChoicePointPt saveB ;
Hdl saveTR ;
Size hostSpeed ;
int N ;

static EnvironmentPt saveE ;
static PredicatePt PR ;
static Bool writeMode ;

#if 1
static short filler ;
#endif



/* GLOBAL STACK CONTROL */

#define PushHVar()			( ResetVar(H), cPt(H++) )
#define PushH(v)			Push(H, v)
#define PopH()				Pop(H)
#define GrowH(n)			Grow(H, n)



/* MISC FUNCTIONS */

static PredicatePt SearchContextBelow(FunctorPt f)
{
	register PredicatePt pr ;
	Pt c = C ;
	if( C != tNilAtom )
		for( C = Drf(XListTail(C)) ; C != tNilAtom ; C = Drf(XListTail(C)) )
			if( (pr = FindPredicate(f)) != nil
			     && (PredIsVisible(pr) || forceVisibility_flag) )
				return pr ;
	if( undefWarnings_flag )
		Warning("Predicate '%s' is not visible in context %s",
						FunctorNameArity(f), TermAsStr(c)) ;
	return nil ;
}

static FunctorPt PrepareCall(void)
{
	register Pt t ;
	VarValue2(t, X0) ;
	if( IsStruct(t) ) {
		register int s = XStructArity(t) ;
		while( s-- )
			Xc(s) = Drf(XStructArg(t,s)) ;
		return XStructFunctor(t) ;
	}
	elif( IsAtom(t) ) {
		return LookupFunctor(XAtom(t), 0) ;
	}
	elif( IsList(t) ) {
		X0 = Drf(XListHead(t)) ;
		X1 = Drf(XListTail(t)) ;
		return listFunctor ;
	}
	elif( IsNumber(t) ) 
		Error("NUMBER in call/1: %s", XNumberAsStr(t)) ;
	elif( IsVar(t) )
		Error("Unbound VARIABLE in call/1") ;
	elif( IsExtra(t) ) 
		Error("EXTRA in call/1: '%s'", XExtraAsStr(t)) ;
	else Default("PrepareCall") ;
	return nil ;
}

#define RunType 1

void MachineRun()
{
#if RunType == 0
	for(;;)
		 InstRun() ;
#else
#if RunType == 1
	for(;;) {
		ZEnsureFreeSpaceOnStacks(0) ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
		InstRun() ; InstRun() ; InstRun() ; InstRun() ; InstRun() ;
	}
#else
	for(;;) {
		DisassembleInst(DisassembleInst(P)) ;
		InstRun() ;
	}
#endif
#endif
	InternalError("RunMachine") ;
}

void CheckHost()
{
	if( cWord(BufferBegin()) < 0 )
		FatalError("This machine is backwards") ;
	if( sizeof(Word) != sizeof(Pt) )
		FatalError("sizeof(Word) != sizeof(Pt)") ;
	if( cWord(&stacksBegin) % sizeof(Word) != 0 )
		Warning("Machine registers are not aligned") ;
	if( GetTag(BufferBegin()) != 0 )
		FatalError("Tags are clobbered by Pt values") ;
	if( cWord(BufferBegin()) % sizeof(Word) != 0 )
		FatalError("Memory areas are not aligned") ;
}

void ZCheckHostSpeed()
{
	double tt = CurrTime() ;
	hostSpeed = 0 ;
	HSave() ;
	do {
		CharPt s = "a(P,T,O):-a(O),a(0,P),a(O,T,P,L,R),a(T,P,L,R),a(0,T)." ;
		TermAsStr(ZReadTermFromStr(s)) ;
		HRestore() ;
		hostSpeed++ ;
	} while( CurrTime() - tt < 0.03 ) ;
	if( hostSpeed == 0 ) hostSpeed = 1 ;
}



/* TRAIL */

/* The trail contain references to bound logic variables located at the
   local stack and to bound logic variables located at the global stack.
*/

void TrailSave()
{
	saveTR = TR ;
}

void TrailRestore()
{
	register Pt t ;
	while( TR != saveTR ) {
		t = Pop(TR) ;
		ResetVar(t) ;
	}
}

void TrailTidy()
{
	register Hdl h = Bf(TR) ;
	while( h < TR )
		if( IsToTrailVar(*h) ) h++ ;
		else *h = Pop(TR) ;
}

void TrailExpand()
{
	Size trailSize = trailEnd - trailBegin ;
	Hdl newTrailBegin, newTrailEnd ;
	register ChoicePointPt b ;
#if 0
	Error("Trail overflow") ;
#endif
	MemoryGrowWarning("trail", trailSize, trailSize * 2) ;
	newTrailBegin = PrimitiveRellocate(trailBegin,
						trailSize,
						trailSize * 2) ;
	newTrailEnd = newTrailBegin + trailSize * 2 ;
	TR += newTrailBegin - trailBegin ;
	saveTR += newTrailBegin - trailBegin ;
	for( b = B ; Lt(b, stacksEnd) ; b = b->B )
		b->TR += newTrailBegin - trailBegin ;
	trailBegin = newTrailBegin ;
	trailEnd = newTrailEnd ;
}

static void TrailRelocate(Hdl newStacksBegin, Hdl newStacksEnd)
{
	register Hdl h ;
	for( h = trailBegin ; h < TR ; h++ ) {
		if( IsLocalVarStrong(*h) ) *h += newStacksEnd - stacksEnd ;
		elif( IsGlobalVarStrong(*h) ) *h += newStacksBegin - stacksBegin ;
		else InternalError("TrailRelocate") ;
	}
}



/* X REGISTERS */

/* The X registers contain references to local vars, references to global
   vars, references to terms in the global stack.  */

void XRegsShow()
{
	register int i ;
	Write("X registers:\n") ;
	dotimes(i, maxX) {
		if( Xc(i) == nil ) return ;
		Write("    X(%d) = ", i) ;
		if( IsVar(Xc(i)) ) {
			if( IsLocalVarStrong(Xc(i)) ) Write("LOCAL %lx\n", Xc(i)) ;
			elif( IsGlobalVarStrong(Xc(i)) ) Write("GLOBAL %lx\n", Xc(i)) ;
			else Write("????? %lx\n", Xc(i)) ;
		}
		elif( IsAtomic(Xc(i)) || IsExtra(Xc(i)) )
			Write("ATOMC %s\n", TermAsStr(Xc(i))) ;
		elif( Ge(XPt(Xc(i)),H) )
			Write("TERM? %lx\n", Xc(i)) ;
		else Write("TERM  %s\n", TermAsStr(Xc(i))) ;
	}
}

static void XRelocate(Hdl newStacksBegin, Hdl newStacksEnd)
{
	register int i ;
	dotimes(i, maxX) {
		if( Xc(i) == nil ) return ;
		if( IsVar(Xc(i)) ) {
			if( IsLocalVarStrong(Xc(i)) )
				Xc(i) += newStacksEnd - stacksEnd ;
			elif( IsGlobalVarStrong(Xc(i)) )
				Xc(i) += newStacksBegin - stacksBegin ;
			else InternalError("XRelocate") ;
		}
		elif( IsAtomic(Xc(i)) || IsExtra(Xc(i)) ) ;
		elif( IsRecord(Xc(i)) )
			Xc(i) += newStacksBegin - stacksBegin ;
		else InternalError("XRelocate") ;
	}
}



/* LOCAL STACK */

/* The local stack contains:
     Environments
        local vars (Y regs),
            references to local vars (Y),
            references to global vars (Y),
            references to terms (Y, CC),
        reference to Environment (E),
        reference to executable code (CP),
        reference to ChoicePoint (B0)

     ChoicePoints
        saving X regs and "state cells" of non-deterministics - (A regs),
            references to local vars (Y),
            references to global vars (Y),
            references to terms (Y),
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
        references to terms (callC, callCH, frameN),
        reference to the trail (redoTR),
        reference to the DebugFrame (father),
		int DebugEvent event ;
		int char type ;
		Size lineN ; 
*/

void LocalStackShow()
{
	register Hdl a, aLim = cHdl(FTopOfLocalStack()) ;
	Write("Local stack:\n") ;
	for( a = stacksEnd-1 ; a >= aLim ; a-- ) {
		Write("    %lx = ", a) ;
		if( IsVar(*a) ) {
			if( IsLocalVarStrong(*a) ) Write("LOCAL %lx\n", *a) ;
			elif( IsGlobalVarStrong(*a) ) Write("GLOBAL %lx\n", *a) ;
			elif( InRange(*a, cPt(trailBegin), cPt(trailEnd-1)) )
										Write("TRAIL %lx\n", *a) ;
			else Write("CODE  %lx\n", *a) ;
		}
		elif( IsAtomic(*a) || IsExtra(*a) )
			Write("ATOMC %s\n", TermAsStr(*a)) ;
		else Write("TERM  %s\n", TermAsStr(*a)) ;
	}
}

static void LocalStackRelocate(Hdl newStacksBegin, Hdl newStacksEnd)
{
	register Hdl z, a, aLim = cHdl(FTopOfLocalStack()) ;
	for( z = newStacksEnd-1, a = stacksEnd-1 ; a >= aLim ; z--, a-- ) {
		if( IsVar(*a) ) {
			if( IsLocalVarStrong(*a) )
				*z = *a + (newStacksEnd - stacksEnd) ;
			elif( IsGlobalVarStrong(*a) )
				*z = *a + (newStacksBegin - stacksBegin) ;
			else *z = *a ;
		}
		elif( IsAtomic(*a) || IsExtra(*a) ) *z = *a ;
		else *z = *a + (newStacksBegin - stacksBegin) ;
	}
}

static void LocalStackAtomGCMark()
{
	register Hdl a, aLim = cHdl(TopOfLocalStack()) ;
	for( a = stacksEnd-1 ; a >= aLim ; a-- )
		if( IsAtom(*a) ) 
			AtomGCMark(XAtom(*a)) ;
}



/* GLOBAL STACK */

/* The global stack contains global vars, functors, unit references, terms. */

static Hdl saveH ;

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

void GlobalStackShow()
{
	register Hdl a ;
	Write("Global stack:\n") ;
	for( a = stacksBegin ; a < H ; a++ ) {
		Write("    %lx = ", a) ;
		if( IsVar(*a) ) {
			if( IsGlobalVarStrong(*a) )
				Write("GLOBAL %lx\n", *a) ;
			elif( FunctorCheck(*a) )
					Write("FUNCT %s\n", FunctorNameArity(cFunctorPt(*a))) ;
			elif( UnitCheck(*a) )
				Write("UNIT  %s\n", UnitSignature(cUnitPt(*a))) ;
			else Write("????? %lx\n", *a) ;
		}
		elif( IsAtomic(*a) || IsExtra(*a) )
			Write("ATOMC %s\n", TermAsStr(*a)) ;
		else Write("TERM [%s] %lx\n", TermTypeStr(*a), ClearTag(*a)) ;
	}
}

static void GlobalStackRelocate(Hdl newStacksBegin) {
	register Hdl z, a ;
	for( z = newStacksBegin, a = stacksBegin ; a < H ; z++, a++ ) {
		if( IsVar(*a) ) {
			if( *a == nil )
				Mesg("NULL AT GLOBAL STACK >>> %ld   (dist = %ld)", *a, a-stacksBegin) ;
			if( IsGlobalVarStrong(*a) )
				*z = *a + (newStacksBegin - stacksBegin) ;
			elif( IsLocalVarStrong(*a) )
				InternalError("GlobalStackRelocate") ;
			else *z = *a ;
		}
		elif( IsAtomic(*a) || IsExtra(*a) ) *z = *a ;
		else *z = *a + (newStacksBegin - stacksBegin) ;
	}
}

static void GlobalStackAtomGCMark()
{
	register Hdl a ;
	for( a = stacksBegin ;  a < H ; a++ )
		if( IsAtom(*a) )
			AtomGCMark(XAtom(*a)) ;
}

static IsGlobalRecord(Pt t)
{
	return IsRecord(t) && XHdl(t) >= stacksBegin && XHdl(t) < stacksEnd ;
}

Bool ZControlStacksExpand(Size ensureExtraSpace)
{
	Size stacksSize = stacksEnd - stacksBegin, newStacksSize ;
	Hdl newStacksBegin, newStacksEnd ;
#if 0
	Error("Local stack overflow") ;
#endif
	for( newStacksSize = stacksSize * 2 ;
			newStacksSize - stacksSize < ensureExtraSpace ;
				newStacksSize *= 2 ) ;
	MemoryGrowWarning("stacks", stacksSize, newStacksSize) ;
	newStacksBegin = PrimitiveAllocate(newStacksSize) ;
	newStacksEnd = newStacksBegin + newStacksSize ;

	LocalStackRelocate(newStacksBegin, newStacksEnd) ;
	GlobalStackRelocate(newStacksBegin) ;
	XRelocate(newStacksBegin, newStacksEnd) ;
	TrailRelocate(newStacksBegin, newStacksEnd) ;

	E = cVoidPt(cPt(E) + (newStacksEnd - stacksEnd)) ;
	B = cVoidPt(cPt(B) + (newStacksEnd - stacksEnd)) ;
	B0 = cVoidPt(cPt(B0) + (newStacksEnd - stacksEnd)) ;
	H += newStacksBegin - stacksBegin ;
	saveH += newStacksBegin - stacksBegin ;
	HB += newStacksBegin - stacksBegin ;
	if( IsRecord(C) ) C += newStacksBegin - stacksBegin ;
	if( IsRecord(CC) ) CC += newStacksBegin - stacksBegin ;
	if( IsRecord(CH) ) CH += newStacksBegin - stacksBegin ;
	if( IsGlobalRecord(ZT) ) ZT += newStacksBegin - stacksBegin ;
	if( IsGlobalRecord(Z) ) Z += newStacksBegin - stacksBegin ;

	PrimitiveRelease(stacksBegin) ;
	stacksBegin = newStacksBegin ;
	stacksEnd = newStacksEnd ;
	return true ;
}


void ControlStacksAtomGCMark()
{
	LocalStackAtomGCMark() ;
	GlobalStackAtomGCMark() ;
}

/* PROCEDURAL & CONTROL INSTRUCTIONS */

/* This macro creates an open ended predicate environment which will 
   progressivelly shrink all along the predicate code. The current size
   of this environment is determined by the second argument to each
   Call instruction in the predicate code. */

#define ALLOCATE() {						\
	saveE = E ;								\
	E = cEnvironmentPt(TopOfLocalStack()) ;	\
	Ef(E) = saveE ;							\
	Ef(CP) = CP ;							\
	Ef(CC) = CC ;							\
	Ef(B0) = B0 ;							\
}

static void NopInst()
{
	JumpNext()
}

static void LocalJumpInst()
{
	P = LookHdl() ;
	JumpNext()
}

static void AllocateInst()
{
	ALLOCATE()
	JumpNext()
}

static void DeallocateInst()
{
	CP = Ef(CP) ;
	CC = Ef(CC) ;
	E = Ef(E) ;
	JumpNext()
}

static void CallInst()
{
	ZEnsureFreeSpaceOnStacks(0) ;
	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	B0 = B ;
	if( Attention() && AttentionHandle(LookPred()) ) JumpNext()
	P = PredCode(LookPred()) ;
	JumpNext()
}

static void ExecuteInst()
{
	ZEnsureFreeSpaceOnStacks(0) ;
	B0 = B ;
	if( Attention() && AttentionHandle(LookPred()) ) JumpNext()
	P = PredCode(LookPred()) ;
	JumpNext()
}

static void CallVarInst()
{
	ZEnsureFreeSpaceOnStacks(0) ;
	CP = P + 1 ;	/* Skip argument */
	CC = C ;
	B0 = B ;
	PR = LookupPredicate(PrepareCall()) ;
	if( Attention() && AttentionHandle(PR) ) JumpNext()
	P = PredCode(PR) ;
	JumpNext()
}

static void ExecuteVarInst()
{
	ZEnsureFreeSpaceOnStacks(0) ;
	B0 = B ;
	PR = LookupPredicate(PrepareCall()) ;
	if( Attention() && AttentionHandle(PR) ) JumpNext()
	P = PredCode(PR) ;
	JumpNext()
}

static void ProceedInst()
{
	P = CP ;
	C = CC ;
	JumpNext()
}

static void DeallocProceedInst()
{
	P = CP = Ef(CP) ;
	C = CC = Ef(CC) ;
	E = Ef(E) ;
	JumpNext()
}

/* Makes lastChoice point to the choicepoint below fp */
static void CutInst()
{
	if( Ef(B0) > B ) {
		SetChoicePoint(Ef(B0)) ;	/* Local stack shrinks */
		/*TrailTidy() ;*/
	}
	if( IsDebugCP(B) ) DebugCut() ;
	JumpNext()
}

/* Inject call to '$$_cut/1' */
static void PutCutLevelInst()
{
	X(GetHdl()) = TagStruct(H) ;
	PushH(cutFunctor) ;
	PushH(TagAtom(Ef(B0))) ;	/* aux tag */
	JumpNext()
}

static void FailInst()
{
	DoFail()
}

static void UndefInst()
{
	if( (PR = SearchContextBelow(LookFunctor())) != nil ) {
		if( Attention() && AttentionHandle(PR) ) JumpNext()
		P = PredCode(PR) ;
		JumpNext()
	}
	DoFail()
}



/* PUT INSTRUCTIONS */

static void PutYVariableInst()
{
	static Hdl h ;
	h = &Y(GetWord()) ;
	X(GetHdl()) = ResetVar(h) ;
	JumpNext()
}

static void PutXVariableInst()
{
	static Hdl h ;
	h = GetHdl() ;
	X(GetHdl()) = X(h) = PushHVar() ;
	JumpNext()
}

static void PutXVariableOneInst()
{
	X(GetHdl()) = PushHVar() ;
	JumpNext()
}

static void PutXValueInst()
{
	static Hdl h ;
	h = GetHdl() ;
	X(GetHdl()) = X(h) ;
	JumpNext()
}

static void PutYValueInst()
{
	N = GetWord() ;
	X(GetHdl()) = Y(N) ;
	JumpNext()
}

static void PutUnsafeValueInst()
{
	VarValue2(D, Y(GetWord())) ;
	if( IsVar(D) && IsCurrEnvVar(D) ) {
		Assign(D, X(GetHdl()) = PushHVar()) ;
		JumpNext()
	}
	else {
		X(GetHdl()) = D ;
		JumpNext()
	}
}

static void PutAtomicInst()
{
	T = GetPt() ;
	X(GetHdl()) = T ;
	JumpNext()
}

static void PutNilInst()
{
	X(GetHdl()) = tNilAtom ;
	JumpNext()
}

static void PutStructureInst()
{
	PushH(GetPt()) ;
	X(GetHdl()) = TagStruct(H-1) ;
	JumpNext()
}

static void PutListInst()
{
	X(GetHdl()) = TagList(H) ;
	JumpNext()
}



/* GET INSTRUCTIONS */

static void GetYVariableInst()
{
	N = GetWord() ;
	Y(N) = X(GetHdl()) ;
	JumpNext()
}

static void GetXValueInst()
{
	static Hdl h ;
	h = GetHdl() ;
	if( Unify(X(GetHdl()), X(h)) ) {
		VarValue2(D, X(h)) ;
		X(h) = D ;
		JumpNext()
	}
	DoFail()
}

static void GetYValueInst()
{
	N = GetWord() ;
	if( Unify(Y(N), X(GetHdl())) ) JumpNext()
	DoFail()
}

static void GetAtomicInst()
{
	T = GetPt() ;
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, T) ;
		JumpNext()
	}
	if( D == T ) JumpNext()
	DoFail()
}

static void GetNilInst()
{
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, tNilAtom) ;
		JumpNext()
	}
	if( D == tNilAtom ) JumpNext()
	DoFail()
}

static void GetStructureInst()
{
	T = GetPt() ;		/* Get functor */
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, TagStruct(H)) ;
		PushH(T) ;
		writeMode = true ;
		JumpNext()
	}
	if( IsThisStruct(D, cFunctorPt(T)) ) {
		S = XStructArgs(D) ;
		writeMode = false ;
		JumpNext()
	}
	DoFail()
}

static void GetListInst()
{
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, TagList(H)) ;
		writeMode = true ;
		JumpNext()
	}
	if( IsList(D) ) {
		S = XListArgs(D) ;
		writeMode = false ;
		JumpNext()
	}
	DoFail()
}



/* UNIFY INSTRUCTIONS */

static void UnifyVoidInst()
{
	if( writeMode ) {
		N = GetWord() ;
		while( N-- ) PushHVar() ;
		JumpNext()
	}
	else {
		S += GetWord() ;
		JumpNext()
	}
}

static void UnifyVoidOneInst()
{
	if( writeMode ) {
		PushHVar() ;
		JumpNext()
	}
	else {
		S++ ;
		JumpNext()
	}
}

static void UnifyXVariableInst()
{
	if( writeMode ) {
		X(GetHdl()) = PushHVar() ;
		JumpNext()
	}
	else {
		X(GetHdl()) = *S++ ;
		JumpNext()
	}
}

static void UnifyYVariableInst()
{
	if( writeMode ) {
		Y(GetWord()) = PushHVar() ;
		JumpNext()
	}
	else {
		Y(GetWord()) = *S++ ;
		JumpNext()
	}
}

static void UnifyXValueInst()
{
	static Hdl h ;
	if( writeMode ) {
		PushH(X(GetHdl())) ;
		JumpNext()
	}
	else {
		if( Unify(*S++, X(h = GetHdl())) ) {
			VarValue2(D, X(h)) ;
			X(h) = D ;
			JumpNext()
		}
		DoFail()
	}
}

static void UnifyYValueInst()
{
	if( writeMode ) {
		PushH(Y(GetWord())) ;
		JumpNext()
	}
	else {
		if( Unify(*S++, Y(GetWord())) ) JumpNext()
		DoFail()
	}
}

static void UnifyXLocalValueInst()
{
	static Hdl h ;
	if( writeMode ) {
		VarValue2(D, X(h = GetHdl())) ;
		if( IsVar(D) && IsLocalVar(D) ) {
			Assign(D, X(h) = PushHVar()) ;
			JumpNext()
		}
		else {
			PushH(X(h) = D) ;
			JumpNext()
		}
	}
	else {
		if( Unify(*S++, X(h = GetHdl())) ) {
			VarValue2(D, X(h)) ;
			X(h) = D ;
			JumpNext()
		}
		DoFail()
	}
}

static void UnifyYLocalValueInst()
{
	if( writeMode ) {
		VarValue2(D, Y(GetWord())) ;
		if( IsVar(D) && IsLocalVar(D) ) {
			Assign(D, PushHVar()) ;
			JumpNext()
		}
		else {
			PushH(D) ;
			JumpNext()
		}
	}
	else {
		if( Unify(*S++, Y(GetWord())) ) JumpNext()
		DoFail()
	}
}

static void UnifyAtomicInst()
{
	if( writeMode ) {
		PushH(GetPt()) ;
		JumpNext()
	}
	else {
		T = GetPt() ;
		VarValue2(D, *S++) ;
		if( IsVar(D) ) {
			Assign(D, T) ;
			JumpNext()
		}
		if( D == T ) JumpNext()
		DoFail()
	}
}

static void UnifyNilInst()
{
	if( writeMode ) {
		PushH(tNilAtom) ;
		JumpNext()
	}
	else {
		VarValue2(D, *S++) ;
		if( IsVar(D) ) {
			Assign(D, tNilAtom) ;
			JumpNext()
		}
		if( D == tNilAtom ) JumpNext()
		DoFail()
	}
}



/* BUILD INSTRUCTIONS */

static void BuildVoidInst()
{
	N = GetWord() ;
	while( N-- ) PushHVar() ;
	JumpNext()
}

static void BuildVoidOneInst()
{
	PushHVar() ;
	JumpNext()
}

static void BuildXVariableInst()
{
	X(GetHdl()) = PushHVar() ;
	JumpNext()
}

static void BuildYVariableInst()
{
	Y(GetWord()) = PushHVar() ;
	JumpNext()
}

static void BuildXValueInst()
{
	PushH(X(GetHdl())) ;
	JumpNext()
}

static void BuildYValueInst()
{
	PushH(Y(GetWord())) ;
	JumpNext()
}

static void BuildXLocalValueInst()
{
	static Hdl h ;
	VarValue2(D, X(h = GetHdl())) ;
	if( IsVar(D) && IsLocalVar(D) ) {
		Assign(D, X(h) = PushHVar()) ;
		JumpNext()
	}
	else {
		PushH(X(h) = D) ;
		JumpNext()
	}
}

static void BuildYLocalValueInst()
{
	VarValue2(D, Y(GetWord())) ;
	if( IsVar(D) && IsLocalVar(D) ) {
		Assign(D, PushHVar()) ;
		JumpNext()
	}
	else {
		PushH(D) ;
		JumpNext()
	}
}

static void BuildAtomicInst()
{
	PushH(GetPt()) ;
	JumpNext()
}

static void BuildNilInst()
{
	PushH(tNilAtom) ;
	JumpNext()
}



/* CONTEXT INSTRUCTIONS */

static void PushCtxCallVarInst()
{
	PushH(TermToUnit(X0)) ;	/* Unit ref saved in the global stack */
	PushH(X0) ;
	PushH(C) ;
	C = TagList(H-2) ;
	X0 = X1 ;
	Jump(CallVar)
}

static void AllocSwitchCtxCallInst()
{
	ZEnsureFreeSpaceOnStacks(0) ;
	ALLOCATE()
	PushH(TermToUnit(P[0])) ;
	PushH(P[0]) ;
	if( T == tNilAtom )
		Error("Cannot switch top of emtpy context") ;
	PushH(XListTail(C)) ;
	C = TagList(H-2) ;

	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	B0 = B ;
	PR = LookupPredicate(cFunctorPt(P[5])) ;
	if( Attention() && AttentionHandle(PR) ) JumpNext()
	P = PredCode(PR) ;
	JumpNext()
}

static void SwitchCtxCallVarInst()
{
	PushH(TermToUnit(X0)) ;
	PushH(X0) ;
	if( C == tNilAtom )
		Error("Cannot switch top of emtpy context") ;
	PushH(XListTail(C)) ;
	C = TagList(H-2) ;
	X0 = X1 ;
	Jump(CallVar)
}

static void EmptyCtxCallVarInst()
{
	C = tNilAtom ;
	Jump(CallVar)
}

static void DownCtxCallVarInst()
{
	if( C != tNilAtom )
		C = XListTail(C) ;
	Jump(CallVar)
}

static void PushHCtxInst()
{
	PushH(C) ;
	PushH(CH) ;
	CH = TagList(H-2) ;
	JumpNext()
}

static void PopHCtxInst()
{
	CH = XListTail(CH) ;
	JumpNext()
}

static void EnterHCtxInst()
{
	if( CH == tNilAtom )
		Error("Empty historic context: cannot enter") ;
	Y(OutPerm(0)) = CH ;
	C = XListHead(CH) ;
	CH = XListTail(CH) ;
	JumpNext()
}

static void ExitHCtxInst()
{
	CH = Y(OutPerm(0)) ;
	JumpNext()
}



/* INDEXING INSTRUCTIONS */

static void MakeIndexInst()
{
	DoIndex(LookPred()) ;
	P-- ;
	JumpNext()
}

static void TryMeElseInst()
{
	static Hdl h ;
	static int n ;
	h = ClauseCode(GetClause()) ;
	n = GetWord() ;
	saveB = B ;
	B = cChoicePointPt(TopOfLocalStack() - n) - 1 ;
	SaveState(saveB, h, n) ;
	JumpNext()
}

static void RetryMeElseInst()
{
	Bf(P) = ClauseCode(GetClause()) ;
	RestoreState(GetWord()) ;
	JumpNext()
}

static void TrustMeInst()
{
	SkipInst() ;
	RestoreState(GetWord()) ;
	SetChoicePoint(Bf(B)) ;
	JumpNext()
}

static void TryInst()
{	
	static Hdl h ;
	h = GetHdl() ;
	saveB = B ;
	B = cChoicePointPt(TopOfLocalStack() - cWord(h[-1])) - 1 ;
	SaveState(saveB, P, cWord(h[-1])) ;
	P = h ;
	JumpNext()
}

static void RetryInst()
{
	Bf(P) = P + 1 ;
	P = LookHdl() ;
	RestoreState(cWord(P[-1])) ;
	JumpNext()
}

static void TrustInst()
{
	P = LookHdl() ;
	RestoreState(cWord(P[-1])) ;
	SetChoicePoint(Bf(B)) ;
	JumpNext()
}

static void SwitchOnTermAuxInst()
{
	if( IsList(D) ) {
		P = cHdl(P[1]) ;
		JumpNext()
	}
	elif( IsAtomic(D) ) {
		P = cHdl(P[0]) ;
		JumpNext()
	}
	elif( IsVar(D) ) {
		P = cHdl(P[3]) ;
		JumpNext()
	}
	else {
		P = cHdl(P[2]) ;
		JumpNext()
	}
}

static void SwitchOnTerm0Inst()
{
	VarValue2(D, X0) ;
	Jump(SwitchOnTermAuxInst)
}

static void SwitchOnTerm1Inst()
{
	VarValue2(D, X1) ;
	Jump(SwitchOnTermAuxInst)
}

static void SwitchOnTerm2Inst()
{
	VarValue2(D, X2) ;
	Jump(SwitchOnTermAuxInst)
}

static void SwitchOnAtomicInst()
{
	register PrologHashTable ht, ht1 ;
	/* Got here from SwitchOnTerm: D already constains Drf(X?) */
	ht = (PrologHashTable)(P+2) ;
	dolist(ht1, ht + PrologHash(D, cWord(P[0])), ht1->next)
		if( ht1->value == D ) {
			P = ht1->address ;
			JumpNext()
		}
	P = cHdl(P[1]) ;
	JumpNext()
}

static void SwitchOnStructureInst()
{
	/* Got here from SwitchOnTerm: D already constains Drf(X?) */
	register PrologHashTable ht, ht1 ;
	D = cPt(XStructFunctor(D)) ;
	ht = (PrologHashTable)(P+2) ;
	dolist(ht1, ht + PrologHash(D, cWord(P[0])), ht1->next)
		if( ht1->value == D ) {
			P = ht1->address ;
			JumpNext()
		}
	P = cHdl(P[1]) ;
	JumpNext()
}

static void DiscardAndFailInst()
{
	SetChoicePoint(Bf(B)) ;
	DoFail()
}



/* DEBUGGING INSTRUCTIONS */

static void DebugExitInst()
{
	DebugExitCode() ;
	JumpNext()
}

static void DebugRedoInst()
{
	DebugRedoCode() ;
	JumpNext()
}

static void DebugRetryInst()
{
	DebugRetryCode() ;
	JumpNext()
}



/* INSTRUCTIONS */

#define maxInstructions		100

Pt	Nop, FAllocate, FDeallocate, Proceed, DeallocProceed, LocalJump,
	Call, Execute, CallVar, ExecuteVar,
	Cut, PutCutLevel, Fail, Undef,

	GetYVariable, GetXValue, GetYValue, GetAtomic, GetNil,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,

	PutXValue, PutYValue, PutUnsafeValue, PutAtomic, PutNil, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,

	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue,
	UnifyAtomic, UnifyNil, BuildVoid, BuildVoidOne, BuildXVariable,

	BuildYVariable, BuildXValue, BuildYValue, BuildXLocalValue,
	BuildYLocalValue, BuildAtomic, BuildNil,

	PushCtxCallVar, AllocSwitchCtxCall, SwitchCtxCallVar, EmptyCtxCallVar,
	DownCtxCallVar, PushHCtx, PopHCtx, EnterHCtx, ExitHCtx,

	MakeIndex, TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust,
	SwitchOnTermAux, SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
	SwitchOnAtomic, SwitchOnStructure, DiscardAndFail,
	
	DebugExit, DebugRedo, DebugRetry ;

static struct instInfo
{
	Pt inst ;
	CharPt name, types ;
} insts[maxInstructions] ;
static int nInsts = 0 ;

static void II(Pt *inst, Proc proc, CharPt name, CharPt types)
{
	insts[nInsts].inst = InstEncode(proc) ;
	if( inst != nil ) *inst = insts[nInsts].inst ;
	insts[nInsts].name = name ;
	insts[nInsts].types = types ;
	nInsts++ ;
}

static void InstructionsInit()
{
/* PROCEDURAL & CONTROL INSTRUCTIONS */
	II(&Nop,			NopInst,				"Nop",				"") ;
	II(&FAllocate,		AllocateInst,			"Allocate",			"") ;
	II(&FDeallocate,	DeallocateInst,			"Deallocate",		"") ;
	II(&Proceed,		ProceedInst,			"Proceed",			"@") ;
	II(&DeallocProceed, DeallocProceedInst,		"DeallocProceed",	"@") ;
	II(&LocalJump,		LocalJumpInst,			"LocalJump",		"l@") ;
	II(&Call,			CallInst,				"Call",				"pe") ;
	II(&Execute,		ExecuteInst,			"Execute",			"p@") ;
	II(&CallVar,		CallVarInst,			"CallVar",			"e") ;
	II(&ExecuteVar,		ExecuteVarInst,			"ExecuteVar",		"@") ;
	II(&Cut,			CutInst,				"Cut",				"") ;
	II(&PutCutLevel,	PutCutLevelInst,		"PutCutLevel",		"x") ;
	II(&Fail,			FailInst,				"Fail",				"") ;
	II(&Undef,			UndefInst,				"Undef",			"f@") ;

/* GET INSTRUCTIONS */
	II(&GetYVariable,	GetYVariableInst,		"GetYVariable",		"yx") ;
	II(&GetXValue,		GetXValueInst,			"GetXValue",		"xx") ;
	II(&GetYValue,		GetYValueInst,			"GetYValue",		"yx") ;
	II(&GetAtomic,		GetAtomicInst,			"GetAtomic",		"tx") ;
	II(&GetNil,			GetNilInst,				"GetNil",			"x") ;
	II(&GetStructure,	GetStructureInst,		"GetStructure",		"fx") ;
	II(&GetList,		GetListInst,			"GetList",			"x") ;
	
/* PUT INSTRUCTIONS */
	II(&PutXVariable,	PutXVariableInst,		"PutXVariable",		"xx") ;
	II(&PutXVariableOne,PutXVariableOneInst,	"PutXVariableOne",	"x"	) ;
	II(&PutYVariable,	PutYVariableInst,		"PutYVariable",		"yx") ;
	II(&PutXValue,		PutXValueInst,			"PutXValue",		"xx") ;
	II(&PutYValue,		PutYValueInst,			"PutYValue",		"yx") ;
	II(&PutUnsafeValue,	PutUnsafeValueInst,		"PutUnsafeValue",	"yx") ;
	II(&PutAtomic,		PutAtomicInst,			"PutAtomic",		"tx") ;
	II(&PutNil,			PutNilInst,				"PutNil",			"x") ;
	II(&PutStructure,	PutStructureInst,		"PutStructure",		"fx") ;
	II(&PutList,		PutListInst,			"PutList",			"x"	) ;

/* UNIFY INSTRUCTIONS */
	II(&UnifyVoid,		UnifyVoidInst,			"UnifyVoid",		"n") ;
	II(&UnifyVoidOne,	UnifyVoidOneInst,		"UnifyVoidOne",		"") ;
	II(&UnifyXVariable,	UnifyXVariableInst,		"UnifyXVariable",	"x") ;
	II(&UnifyYVariable,	UnifyYVariableInst,		"UnifyYVariable",	"y") ;
	II(&UnifyXLocalValue,UnifyXLocalValueInst,	"UnifyXLocalValue",	"x") ;
	II(&UnifyYLocalValue,UnifyYLocalValueInst,	"UnifyYLocalValue",	"y") ;
	II(&UnifyXValue,	UnifyXValueInst,		"UnifyXValue",		"x") ;	
	II(&UnifyYValue,	UnifyYValueInst,		"UnifyYValue",		"y"	) ;
	II(&UnifyAtomic,	UnifyAtomicInst,		"UnifyAtomic",		"t") ;
	II(&UnifyNil,		UnifyNilInst,			"UnifyNil",			"") ;

/* BUILD INSTRUCTIONS */
	II(&BuildVoid,		BuildVoidInst,			"BuildVoid",		"n") ;
	II(&BuildVoidOne,	BuildVoidOneInst,		"BuildVoidOne",		"") ;
	II(&BuildXVariable,	BuildXVariableInst,		"BuildXVariable",	"x"	) ;
	II(&BuildYVariable,	BuildYVariableInst,		"BuildYVariable",	"y"	) ;
	II(&BuildXValue,	BuildXValueInst,		"BuildXValue",		"x") ;
	II(&BuildYValue,	BuildYValueInst,		"BuildYValue",		"y") ;
	II(&BuildXLocalValue,BuildXLocalValueInst,	"BuildXLocalValue",	"x"	) ;
	II(&BuildYLocalValue,BuildYLocalValueInst,	"BuildYLocalValue",	"y"	) ;
	II(&BuildAtomic,	BuildAtomicInst,		"BuildAtomic",		"t") ;
	II(&BuildNil,		BuildNilInst,			"BuildNil",			"") ;

/* CONTEXT INSTRUCTIONS */
	II(&PushCtxCallVar,	PushCtxCallVarInst,		"PushCtxCallVar",	"e") ;
	II(&AllocSwitchCtxCall, AllocSwitchCtxCallInst, "AllocSwitchCtxCall", "te") ;
	II(&SwitchCtxCallVar,SwitchCtxCallVarInst,	"SwitchCtxCallVar",	"e") ;
	II(&EmptyCtxCallVar,EmptyCtxCallVarInst,	"EmptyCtxCallVar",	"e") ;
	II(&DownCtxCallVar,	DownCtxCallVarInst,		"DownCtxCallVar",	"e") ;	
	II(&PushHCtx,		PushHCtxInst,			"PushHCtx",			"") ;
	II(&PopHCtx,		PopHCtxInst,			"PopHCtx",			"") ;
	II(&EnterHCtx,		EnterHCtxInst,			"EnterHCtx",		"") ;
	II(&ExitHCtx,		ExitHCtxInst,			"ExitHCtx",			"") ;

/* INDEXING INSTRUCTIONS */
	II(&MakeIndex,		MakeIndexInst,			"MakeIndex",		"p@") ;
	II(&TryMeElse,		TryMeElseInst,			"TryMeElse",		"cn") ;
	II(&RetryMeElse,	RetryMeElseInst,		"RetryMeElse",		"cn") ;
	II(&TrustMe,		TrustMeInst,			"TrustMe",			".n") ;
	II(&Try,			TryInst,				"Try",				"l") ;
	II(&Retry,			RetryInst,				"Retry",			"l"	) ;
	II(&Trust,			TrustInst,				"Trust",			"l"	) ;
	II(&SwitchOnTermAux,SwitchOnTermAuxInst,	"SwitchOnTermAux",	"") ;
	II(&SwitchOnTerm0,	SwitchOnTerm0Inst,		"SwitchOnTerm0",	"llll@") ;
	II(&SwitchOnTerm1,	SwitchOnTerm1Inst,		"SwitchOnTerm1",	"llll") ;
	II(&SwitchOnTerm2,	SwitchOnTerm2Inst,		"SwitchOnTerm2",	"llll") ;
	II(&SwitchOnAtomic,	SwitchOnAtomicInst,		"SwitchOnAtomic",	"H") ;
	II(&SwitchOnStructure,SwitchOnStructureInst,"SwitchOnStructure","H") ;
	II(&DiscardAndFail,	DiscardAndFailInst,		"DiscardAndFail",	"") ;

/* DEBUGGING INSTRUCTIONS */
	II(&DebugExit,		DebugExitInst,			"DebugExit",		"") ;
	II(&DebugRedo,		DebugRedoInst,			"DebugRedo",		"") ;
	II(&DebugRetry,		DebugRetryInst,			"DebugRetry",		"") ;
}

Bool GetInstInfo(Pt inst, CharPt *name, CharPt *types)
{
	register int i ;	
	dotimes(i, nInsts)
		if( insts[i].inst == inst ) {
			*name = insts[i].name ;
			*types = insts[i].types ;
			return true ;
		}
	return false ;
}

void MachineInit()
{
	InstructionsInit() ;
}

void UserModeInstructions()
{
}
