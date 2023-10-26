/*
 *   This file is part of the CxProlog system

 *   Machine.c
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
Pt C, CC, CH, X[maxX], D, T ;
unsigned int N ;
Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
Pt PFailAddr ;
Size hostSpeed ;
static Bool writeMode ;

#if 0
static short filler ;
#endif

static Pt
	SysCall, SysExecute, SysCallVar, SysExecuteVar,	ExecuteVarAux, SysExecuteVarAux ;



/* MISC FUNCTIONS */

static PredicatePt SearchContextBelow(FunctorPt f)
{
	register PredicatePt pr ;
	register Pt c = C ;
	if( C != tNilAtom && CanBeVisible(f) )
		for( C = Drf(XListTail(C)) ; C != tNilAtom ; C = Drf(XListTail(C)) )
			if( (pr = FindVisiblePredicate(f)) != nil )
				return pr ;
	if( undefWarnings_flag )
		Warning("Predicate '%s' is not visible in context %s",
							FunctorNameArity(f), TermAsStr(c)) ;
	return nil ;
}

static PredicatePt SearchContext(FunctorPt f)
{
	register PredicatePt pr ;
	if( (pr = FindPredicate(f)) != nil )
		return pr ;
	else
		return( SearchContextBelow(f) ) ;
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
		Error("EXTRA term in call/1: '%s'", XExtraAsStr(t)) ;
	else Default("PrepareCall") ;
	return nil ;
}

void MachineRun()
{
	DebugReset() ;
#if 0
	for(;;) {
		DisassembleInst(P) ;
		(*cProc(*P++))() ;
	}
#else
	Run() ;
#endif
	InternalError("RunMachine") ;
}

Bool MachineIsOn()
{
	return P != nil ;
}

void CheckHost()
{
	if( sizeof(Word) != sizeof(Pt) )
		FatalError("sizeof(Word) != sizeof(Pt)") ;
	if( cWord(&stacksBegin) % sizeof(Word) != 0 )
		Warning("Machine registers are not aligned") ;
	if( cWord(BufferBegin()) < 0 )
		Warning("This machine is backwards") ;
	if( GetTag(BufferBegin()) != 0 )
		Warning("Tags are clobbered by Pt values") ;
	if( cWord(BufferBegin()) % sizeof(Word) != 0 )
		Warning("Memory areas are not aligned") ;
}

void CheckHostSpeed()
{
	double tt = CurrTime() ;
	HSave() ;
	hostSpeed = 0 ;
	do {
		CharPt s = "a(P,T,O):-a(O),a(0,P),a(O,T,P,L,R),a(T,P,L,R),a(0,T)." ;
		Pt t = ReadTermFromStr(s) ;
		TermAsStr(t) ;
		HRestore() ;
		hostSpeed++ ;
	} while( CurrTime() - tt < 0.03 ) ;
	if( hostSpeed == 0 ) hostSpeed = 1 ;
}



/* TRAIL */

/* The trail contain references to bound logic variables located in the
   local stack and to bound logic variables located in the global stack.
*/

static Hdl saveTR ;

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
	register ChoicePointPt cp ;
#if 0
	Error("Trail overflow") ;
#endif
	MemoryGrowWarning("trail", trailSize, trailSize * 2) ;
	newTrailBegin = PrimitiveAllocate(trailSize * 2) ;
	newTrailEnd = newTrailBegin + trailSize * 2 ;
	CopyWords(newTrailBegin, trailBegin, trailSize) ;
	TR += newTrailBegin - trailBegin ;
	saveTR += newTrailBegin - trailBegin ;
	for( cp = B ; Lt(cp, stacksEnd) ; cp = cp->B )
		cp->TR += newTrailBegin - trailBegin ;
	PrimitiveRelease(trailBegin) ;
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
		else Xc(i) += newStacksBegin - stacksBegin ;
	}
}



/* LOCAL STACK */

/* The local stack contains local vars, references to global vars,
   references to the trail, references to executable code,  references
   to terms in the global stack.  */

void LocalStackShow()
{
	register Hdl a, aLim = cHdl(TopOfLocalStack()) ;
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
	register Hdl z, a, aLim = cHdl(TopOfLocalStack()) ;
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
		else Write("TERM  %s\n", TermAsStr(*a)) ;
	}
}

static void GlobalStackRelocate(Hdl newStacksBegin) {
	register Hdl z, a ;
	for( z = newStacksBegin, a = stacksBegin ; a < H ; z++, a++ ) {
		if( IsVar(*a) ) {
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

void ControlStacksExpand()
{
	Size stacksSize = stacksEnd - stacksBegin ;
	Hdl newStacksBegin, newStacksEnd ;
#if 0
	Error("Local stack overflow") ;
	Write("free stack = %ld\n", TopOfLocalStack() - cPt(H)) ;
#endif
	MemoryGrowWarning("stacks", stacksSize, stacksSize * 2) ;
	newStacksBegin = PrimitiveAllocate(stacksSize * 2) ;
	newStacksEnd = newStacksBegin + stacksSize * 2 ;

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
	C += IsAtom(C) ? 0 : (newStacksBegin - stacksBegin) ;
	CC += IsAtom(CC) ? 0 : (newStacksBegin - stacksBegin) ;
	CH += IsAtom(CH) ? 0 : (newStacksBegin - stacksBegin) ;

	PrimitiveRelease(stacksBegin) ;
	stacksBegin = newStacksBegin ;
	stacksEnd = newStacksEnd ;
}

void ControlStacksAtomGCMark()
{
	LocalStackAtomGCMark() ;
	GlobalStackAtomGCMark() ;
}

/* PROCEDURAL & CONTROL INSTRUCTIONS */


#define ALLOCATE() {						\
	static EnvironmentPt saveE ;			\
	saveE = E ;								\
	E = cEnvironmentPt(TopOfLocalStack()) ;	\
	Ef(E) = saveE ;							\
	Ef(CP) = CP ;							\
	Ef(CC) = CC ;							\
	Ef(B0) = B0 ;							\
}

#define AT_EVERY_CALL(p, f) {				\
	if( IsStacksOverflow() ) ControlStacksExpand() ;\
	if( TraceActive() ) DebugCall(p, f) ;	\
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
	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	AT_EVERY_CALL(LookPred(), nil) ;
	P = PredCode(LookPred()) ;
	B0 = B ;
	JumpNext()
}

static void SysCallInst()	/* system predicate calling another system predicate */
{
	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	P = PredCode(LookPred()) ;
	B0 = B ;
	JumpNext()
}

static void ExecuteInst()
{
	AT_EVERY_CALL(LookPred(), nil) ;
	P = PredCode(LookPred()) ;
	B0 = B ;
	JumpNext()
}

static void SysExecuteInst()	/* system predicate calling another system predicate */
{
	P = PredCode(LookPred()) ;
	B0 = B ;
	JumpNext()
}

static void CallVarInst()
{
	CP = P + 1 ;	/* Skip argument */
	CC = C ;
	T = cPt(PrepareCall()) ;
	Jump(ExecuteVarAux)
}

static void SysCallVarInst()
{
	CP = P + 1 ;	/* Skip argument */
	CC = C ;
	T = cPt(PrepareCall()) ;
	Jump(SysExecuteVarAux)
}

static void ExecuteVarInst()
{
	T = cPt(PrepareCall()) ;
	Jump(ExecuteVarAux)
}

static void SysExecuteVarInst()
{
	T = cPt(PrepareCall()) ;
	Jump(SysExecuteVarAux)
}

static void ExecuteVarAuxInst()
{
	PredicatePt pr ;
	if( (pr = SearchContext(cFunctorPt(T))) == nil ) {
		AT_EVERY_CALL(nil, cFunctorPt(T)) ;
		DoFail()
	}
	else {
		AT_EVERY_CALL(pr, nil) ;
		P = PredCode(pr) ;
		B0 = B ;
		JumpNext()
	}
}

static void SysExecuteVarAuxInst()	/* system predicate calling a predicate */
{
	static PredicatePt pr ;
	if( (pr = SearchContext(cFunctorPt(T))) == nil ) {
		DoFail()
	}
	else {
		if( not PredIsBuiltin(pr) )
			AT_EVERY_CALL(pr, nil) ;
		P = PredCode(pr) ;
		B0 = B ;
		JumpNext()
	}
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
		B = Ef(B0) ;
		HB = Bf(H) ;
		/*TrailTidy() ;*/
	}
	JumpNext()
}

static void PutCutLevelInst()
{
	X(GetHdl()) = TagStruct(H) ;
	Push(H, cutFunctor) ;
	Push(H, TagAtom(Ef(B0))) ;	/* aux tag */
	JumpNext()
}

static void FailInst()
{
	DoFail()
}

static void UndefInst()
{
	if( (T = cPt(SearchContextBelow(LookFunctor()))) == nil )
		DoFail()
	P = PredCode(cPredicatePt(T)) ;
	JumpNext()
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
	X(GetHdl()) = X(h) = PushVar(H) ;
	JumpNext()
}

static void PutXVariableOneInst()
{
	X(GetHdl()) = PushVar(H) ;
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
		Assign(D, X(GetHdl()) = PushVar(H)) ;
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
	T = GetPt() ;
	X(GetHdl()) = TagStruct(H) ;
	Push(H, T) ;
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
	T = GetPt() ;
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, TagStruct(H)) ;
		Push(H, T) ;
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
		while( N-- ) PushVar(H) ;
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
		PushVar(H) ;
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
		X(GetHdl()) = PushVar(H) ;
		JumpNext()
	}
	else {
		X(GetHdl()) = *S++ ;
		JumpNext()
	}
	JumpNext()
}

static void UnifyYVariableInst()
{
	if( writeMode ) {
		Y(GetWord()) = PushVar(H) ;
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
		Push(H, X(GetHdl())) ;
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
		Push(H, Y(GetWord())) ;
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
			Assign(D, X(h) = PushVar(H)) ;
			JumpNext()
		}
		else {
			Push(H, X(h) = D) ;
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
			Assign(D, PushVar(H)) ;
			JumpNext()
		}
		else {
			Push(H, D) ;
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
		Push(H, GetPt()) ;
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
		Push(H, tNilAtom) ;
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
	while( N-- ) PushVar(H) ;
	JumpNext()
}

static void BuildVoidOneInst()
{
	PushVar(H) ;
	JumpNext()
}

static void BuildXVariableInst()
{
	X(GetHdl()) = PushVar(H) ;
	JumpNext()
}

static void BuildYVariableInst()
{
	Y(GetWord()) = PushVar(H) ;
	JumpNext()
}

static void BuildXValueInst()
{
	Push(H, X(GetHdl())) ;
	JumpNext()
}

static void BuildYValueInst()
{
	Push(H, Y(GetWord())) ;
	JumpNext()
}

static void BuildXLocalValueInst()
{
	static Hdl h ;
	VarValue2(D, X(h = GetHdl())) ;
	if( IsVar(D) && IsLocalVar(D) ) {
		Assign(D, X(h) = PushVar(H)) ;
		JumpNext()
	}
	else {
		Push(H, X(h) = D) ;
		JumpNext()
	}
}

static void BuildYLocalValueInst()
{
	VarValue2(D, Y(GetWord())) ;
	if( IsVar(D) && IsLocalVar(D) ) {
		Assign(D, PushVar(H)) ;
		JumpNext()
	}
	else {
		Push(H, D) ;
		JumpNext()
	}
}

static void BuildAtomicInst()
{
	Push(H, GetPt()) ;
	JumpNext()
}

static void BuildNilInst()
{
	Push(H, tNilAtom) ;
	JumpNext()
}



/* CONTEXT INSTRUCTIONS */

static void PushCtxCallVarInst()
{
	Push(H, TermToUnit(X0)) ;	/* Unit ref placed in the global stack */
	Push(H, X0) ;
	Push(H, C) ;
	C = TagList(H-2) ;
	X0 = X1 ;
	Jump(CallVar)
}

static void AllocSwitchCtxCallInst()
{
	ALLOCATE()
	Push(H, TermToUnit(P[0])) ;
	Push(H, P[0]) ;
	if( T == tNilAtom )
		Error("Cannot switch top of emtpy context") ;
	Push(H, XListTail(C)) ;
	C = TagList(H-2) ;

	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	T = P[5] ;	/* functor */
	Jump(ExecuteVarAux)
}

static void SwitchCtxCallVarInst()
{
	Push(H, TermToUnit(X0)) ;
	Push(H, X0) ;
	if( C == tNilAtom )
		Error("Cannot switch top of emtpy context") ;
	Push(H, XListTail(C)) ;
	C = TagList(H-2) ;
	X0 = X1 ;
	Jump(CallVar)
}

static void EmptyCtxCallVarInst()
{
	C = tNilAtom ;
	Jump(CallVar)
}

static void PushHCtxInst()
{
	Push(H, C) ;
	Push(H, CH) ;
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

#define SaveState(next, old)				\
				Bf(E) = E ;					\
				Bf(CP) = CP ;				\
				Bf(B) = old ;				\
				Bf(B0) = B0 ;				\
				Bf(C) = C ;					\
				Bf(CC) = CC ;				\
				Bf(CH) = CH ;				\
				Bf(P) = next ;				\
				Bf(TR) = TR ;				\
				HB = Bf(H) = H ;			\
				while( N-- ) A(N) = Xc(N)

#define RestoreState()						\
				E = Bf(E) ;					\
				CP = Bf(CP) ;				\
				B0 = Bf(B0) ;				\
				C = Bf(C) ;					\
				CC = Bf(CC) ;				\
				CH = Bf(CH) ;				\
				H = Bf(H) ;					\
				saveTR = Bf(TR) ;			\
				TrailRestore() ;			\
				while( N-- ) Xc(N) = A(N)

#define DiscardLastChoice()					\
				B = Bf(B) ;					\
				HB = Bf(H)

static void TryMeElseInst()
{
	static Hdl h ;
	static ChoicePointPt saveB ;
	h = ClauseCode(GetClause()) ;
	N = GetWord() ;
	saveB = B ;
	B = cChoicePointPt(TopOfLocalStack() - N) - 1 ;
	SaveState(h, saveB) ;
	JumpNext()
}

static void RetryMeElseInst()
{
	Bf(P) = ClauseCode(GetClause()) ;
	N = GetWord() ;
	RestoreState() ;
	JumpNext()
}

static void TrustMeInst()
{
	SkipInst() ;
	N = GetWord() ;
	RestoreState() ;
	DiscardLastChoice() ;
	JumpNext()
}

static void TryInst()
{	
	static Hdl h ;
	static ChoicePointPt saveB ;
	h = GetHdl() ;
	N = cWord(h[-1]) ;
	saveB = B ;
	B = cChoicePointPt(TopOfLocalStack() - N) - 1 ;
	SaveState(P, saveB) ;
	P = h ;
	JumpNext()
}

static void RetryInst()
{
	Bf(P) = P + 1 ;
	P = LookHdl() ;
	N = cWord(P[-1]) ;
	RestoreState() ;
	JumpNext()
}

static void TrustInst()
{
	P = LookHdl() ;
	N = cWord(P[-1]) ;
	RestoreState() ;
	DiscardLastChoice() ;
	JumpNext()
}

static void DiscardAndFailInst()
{
	DiscardLastChoice() ;
	DoFail()
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


/* INSTRUCTIONS */

#define maxInstructions		100

Pt	Nop, Allocate, Deallocate, Proceed, DeallocProceed, LocalJump,
	Call, Execute, CallVar, ExecuteVar, Cut, PutCutLevel, Fail, Undef,

	GetYVariable, GetXValue, GetYValue, GetAtomic, GetNil,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,

	PutXValue, PutYValue, PutUnsafeValue, PutAtomic, PutNil, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,

	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue,
	UnifyAtomic, UnifyNil, BuildVoid, BuildVoidOne, BuildXVariable,

	BuildYVariable, BuildXValue, BuildYValue, BuildXLocalValue,
	BuildYLocalValue, BuildAtomic, BuildNil,

	PushCtxCallVar, AllocSwitchCtxCall, SwitchCtxCallVar, EmptyCtxCallVar,
	PushHCtx, PopHCtx, EnterHCtx, ExitHCtx,

	MakeIndex, TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust,
	SwitchOnTermAux, SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
	SwitchOnAtomic, SwitchOnStructure, DiscardAndFail ;

static struct instInfo
{
	Pt inst ;
	CharPt name, types ;
} insts[maxInstructions] ;
static int nInsts = 0 ;

static void II(Pt *inst, Proc proc, CharPt name, CharPt types)
{
	insts[nInsts].inst = *inst = Z(proc) ;
	insts[nInsts].name = name ;
	insts[nInsts].types = types ;
	nInsts++ ;
}

static void InstructionsInit()
{
/* AUXILIARY PROCEDURAL & CONTROL INSTRUCTIONS */
	II(&SysCall,		SysCallInst,			"SysCall",			"pe") ;
	II(&SysExecute,		SysExecuteInst,			"SysExecute",		"p@") ;
	II(&SysCallVar,		SysCallVarInst,			"SysCallVar",		"e") ;
	II(&SysExecuteVar,	SysExecuteVarInst,		"SysExecuteVar",	"@") ;
	II(&ExecuteVarAux,	ExecuteVarAuxInst,		"ExecuteVarAux",	"") ;
	II(&SysExecuteVarAux,SysExecuteVarAuxInst,	"SysExecuteVarAux",	"") ;

/* PROCEDURAL & CONTROL INSTRUCTIONS */
	II(&Nop,			NopInst,				"Nop",				"") ;
	II(&Allocate,		AllocateInst,			"Allocate",			"") ;
	II(&Deallocate,		DeallocateInst,			"Deallocate",		"") ;
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
	II(&DiscardAndFail,	DiscardAndFailInst,		"DiscardAndFail",	"") ;
	II(&SwitchOnTermAux,SwitchOnTermAuxInst,	"SwitchOnTermAux",	"") ;
	II(&SwitchOnTerm0,	SwitchOnTerm0Inst,		"SwitchOnTerm0",	"llll@") ;
	II(&SwitchOnTerm1,	SwitchOnTerm1Inst,		"SwitchOnTerm1",	"llll") ;
	II(&SwitchOnTerm2,	SwitchOnTerm2Inst,		"SwitchOnTerm2",	"llll") ;
	II(&SwitchOnAtomic,	SwitchOnAtomicInst,		"SwitchOnAtomic",	"H") ;
	II(&SwitchOnStructure,SwitchOnStructureInst,"SwitchOnStructure","H") ;
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
	PFailAddr = Z(FailInst) ;
	C = tNilAtom ;	/* This makes CurrUnit() == bottomUnit */
	InstructionsInit() ;
	Call = Z(SysCallInst) ;
	Execute = Z(SysExecuteInst) ;
/*	CallVar = Z(SysCallVarInst) ;
	ExecuteVar = Z(SysExecuteVarInst) ;*/
}

void UserModeInstructions()
{
	Call = Z(CallInst) ;
	Execute = Z(ExecuteInst) ;
	CallVar = Z(CallVarInst) ;
	ExecuteVar = Z(ExecuteVarInst) ;
}
