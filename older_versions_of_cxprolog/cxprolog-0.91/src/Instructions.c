/*
 *   This file is part of the CxProlog system

 *   Instructions.c
 *   by A.Miguel Dias - 2002/03/28
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* STACKS CONTROL */

#define PushHVar()			( ResetVar(H), cPt(H++) )
#define PushH(v)			Push(H, v)
#define PopH()				Pop(H)
#define GrowH(n)			Grow(H, n)



/* ARGUMENT ACCESS */

#define GetPt()			(*P++)
#define LookPt()		(*P)
#define LookHdl()		cHdl(*P)
#define GetHdl()		cHdl(*P++)
#define GetClause()		cClausePt(*P++)
#define GetWord()		cWord(*P++)
#define LookWord()		cWord(*P)
#define SkipWord()		(P++)
#define LookPred()		cPredicatePt(*P)
#define LookFunctor()	cFunctorPt(*P)



/* AUXILIARY REGISTERS */

static Bool writeMode ;
static UnitPt U ;


/* AUXILIARY FUNCTIONS */

static PredicatePt SearchContextBelow(FunctorPt f)
{
	Pt c = C ;
	if( C != tNilAtom ) C = XListTail(C) ;
	for( ; C != tNilAtom ; C = XListTail(C) ) { /* no need to deref */
		PredicatePt pr = FindPredicate(f) ;
		if( pr != nil && (PredIsVisible(pr) || forceVisibility_flag) )
			return pr ;
	}
	if( undefWarnings_flag )
		Warning("Predicate '%s' is not visible in context %s",
						FunctorNameArity(f), TermAsStr(c)) ;
	return failPred ;
}

static PredicatePt SearchContext(FunctorPt f) /* for the context instructions */
{
	PredicatePt pr = FindPredicate(f) ;
	if( pr != nil && (PredIsVisible(pr) || forceVisibility_flag) )
		return pr ;
	else return SearchContextBelow(f) ;
}

static void PrepareCallError(Pt t) {
	t = Drf(t) ;
	if( IsNumber(t) ) 
		Error("NUMBER in call/1: %s", XNumberAsStr(t)) ;
	elif( IsVar(t) )
		Error("Unbound VARIABLE in call/1") ;
	elif( IsExtra(t) ) 
		Error("EXTRA in call/1: '%s'", XExtraAsStr(t)) ;
	else InternalError("PrepareCallError") ;
}

static FunctorPt PrepareCall(register Pt t)
{
	VarValue(t) ;
redo:
	if( IsStruct(t) ) {
		if( IsUnitParam(t) ) {
			t = Drf(Z(OutParam(XUnitParam(t)))) ;
			goto redo ;
		}
		else {
			register int s = XStructArity(t) ;
			while( s-- )
				Xc(s) = XStructArg(t,s) ;
			return XStructFunctor(t) ;
		}
	}
	elif( IsAtom(t) ) {
		return LookupFunctor(XAtom(t), 0) ;
	}
	elif( IsList(t) ) {
		X0 = XListHead(t) ;
		X1 = XListTail(t) ;
		return listFunctor ;
	}
	else PrepareCallError(t) ;
	return nil ; /* avoids warning */
}

static FunctorPt PrepareCallExtraArgsInList(Pt t, Pt list)
{
	FunctorPt f = PrepareCall(t) ;
	int arity = FunctorArity(f) ;
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) )
		Xc(arity++) = Drf(XListHead(list)) ;
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
	return LookupFunctor(FunctorAtom(f), arity) ;
}

static FunctorPt PrepareCallExtraArgs(register Pt t, int extra)
{
	VarValue(t) ;
redo:
	if( IsStruct(t) ) {
		if( IsUnitParam(t) ) {
			t = Drf(Z(OutParam(XUnitParam(t)))) ;
			goto redo ;
		}
		else {
			register int s = XStructArity(t) ;
			FunctorPt f = LookupFunctor(XStructAtom(t), s + extra) ;
			if( s > 1 ) {
				s += extra ;
				while( extra-- )
					Xc(--s) = Xc(extra + 1) ;
			}
			while( s-- )
				Xc(s) = XStructArg(t,s) ;
			return f ;
		}
	}
	elif( IsAtom(t) ) {
		FunctorPt f = LookupFunctor(XAtom(t), extra) ;
		register int i ;
		dotimes(i, extra)
			Xc(i) = Xc(i + 1) ;
		return f ;
	}
	elif( IsList(t) ) {
		FunctorPt f = LookupFunctor(XAtom(tDotAtom), 2 + extra) ;
		register int s = 2 + extra ;
		while( extra-- )
			Xc(--s) = Xc(extra + 1) ;
		X0 = XListHead(t) ;
		X1 = XListTail(t) ;
		return f ;
	}
	else PrepareCallError(t) ;
	return nil ; /* avoids warning */
}

static void ExecutePred(PredicatePt pr)
{
	ZEnsureFreeSpaceOnStacks(0, nil) ;
	B0 = B ;
	if( Attention() && AttentionHandle(pr) ) return ;
	P = PredCode(pr) ;
}

/* Finalizers ARE TO BE USED ONLY with non-deterministic
   predicates written in C. A finalizer is activated
   when the choice-point of the associated a predicate
   is discarded. This can happen only in the instructions
   Cut, MetaCut, DiscardAndFail and RaiseException. */

void SetupFinalizer(VFunV proc, VoidPt arg)
{
	if( Gt(TR,F-1) ) TrailExpand() ;
	F-- ;
	F->cp = B ;
	F->proc = proc ;
	F->arg = arg ;
}

static void FinalizeUntilHere(ChoicePointPt here)
{
	for( ; Lt(F,trailEnd) && Lt(F->cp,here) ; F++ ) {
		SetChoicePoint(F->cp) ;
		F->proc(F->arg) ; /* Call finalizer */
	}
}



/* PROCEDURAL & CONTROL INSTRUCTIONS */

/* This macro creates an open ended predicate environment which will 
   progressivelly shrink along the predicate code. The current size
   of this environment is determined by the second argument to each
   Call instruction in the predicate code. */

#define ALLOCATE() {						\
	D = cPt(E) ;	/* save curr env */		\
	E = cEnvironmentPt(TopOfLocalStack()) ;	\
	Ef(E) = cEnvironmentPt(D) ;				\
	Ef(CP) = CP ;							\
	Ef(P) = P ;	/* for stacktrace */		\
	Ef(CC) = CC ;							\
	Ef(B0) = B0 ;							\
}

static void NopInst()
{
	JumpNext() ;
}

static void LocalJumpInst()
{
	P = LookHdl() ;
	JumpNext() ;
}

static void AllocateInst()
{
	ALLOCATE()
	JumpNext() ;
}

static void DeallocateInst()
{
	CP = Ef(CP) ;
	CC = Ef(CC) ;
	E = Ef(E) ;
	JumpNext() ;
}

static void EnsureFreeSpaceInst()
{
	static Size n ;
	n = GetWord() ;
	if( LookWord() != 0 ) {
		D = cPt(CP) ;
		CP = P + 1 ;
	}
	ZEnsureFreeSpaceOnStacks(n, nil) ;
	if( LookWord() != 0 )
		CP = cHdl(D) ;
	SkipWord() ;
	JumpNext() ;
}

static void CallInst()
{
	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	ExecutePred(LookPred()) ;
	JumpNext() ;
}

static void ExecuteInst()
{
	ExecutePred(LookPred()) ;
	JumpNext() ;
}

static void CallVarInst()
{
	CP = P + 1 ;	/* Skip argument */
	CC = C ;
	ExecutePred(LookupPredicate(PrepareCall(X0))) ;
	JumpNext() ;
}

static void ExecuteVarInst()
{
	ExecutePred(LookupPredicate(PrepareCall(X0))) ;
	JumpNext() ;
}

static void PApply()
{
	ExecutePred(LookupPredicate(PrepareCallExtraArgsInList(X0, X1))) ;
	JumpNext() ;
}

static void PCallVar1()
{
	ExecutePred(LookupPredicate(PrepareCall(X0))) ;
	JumpNext() ;
}

static void PCallVar2()
{
	ExecutePred(LookupPredicate(PrepareCallExtraArgs(X0, 1))) ;
	JumpNext() ;
}

static void PCallVar3()
{
	ExecutePred(LookupPredicate(PrepareCallExtraArgs(X0, 2))) ;
	JumpNext() ;
}

static void PCallVar4()
{
	ExecutePred(LookupPredicate(PrepareCallExtraArgs(X0, 3))) ;
	JumpNext() ;
}

static void PCallVar5()
{
	ExecutePred(LookupPredicate(PrepareCallExtraArgs(X0, 4))) ;
	JumpNext() ;
}

static void PCallVar6()
{
	ExecutePred(LookupPredicate(PrepareCallExtraArgs(X0, 5))) ;
	JumpNext() ;
}

static void ProceedInst()
{
	P = CP ;
	C = CC ;
	JumpNext() ;
}

static void DeallocProceedInst()
{
	P = CP = Ef(CP) ;
	C = CC = Ef(CC) ;
	E = Ef(E) ;
	JumpNext() ;
}

void CutTo(ChoicePointPt cp)
{
	if( B < cp ) { /* Is current B newer? */
		FinalizeUntilHere(cp) ;
		SetChoicePoint(cp) ;
	}
}

static void CutInst()
{
	if( !DebugCut(Ef(B0)) )
		CutTo(Ef(B0)) ; /* Cut to the B at clause entry */
	JumpNext() ;
}

static void PMetaCut()
{
	if( !DebugCut(cChoicePointPt(X0)) )	
		CutTo(cChoicePointPt(X0)) ; /* Cut to the B at clause entry */
	JumpNext() ;
}

static void PDynamicCut()
{
	Error("Dynamic '!/0' is not supported") ;
	JumpNext() ;
}

static void PutCutLevelInst()
{
	X(GetHdl()) = TagStruct(H) ; /* Inject call to '$$_meta_cut/1' predicate */
	PushH(metaCutFunctor) ;
	PushH(Ef(B0)) ;	/* Sole instance of global stack pointing to local stack */
	JumpNext() ;
}

static void FailInst()
{
	DoFail() ;
}

static void DeletedClauseInst()
{
	DoFail() ;
}

static void UndefInst()
{
	ExecutePred(SearchContextBelow(LookFunctor())) ;
	JumpNext() ;
}

static void NotRunningInst()
{
	FatalError("NotRunning") ;
}



/* PUT INSTRUCTIONS */

static void PutYVariableInst()
{
	static Hdl h ;
	h = &Y(GetWord()) ;
	X(GetHdl()) = ResetVar(h) ;
	JumpNext() ;
}

static void PutXVariableInst()
{
	static Hdl h ;
	h = GetHdl() ;
	X(GetHdl()) = X(h) = PushHVar() ;
	JumpNext() ;
}

static void PutXVariableOneInst()
{
	X(GetHdl()) = PushHVar() ;
	JumpNext() ;
}

static void PutXValueInst()
{
	static Hdl h ;
	h = GetHdl() ;
	X(GetHdl()) = X(h) ;
	JumpNext() ;
}

static void PutYValueInst()
{
	N = GetWord() ;
	X(GetHdl()) = Y(N) ;
	JumpNext() ;
}

static void PutZValueInst()
{
	N = GetWord() ;
	X(GetHdl()) = Z(N) ;
	JumpNext() ;
}

static void PutUnsafeValueInst()
{
	VarValue2(D, Y(GetWord())) ;
	if( IsVar(D) && IsCurrEnvVar(D) ) {
		Assign(D, X(GetHdl()) = PushHVar()) ;
		JumpNext() ;
	}
	else {
		X(GetHdl()) = D ;
		JumpNext() ;
	}
}

static void PutAtomicInst()
{
	static Pt t ;
	t = GetPt() ;
	X(GetHdl()) = t ;
	JumpNext() ;
}

static void PutNilInst()
{
	X(GetHdl()) = tNilAtom ;
	JumpNext() ;
}

static void PutStructureInst()
{
	PushH(GetPt()) ;
	X(GetHdl()) = TagStruct(H-1) ;
	JumpNext() ;
}

static void PutListInst()
{
	X(GetHdl()) = TagList(H) ;
	JumpNext() ;
}



/* GET INSTRUCTIONS */

static void GetYVariableInst()
{
	N = GetWord() ;
	Y(N) = X(GetHdl()) ;
	JumpNext() ;
}

static void GetXValueInst()
{
	static Hdl h ;
	h = GetHdl() ;
	Ensure( Unify(X(GetHdl()), X(h)) ) ;
	VarValue2(D, X(h)) ;
	X(h) = D ;
	JumpNext() ;
}

static void GetYValueInst()
{
	N = GetWord() ;
	MustBe( Unify(Y(N), X(GetHdl())) ) ;
}

static void GetZValueInst()
{
	N = GetWord() ;
	MustBe( Unify(Z(N), X(GetHdl())) ) ;
}

static void GetAtomicInst()
{
	static Pt t ;
	t = GetPt() ;
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, t) ;
		JumpNext() ;
	}
	MustBe( D == t ) ;
}

static void GetNilInst()
{
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, tNilAtom) ;
		JumpNext() ;
	}
	MustBe( D == tNilAtom ) ;
}

static void GetStructureInst()
{
	static Pt t ;
	t = GetPt() ;		/* Get functor */
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, TagStruct(H)) ;
		PushH(t) ;
		writeMode = true ;
		JumpNext() ;
	}
	Ensure( IsThisStruct(D, cFunctorPt(t)) ) ;
	S = XStructArgs(D) ;
	writeMode = false ;
	JumpNext() ;
}

static void GetListInst()
{
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, TagList(H)) ;
		writeMode = true ;
		JumpNext() ;
	}
	Ensure( IsList(D) ) ;
	S = XListArgs(D) ;
	writeMode = false ;
	JumpNext() ;
}



/* UNIFY INSTRUCTIONS */

static void UnifyVoidInst()
{
	if( writeMode ) {
		N = GetWord() ;
		while( N-- )
			Ignore(PushHVar()) ;
		JumpNext() ;
	}
	else {
		S += GetWord() ;
		JumpNext() ;
	}
}

static void UnifyVoidOneInst()
{
    if( writeMode ) {
        Ignore(PushHVar()) ;
        JumpNext() ;
    }
    else {
        S++ ;
        JumpNext() ;
    }
}

static void UnifyXVariableInst()
{
	if( writeMode ) {
		X(GetHdl()) = PushHVar() ;
		JumpNext() ;
	}
	else {
		X(GetHdl()) = *S++ ;
		JumpNext() ;
	}
}

static void UnifyYVariableInst()
{
    if( writeMode ) {
        Y(GetWord()) = PushHVar() ;
        JumpNext() ;
    }
    else {
        Y(GetWord()) = *S++ ;
        JumpNext() ;
    }
}

static void UnifyXValueInst()
{
    static Hdl h ;
    if( writeMode ) {
        PushH(X(GetHdl())) ;
        JumpNext() ;
    }
    else {
        Ensure( Unify(*S++, X(h = GetHdl())) ) ;
        VarValue2(D, X(h)) ;
        X(h) = D ;
        JumpNext() ;
    }
}

static void UnifyYValueInst()
{
	if( writeMode ) {
 		PushH(Y(GetWord())) ;
		JumpNext() ;
	}
	else {
		MustBe( Unify(*S++, Y(GetWord())) ) ;
	}
}

static void UnifyZValueInst() /* @@@ */
{
	if( writeMode ) {
		VarValue2(D, Z(GetWord())) ;
		if( IsVar(D) && IsLocalVar(D) ) {
			Mesg("LOCAL Z") ;
			Assign(D, PushHVar()) ;
			JumpNext() ;
		}
		else {
			PushH(D) ;
			JumpNext() ;
		}
	}
	else
		MustBe( Unify(*S++, Z(GetWord())) ) ;
}

static void UnifyXLocalValueInst()
{
	static Hdl h ;
	if( writeMode ) {
		VarValue2(D, X(h = GetHdl())) ;
		if( IsVar(D) && IsLocalVar(D) ) {
			Assign(D, X(h) = PushHVar()) ;
			JumpNext() ;
		}
		else {
			PushH(X(h) = D) ;
			JumpNext() ;
		}
	}
	else {
		Ensure( Unify(*S++, X(h = GetHdl())) ) ;
		VarValue2(D, X(h)) ;
		X(h) = D ;
		JumpNext() ;
	}
}

static void UnifyYLocalValueInst()
{
	if( writeMode ) {
		VarValue2(D, Y(GetWord())) ;
		if( IsVar(D) && IsLocalVar(D) ) {
			Assign(D, PushHVar()) ;
			JumpNext() ;
		}
		else {
			PushH(D) ;
			JumpNext() ;
		}
	}
	else
		MustBe( Unify(*S++, Y(GetWord())) ) ;
}

static void UnifyAtomicInst()
{
	static Pt t ;
	if( writeMode ) {
		PushH(GetPt()) ;
		JumpNext() ;
	}
	else {
		t = GetPt() ;
		VarValue2(D, *S++) ;
		if( IsVar(D) ) {
			Assign(D, t) ;
			JumpNext() ;
		}
		MustBe( D == t ) ;
	}
}

static void UnifyNilInst()
{
	if( writeMode ) {
		PushH(tNilAtom) ;
		JumpNext() ;
	}
	else {
		VarValue2(D, *S++) ;
		if( IsVar(D) ) {
			Assign(D, tNilAtom) ;
			JumpNext() ;
		}
		MustBe( D == tNilAtom ) ;
	}
}



/* BUILD INSTRUCTIONS */

static void BuildVoidInst()
{
	N = GetWord() ;
	while( N-- )
		Ignore(PushHVar()) ;
	JumpNext() ;
}

static void BuildVoidOneInst()
{
	Ignore(PushHVar()) ;
	JumpNext() ;
}

static void BuildXVariableInst()
{
	X(GetHdl()) = PushHVar() ;
	JumpNext() ;
}

static void BuildYVariableInst()
{
	Y(GetWord()) = PushHVar() ;
	JumpNext() ;
}

static void BuildXValueInst()
{
	PushH(X(GetHdl())) ;
	JumpNext() ;
}

static void BuildYValueInst()
{
	PushH(Y(GetWord())) ;
	JumpNext() ;
}

static void BuildZValueInst()
{
	PushH(Z(GetWord())) ;
	JumpNext() ;
}

static void BuildXLocalValueInst()
{
	static Hdl h ;
	VarValue2(D, X(h = GetHdl())) ;
	if( IsVar(D) && IsLocalVar(D) ) {
		Assign(D, X(h) = PushHVar()) ;
		JumpNext() ;
	}
	else {
		PushH(X(h) = D) ;
		JumpNext() ;
	}
}

static void BuildYLocalValueInst()
{
	VarValue2(D, Y(GetWord())) ;
	if( IsVar(D) && IsLocalVar(D) ) {
		Assign(D, PushHVar()) ;
		JumpNext() ;
	}
	else {
		PushH(D) ;
		JumpNext() ;
	}
}

static void BuildAtomicInst()
{
	PushH(GetPt()) ;
	JumpNext() ;
}

static void BuildNilInst()
{
	PushH(tNilAtom) ;
	JumpNext() ;
}



/* CONTEXT INSTRUCTIONS */

static void PCtxExtension()
{
	U = TermToUnit(X0, H) ;
	PushH(U) ;	/* Unit is ref stored in the global stack */
	PushH(X0) ; /* Link unit-term in the context list */
	PushH(C) ;
	C = TagList(H-2) ;
#if 0
	if( UnitIsAnonymous(cUnitPt(H[-3])) )
		CompileClauseList(X0, true, true) ;
#endif	
	ExecutePred(SearchContext(PrepareCall(X1))) ;
	JumpNext() ;
}

static void PCtxSwitch()
{
	U = TermToUnit(X0, H) ;
	PushH(U) ;	/* Unit is ref stored in the global stack */
	PushH(X0) ; /* Link unit-term in the context list */
	if( C == tNilAtom )
		Error("Cannot switch top of empty context") ;
	PushH(XListTail(C)) ;
	C = TagList(H-2) ;
#if 0
	if( UnitIsAnonymous(cUnitPt(H[-3])) )
		CompileClauseList(X0, true, true) ;
#endif
	ExecutePred(SearchContext(PrepareCall(X1))) ;
	JumpNext() ;
}

static void ImportInst()
{
	D = ZPushTerm_ConvUnitParams(P[0]) ;
	U = TermToUnit(D, H) ;
	PushH(U) ;	/* Unit is ref stored in the global stack */
	PushH(D) ; /* Link unit-term in the context list */
	if( C == tNilAtom )
		Error("Cannot switch top of emtpy context") ;
	PushH(XListTail(C)) ;
	C = TagList(H-2) ;
#if 0
	if( UnitIsAnonymous(cUnitPt(H[-3])) )
		CompileClauseList(X0, true, true) ;
#endif
	ExecutePred(SearchContext(cFunctorPt(P[5]))) ;
	JumpNext() ;
}

static void PCtxEmpty()
{
	C = tNilAtom ;
	ExecutePred(SearchContext(PrepareCall(X0))) ;
	JumpNext() ;
}

static void PCtxDown()
{
	if( C == tNilAtom )
		Error("Cannot use 'down/1' on the emtpy context") ;
	C = XListTail(C) ;
	ExecutePred(SearchContext(PrepareCall(X0))) ;
	JumpNext() ;
}

static void HCtxPopAux()
{
	CH = XListTail(CH) ;
	Jump(DeallocProceed) ;
}
static Pt HCtxPushCode[2] =
{ 
	cPt(WordsOf(Environment)),
	cPt(HCtxPopAux)
} ;
static void PHCtxPush()
{
	ALLOCATE()
	PushH(C) ;
	PushH(CH) ;
	CH = TagList(H-2) ;
	CP = HCtxPushCode + 1 ;
	ExecutePred(LookupPredicate(PrepareCall(X0))) ;
	JumpNext() ;
}

static void HCtxExitAux()
{
	CH = Y(OutPerm(0)) ;
	Jump(DeallocProceed) ;
}
static Pt HCtxEnterCode[2] =
{
	cPt(WordsOf(Environment)+1),
	cPt(HCtxExitAux)
} ;
static void PHCtxEnter()
{
	ALLOCATE()
	if( CH == tNilAtom )
		Error("Empty historic context: cannot enter") ;
	Y(OutPerm(0)) = CH ;
	C = XListHead(CH) ;
	CH = XListTail(CH) ;
	CP = HCtxEnterCode + 1 ;
	ExecutePred(SearchContext(PrepareCall(X0))) ;
	JumpNext() ;
}



/* INDEXING INSTRUCTIONS */

static void MakeIndexInst()
{
	DoIndex(LookPred()) ;
	P-- ;
	JumpNext() ;
}

static void TryMeElseInst()
{
	static Hdl h ;
	static int n ;
	h = ClauseCode(GetClause()) ;
	n = GetWord() ;
	D = cPt(B) ;	/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - n) - 1 ;
	SaveState(cChoicePointPt(D), h, n) ;
	JumpNext() ;
}

static void RetryMeElseInst()
{
	Bf(P) = ClauseCode(GetClause()) ;
	RestoreState(GetWord()) ;
	JumpNext() ;
}

static void TrustMeInst()
{
	SkipWord() ;
	RestoreState(GetWord()) ;
	SetChoicePoint(Bf(B)) ;
	JumpNext() ;
}

static void TryInst()
{	
	static Hdl h ;
	h = GetHdl() ;
	D = cPt(B) ;	/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - cWord(h[-1])) - 1 ;
	SaveState(cChoicePointPt(D), P, cWord(h[-1])) ;
	P = h ;
	JumpNext() ;
}

static void RetryInst()
{
	Bf(P) = P + 1 ;
	P = LookHdl() ;
	RestoreState(cWord(P[-1])) ;
	JumpNext() ;
}

static void TrustInst()
{
	P = LookHdl() ;
	RestoreState(cWord(P[-1])) ;
	SetChoicePoint(Bf(B)) ;
	JumpNext() ;
}

static void DoSwitchOnTerm()
{
	VarValue(D) ;
	if( IsList(D) )
		P = cHdl(P[1]) ;
	elif( IsAtomic(D) )
		P = cHdl(P[0]) ;
	elif( IsVar(D) )
		P = cHdl(P[3]) ;
	else
		P = cHdl(P[2]) ;
}

static void SwitchOnTerm0Inst()
{
	D = X0 ;
	DoSwitchOnTerm() ;
	JumpNext() ;
}

static void SwitchOnTerm1Inst()
{
	D = X1 ;
	DoSwitchOnTerm() ;
	JumpNext() ;
}

static void SwitchOnTerm2Inst()
{
	D = X2 ;
	DoSwitchOnTerm() ;
	JumpNext() ;
}

static void SwitchOnAtomicInst()
{
	register PrologHashTable ht, ht1 ;
	/* Got here from SwitchOnTerm: D already constains Drf(Xi) */
	ht = (PrologHashTable)(P+2) ;
	doseq(ht1, ht + PrologHash(D, cWord(P[0])), ht1->next)
		if( ht1->value == D ) {
			P = ht1->address ;
			JumpNext() ;
		}
	P = cHdl(P[1]) ;
	JumpNext() ;
}

static void SwitchOnStructureInst()
{
	/* Got here from SwitchOnTerm: D already constains Drf(X?) */
	register PrologHashTable ht, ht1 ;
	D = cPt(XStructFunctor(D)) ;
	ht = (PrologHashTable)(P+2) ;
	doseq(ht1, ht + PrologHash(D, cWord(P[0])), ht1->next)
		if( ht1->value == D ) {
			P = ht1->address ;
			JumpNext() ;
		}
	P = cHdl(P[1]) ;
	JumpNext() ;
}

static void DiscardAndFailInst()
{	/* pre: B is the choice point of a non-deterministic C predicate */
	Discard() ;
	DoFail() ;
}



/* DEBUGGING INSTRUCTIONS */

static void DebugExitInst()
{
	DebugExitCode() ;
	JumpNext() ;
}

static void DebugRedoInst()
{
	DebugRedoCode() ;
	JumpNext() ;
}

static void DebugRetryInst()
{
	DebugRetryCode() ;
	JumpNext() ;
}



/* INSTRUCTIONS */

#define maxInstructions		100

Pt	Nop, FAllocate, FDeallocate, Proceed, DeallocProceed, LocalJump,
	Call, EnsureFreeSpace, Execute, CallVar, ExecuteVar,
	Cut, PutCutLevel, Fail, DeletedClause, Undef, NotRunning,

	GetYVariable, GetXValue, GetYValue, GetZValue, GetAtomic, GetNil,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,

	PutXValue, PutYValue, PutZValue, PutUnsafeValue, PutAtomic, PutNil, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,

	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue, UnifyZValue,
	UnifyAtomic, UnifyNil, BuildVoid, BuildVoidOne, BuildXVariable,

	BuildYVariable, BuildXValue, BuildYValue, BuildZValue, BuildXLocalValue,
	BuildYLocalValue, BuildAtomic, BuildNil,

	Import,

	MakeIndex, TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust,
	SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
	SwitchOnAtomic, SwitchOnStructure, DiscardAndFail,
	
	DebugExit, DebugRedo, DebugRetry,

	FirstInst, LastInst ;

static struct instInfo
{
	Pt inst ;
	CharPt name, types ;
} insts[maxInstructions] ;
static int nInsts = 0 ;

static void II(Pt *inst, VFun proc, CharPt name, CharPt types)
{
	insts[nInsts].inst = InstEncode(proc) ;
	if( inst != nil ) *inst = insts[nInsts].inst ;
	insts[nInsts].name = name ;
	insts[nInsts].types = types ;
	if( Lt(insts[nInsts].inst, FirstInst) ) FirstInst = insts[nInsts].inst ;
	if( Gt(insts[nInsts].inst, LastInst) ) LastInst = insts[nInsts].inst ;
	if( ++nInsts == maxInstructions )
		InternalError("Too many instructions") ;
}

void InstructionsInit()
{
	FirstInst = InstEncode(NopInst) ;
	LastInst = InstEncode(NopInst) ;

/* PROCEDURAL & CONTROL INSTRUCTIONS */
	II(&Nop,			NopInst,				"Nop",				"") ;
	II(&FAllocate,		AllocateInst,			"Allocate",			"") ;
	II(&FDeallocate,	DeallocateInst,			"Deallocate",		"") ;
	II(&Proceed,		ProceedInst,			"Proceed",			"@") ;
	II(&DeallocProceed, DeallocProceedInst,		"DeallocProceed",	"@") ;
	II(&LocalJump,		LocalJumpInst,			"LocalJump",		"l@") ;
	II(&EnsureFreeSpace,EnsureFreeSpaceInst,	"EnsureFreeSpace",	"ne") ;
	II(&Call,			CallInst,				"Call",				"pe") ;
	II(&Execute,		ExecuteInst,			"Execute",			"p@") ;
	II(&CallVar,		CallVarInst,			"CallVar",			"e") ;
	II(&ExecuteVar,		ExecuteVarInst,			"ExecuteVar",		"@") ;

	II(&Cut,			CutInst,				"Cut",				"") ;
//	II(&MetaCut,		PMetaCut,				"MetaCut",			"") ;
//	II(&DynamicCut,		PDynamicCut,			"DynamicCut",		"") ;
	II(&PutCutLevel,	PutCutLevelInst,		"PutCutLevel",		"x") ;

	II(&Fail,			FailInst,				"Fail",				"") ;
	II(&DeletedClause,	DeletedClauseInst,		"DeletedClause",	"@") ;
	II(&Undef,			UndefInst,				"Undef",			"f@") ;
	II(&NotRunning,		NotRunningInst,			"NotRunning",		"") ;

/* GET INSTRUCTIONS */
	II(&GetYVariable,	GetYVariableInst,		"GetYVariable",		"yx") ;
	II(&GetXValue,		GetXValueInst,			"GetXValue",		"xx") ;
	II(&GetYValue,		GetYValueInst,			"GetYValue",		"yx") ;
	II(&GetZValue,		GetZValueInst,			"GetZValue",		"zx") ;
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
	II(&PutZValue,		PutZValueInst,			"PutZValue",		"zx") ;
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
	II(&UnifyZValue,	UnifyZValueInst,		"UnifyZValue",		"z"	) ;
	II(&UnifyAtomic,	UnifyAtomicInst,		"UnifyAtomic",		"t") ;
	II(&UnifyNil,		UnifyNilInst,			"UnifyNil",			"") ;

/* BUILD INSTRUCTIONS */
	II(&BuildVoid,		BuildVoidInst,			"BuildVoid",		"n") ;
	II(&BuildVoidOne,	BuildVoidOneInst,		"BuildVoidOne",		"") ;
	II(&BuildXVariable,	BuildXVariableInst,		"BuildXVariable",	"x"	) ;
	II(&BuildYVariable,	BuildYVariableInst,		"BuildYVariable",	"y"	) ;
	II(&BuildXValue,	BuildXValueInst,		"BuildXValue",		"x") ;
	II(&BuildYValue,	BuildYValueInst,		"BuildYValue",		"y") ;
	II(&BuildZValue,	BuildZValueInst,		"BuildZValue",		"z") ;
	II(&BuildXLocalValue,BuildXLocalValueInst,	"BuildXLocalValue",	"x"	) ;
	II(&BuildYLocalValue,BuildYLocalValueInst,	"BuildYLocalValue",	"y"	) ;
	II(&BuildAtomic,	BuildAtomicInst,		"BuildAtomic",		"t") ;
	II(&BuildNil,		BuildNilInst,			"BuildNil",			"") ;

/* CONTEXT INSTRUCTIONS */
//	II(&CtxExtension,	PCtxExtension,			"CtxExtension",		"") ;
//	II(&CtxSwitch,		PCtxSwitch,				"CtxSwitch",		"") ;
	II(&Import,			ImportInst,				"Import",			"t@") ;
//	II(&CtxEmpty,		PCtxEmpty,				"CtxEmpty",			"") ;
//	II(&CtxDown,		PCtxDown,				"CtxDown",			"") ;	
//	II(&HCtxPush,		PHCtxPush,				"HCtxPush",			"") ;
//	II(&HCtxEnter,		PHCtxEnter,				"HCtxEnter",		"") ;

/* INDEXING INSTRUCTIONS */
	II(&MakeIndex,		MakeIndexInst,			"MakeIndex",		"p@") ;
	II(&TryMeElse,		TryMeElseInst,			"TryMeElse",		"cn") ;
	II(&RetryMeElse,	RetryMeElseInst,		"RetryMeElse",		"cn") ;
	II(&TrustMe,		TrustMeInst,			"TrustMe",			".n") ;
	II(&Try,			TryInst,				"Try",				"l") ;
	II(&Retry,			RetryInst,				"Retry",			"l"	) ;
	II(&Trust,			TrustInst,				"Trust",			"l"	) ;
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

CharPt GetInstInfo(Pt inst, CharPt *types)
{
	register int i ;
	PredicatePt pr ;
	dotimes(i, nInsts)
		if( insts[i].inst == inst ) {
			if( types != nil )
				*types = insts[i].types ;
			return insts[i].name ;
		}
	if( types != nil ) *types = "" ;
	if( (pr = FindCPredByInst(inst)) != nil )
		return PredNameArity(pr) ;
	else return "UNKNOWN INSTRUCTION" ;
}

void UserModeInstructions()
{
}

void InstructionsInit2()
{
	InstallCBuiltinPred("!", 0, PDynamicCut) ;
	InstallCBuiltinPred("$$_meta_cut", 1, PMetaCut) ;

	InstallCBuiltinPred("apply", 2, PApply) ;
	InstallCBuiltinPred("call", 1, PCallVar1) ;
	InstallCBuiltinPred("call", 2, PCallVar2) ;
	InstallCBuiltinPred("call", 3, PCallVar3) ;
	InstallCBuiltinPred("call", 4, PCallVar4) ;
	InstallCBuiltinPred("call", 5, PCallVar5) ;
	InstallCBuiltinPred("call", 6, PCallVar6) ;

	InstallCBuiltinPred(">>", 2, PCtxExtension) ;
	InstallCBuiltinPred("<>", 2, PCtxSwitch) ;
	InstallCBuiltinPred("call_on_empty_context", 1, PCtxEmpty) ;
	InstallCBuiltinPred("down", 1, PCtxDown) ;
	InstallCBuiltinPred(">", 1, PHCtxPush) ;
	InstallCBuiltinPred("<", 1, PHCtxEnter) ;
}
