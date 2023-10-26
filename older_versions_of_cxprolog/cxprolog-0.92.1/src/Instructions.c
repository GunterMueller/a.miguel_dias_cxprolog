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
#define LookClause()	cClausePt(*P)
#define GetClause()		cClausePt(*P++)
#define GetWord()		cWord(*P++)
#define LookWord()		cWord(*P)
#define SkipWord()		(P++)
#define LookPred()		cPredicatePt(*P)
#define GetFunctor()	cFunctorPt(*P++)
#define LookFunctor()	cFunctorPt(*P)



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
   Call instruction in the predicate body code. */

#define ALLOCATE() Do(						\
	Q.e = E ;	/* save curr env */			\
	E = cEnvironmentPt(TopOfLocalStack()) ;	\
	Ef(E) = Q.e ;							\
	Ef(CP) = CP ;							\
	Ef(CC) = CC ;							\
	Ef(B0) = B0 ; )

#define DEALLOCATE() Do(					\
	CP = Ef(CP) ;							\
	CC = Ef(CC) ;							\
	E = Ef(E) ; )

static void NopInst()
{
	JumpNext() ;
}

static void LocalJumpInst()
{
	P = LookHdl() ;
	JumpNext() ;
}

static void FAllocateInst()
{
	ALLOCATE() ;
	JumpNext() ;
}

static void EnsureFreeSpaceInst()
{
	Z.w = GetWord() ;
	if( LookPt() != nil ) {	/* hasEnvironment */
		Q.h = CP ;
		CP = P + 1 ;
	}
	ZEnsureFreeSpaceOnStacks(Z.w, nil) ;
	if( LookPt() != nil )	/* hasEnvironment */
		CP = Q.h ;
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

static void DeallocExecuteInst()
{
	DEALLOCATE() ;
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

static void DeallocExecuteVarInst()
{
	DEALLOCATE() ;
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
	DEALLOCATE() ;
	P = CP ;
	C = CC ;
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

static void EmptyPredInst()
{
	ExecutePred(SearchContextBelow(LookFunctor())) ;
	JumpNext() ;
}

static void UndefPredInst()
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
	Q.h = &Y(GetWord()) ;
	X(GetHdl()) = ResetVar(Q.h) ;
	JumpNext() ;
}

static void PutXVariableInst()
{
	Q.h = GetHdl() ;
	X(GetHdl()) = X(Q.h) = PushHVar() ;
	JumpNext() ;
}

static void PutXVariableOneInst()
{
	X(GetHdl()) = PushHVar() ;
	JumpNext() ;
}

static void PutXValueInst()
{
	Q.h = GetHdl() ;
	X(GetHdl()) = X(Q.h) ;
	JumpNext() ;
}

static void PutYValueInst()
{
	Z.w = GetWord() ;
	X(GetHdl()) = Y(Z.w) ;
	JumpNext() ;
}

static void PutZValueInst()
{
	Z.w = GetWord() ;
	X(GetHdl()) = Z(Z.w) ;
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
	Q.t = GetPt() ;
	X(GetHdl()) = Q.t ;
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
	Z.w = GetWord() ;
	Y(Z.w) = X(GetHdl()) ;
	JumpNext() ;
}

static void GetXValueInst()
{
	Q.h = GetHdl() ;
	Ensure( Unify(X(GetHdl()), X(Q.h)) ) ;
	VarValue2(D, X(Q.h)) ;
	X(Q.h) = D ;
	JumpNext() ;
}

static void GetYValueInst()
{
	Z.w = GetWord() ;
	MustBe( Unify(Y(Z.w), X(GetHdl())) ) ;
}

static void GetZValueInst()
{
	Z.w = GetWord() ;
	MustBe( Unify(Z(Z.w), X(GetHdl())) ) ;
}

static void GetAtomicInst()
{
	Q.t = GetPt() ;
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, Q.t) ;
		JumpNext() ;
	}
	MustBe( D == Q.t ) ;
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
	Q.f = GetFunctor() ;		/* Get functor */
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, TagStruct(H)) ;
		PushH(Q.t) ;
		S = nil ;	/* set write mode */
		JumpNext() ;
	}
	Ensure( IsThisStruct(D, Q.f) ) ;
	S = XStructArgs(D) ;
	JumpNext() ;
}

static void GetListInst()
{
	VarValue2(D, X(GetHdl())) ;
	if( IsVar(D) ) {
		Assign(D, TagList(H)) ;
		S = nil ;	/* set write mode */
		JumpNext() ;
	}
	Ensure( IsList(D) ) ;
	S = XListArgs(D) ;
	JumpNext() ;
}



/* UNIFY INSTRUCTIONS */

static void UnifyVoidInst()
{
	if( S == nil ) { /* write more */
		Z.w = GetWord() ;
		while( Z.w-- )
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
    if( S == nil ) { /* write more */
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
	if( S == nil ) { /* write more */
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
    if( S == nil ) { /* write more */
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
    if( S == nil ) { /* write more */
        PushH(X(GetHdl())) ;
        JumpNext() ;
    }
    else {
        Ensure( Unify(*S++, X(Q.h = GetHdl())) ) ;
        VarValue2(D, X(Q.h)) ;
        X(Q.h) = D ;
        JumpNext() ;
    }
}

static void UnifyYValueInst()
{
	if( S == nil ) { /* write more */
 		PushH(Y(GetWord())) ;
		JumpNext() ;
	}
	else {
		MustBe( Unify(*S++, Y(GetWord())) ) ;
	}
}

static void UnifyZValueInst() /* @@@ */
{
	if( S == nil ) { /* write more */
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
	if( S == nil ) { /* write more */
		VarValue2(D, X(Q.h = GetHdl())) ;
		if( IsVar(D) && IsLocalVar(D) ) {
			Assign(D, X(Q.h) = PushHVar()) ;
			JumpNext() ;
		}
		else {
			PushH(X(Q.h) = D) ;
			JumpNext() ;
		}
	}
	else {
		Ensure( Unify(*S++, X(Q.h = GetHdl())) ) ;
		VarValue2(D, X(Q.h)) ;
		X(Q.h) = D ;
		JumpNext() ;
	}
}

static void UnifyYLocalValueInst()
{
	if( S == nil ) { /* write more */
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
	if( S == nil ) { /* write more */
		PushH(GetPt()) ;
		JumpNext() ;
	}
	else {
		Q.t = GetPt() ;
		VarValue2(D, *S++) ;
		if( IsVar(D) ) {
			Assign(D, Q.t) ;
			JumpNext() ;
		}
		MustBe( D == Q.t ) ;
	}
}

static void UnifyNilInst()
{
	if( S == nil ) { /* write more */
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
	Z.w = GetWord() ;
	while( Z.w-- )
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
	VarValue2(D, X(Q.h = GetHdl())) ;
	if( IsVar(D) && IsLocalVar(D) ) {
		Assign(D, X(Q.h) = PushHVar()) ;
		JumpNext() ;
	}
	else {
		PushH(X(Q.h) = D) ;
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
	Q.u = TermToUnit(X0, H) ;
	PushH(Q.u) ;	/* Unit is ref stored in the global stack */
	PushH(X0) ;		/* Link unit-term in the context list */
	PushH(C) ;
	C = TagList(H-2) ;
#if 0
	if( UnitIsAnonymous(cUnitPt(H[-3])) )
		SetClauseList(X0, true, true) ;
#endif	
	ExecutePred(SearchContext(PrepareCall(X1))) ;
	JumpNext() ;
}

static void PCtxSwitch()
{
	Q.u = TermToUnit(X0, H) ;
	PushH(Q.u) ;	/* Unit is ref stored in the global stack */
	PushH(X0) ;		/* Link unit-term in the context list */
	if( C == tNilAtom )
		Error("Cannot switch top of empty context") ;
	PushH(XListTail(C)) ;
	C = TagList(H-2) ;
#if 0
	if( UnitIsAnonymous(cUnitPt(H[-3])) )
		SetClauseList(X0, true, true) ;
#endif
	ExecutePred(SearchContext(PrepareCall(X1))) ;
	JumpNext() ;
}

static void ImportInst()
{
	D = ZPushTerm_ConvUnitParams(P[0]) ;
	Q.u = TermToUnit(D, H) ;
	PushH(Q.u) ;	/* Unit is ref stored in the global stack */
	PushH(D) ;		/* Link unit-term in the context list */
	if( C == tNilAtom )
		Error("Cannot switch top of emtpy context") ;
	PushH(XListTail(C)) ;
	C = TagList(H-2) ;
#if 0
	if( UnitIsAnonymous(cUnitPt(H[-3])) )
		SetClauseList(X0, true, true) ;
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
	cPt(WordsOf(Environment)), /* zero local-var env */
	cPt(HCtxPopAux)
} ;
static void PHCtxPush()
{
	ALLOCATE() ;
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
	cPt(WordsOf(Environment)+1), /* one local-var env */
	cPt(HCtxExitAux)
} ;
static void PHCtxEnter()
{
	ALLOCATE() ;
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
	Q.h = ClauseCode(GetClause()) ;
	Z.w = GetWord() ;
	D = cPt(B) ;	/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - Z.w) - 1 ;
	SaveState(cChoicePointPt(D), Q.h, Z.w) ;
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
	Q.h = GetHdl() ;
	D = cPt(B) ;	/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - Q.t[-1]) - 1 ;
	SaveState(cChoicePointPt(D), P, Q.t[-1]) ;
	P = Q.h ;
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

#define ClauseMatchThis(cl, t)				\
   ( IsVar(ClauseIdxInfo(cl, 0)) || ClauseIdxInfo(cl, 0) == (t) )

#define ClauseMatchStruct(cl)				\
   ( IsVar(ClauseIdxInfo(cl, 0)) || IsStruct(ClauseIdxInfo(cl, 0)) )

#define ClauseMatchAtomic(cl)				\
   ( IsVar(ClauseIdxInfo(cl, 0)) || IsAtomic(ClauseIdxInfo(cl, 0)) )

#define ClauseAliveMatchThis(cl, t)			\
	( ClauseIsAlive(cl)	&& ClauseMatchThis(cl, t) )

#define ClauseAliveMatchStruct(cl)			\
	( ClauseIsAlive(cl)	&& ClauseMatchStruct(cl) )

#define ClauseAliveMatchAtomic(cl)			\
	( ClauseIsAlive(cl) && ClauseMatchAtomic(cl) )

#define ClauseAlive2MatchThis(cl, clock, t)	\
	( ClauseIsAlive2(cl, clock)	&& ClauseMatchThis(cl, t) )

static void DynamicEnterInst() /* Logical update semantic view */
{
/* First clause always alive */
	Q.c = GetClause() ;
	Z.w = LookWord() ;
	P = ClauseCodeSkipHeader(Q.c) ;
/* Search second clause alive */
	for( Q.c = ClauseNext(Q.c) ; Q.c != nil ; Q.c = ClauseNext(Q.c) ) {
		if( ClauseIsAlive(Q.c) ) { /* There is a second clause */
			D = cPt(B) ;	/* save current choice point */
			B = cChoicePointPt(TopOfLocalStack() - Z.w - 1) - 1 ; /* extra A() */
			SaveState(cChoicePointPt(D), ClauseCode(Q.c), Z.w) ;
			A(Z.w) = GlobalClock ;	/* Store call-clock in the extra A() */
			JumpNext() ;
		}
	}
	JumpNext() ;
}

static void DynamicElseInst() /* Logical update semantic view */
{
	Q.c = GetClause() ;
	Z.w = GetWord() ;
	RestoreState(Z.w) ;
/* Search next clause alive for the call */
	for( ; Q.c != nil ; Q.c = ClauseNext(Q.c) ) {
		if( ClauseIsAlive2(Q.c, A(Z.w)) ) {
			Bf(P) = ClauseCode(Q.c) ;	/* Setup next alternative */
			JumpNext() ;
		}
	}
	SetChoicePoint(Bf(B)) ;	/* Discard */
	JumpNext() ;
}

static void DynamicEnterIndexedInst() /* Logical update semantic view */
{
	VarValue2(D, X0) ;
	X0 = D ;		/* Necessary for DynamicElseIndexedInst */
	if( IsVar(D) ) {
	/* First clause always alive */
		Q.c = cClausePt(P[3]) ;
		P = ClauseCodeSkipHeader(Q.c) ;
	/* Search second clause alive */
		for( Q.c = ClauseNext(Q.c) ; Q.c != nil ; Q.c = ClauseNext(Q.c) )
			if( ClauseIsAlive(Q.c) ) goto hasSecondClause ;
		JumpNext() ;
	}
	elif( IsStruct(D) ) {
		D = TagStruct(XStructFunctor(D)) ;
		if( P[2] == nil ) {
		/* Search first var or struct clause */
			for( Q.c = cClausePt(P[3]) ; ; Q.c = ClauseNext(Q.c) ) {
				if( Q.c == nil ) { Q.c = failClause ; break ; }
				if( ClauseAliveMatchStruct(Q.c) ) break ;
			}
			P[2] = cPt(Q.c) ;
		}
		else Q.c = cClausePt(P[2]) ;
		for( ; ; Q.c = ClauseNext(Q.c) ) {
			if( Q.c == nil ) DoFail() ;
			if( ClauseAliveMatchThis(Q.c, D) ) break ;
		}
	}
	elif( IsList(D) ) {
		D = TagList(nil) ;
		if( P[1] == nil ) {
		/* Search first clause var or list */
			for( Q.c = cClausePt(P[3]) ; ; Q.c = ClauseNext(Q.c) ) {
				if( Q.c == nil ) { Q.c = failClause ; break ; }
				if( ClauseAliveMatchThis(Q.c, D) ) break ;
			}
			P[1] = cPt(Q.c) ;
		}
		else Q.c = cClausePt(P[1]) ;
	}
	else {
		if( P[0] == nil ) {
		/* Search first var or atomic clause */
			for( Q.c = cClausePt(P[3]) ; ; Q.c = ClauseNext(Q.c) ) {
				if( Q.c == nil ) { Q.c = failClause ; break ; }
				if( ClauseAliveMatchAtomic(Q.c) ) break ;
			}
			P[0] = cPt(Q.c) ;
		}
		else Q.c = cClausePt(P[0]) ;
		for( ; ; Q.c = ClauseNext(Q.c) ) {
			if( Q.c == nil ) DoFail() ;
			if( ClauseAliveMatchThis(Q.c, D) ) break ;
		}
	}
	P = ClauseCodeSkipHeader(Q.c) ;	
/* Search second clause relevant */
	for( Q.c = ClauseNext(Q.c) ; Q.c != nil ; Q.c = ClauseNext(Q.c) )
		if( ClauseAliveMatchThis(Q.c, D) ) goto hasSecondClause ;
	JumpNext() ;

hasSecondClause:
	Z.w = ClauseArity(Q.c) ;
	D = cPt(B) ;	/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - Z.w - 1) - 1 ; /* extra A() */
	SaveState(cChoicePointPt(D), ClauseCode(Q.c), Z.w) ;
	A(Z.w) = GlobalClock ;	/* Store call-clock in the extra A() */
	JumpNext() ;
}

static void DynamicElseIndexedInst() /* Logical update semantic view */
{
	Q.c = GetClause() ;
	Z.w = GetWord() ;
	RestoreState(Z.w) ;
	if( IsVar(X0) ) {
/* Search next clause alive for the call */
		for( ; Q.c != nil ; Q.c = ClauseNext(Q.c) )
			if( ClauseIsAlive2(Q.c, A(Z.w)) ) {
				Bf(P) = ClauseCode(Q.c) ;	/* Setup next alternative */
				JumpNext() ;
			}
	}
	else {
		D = IsStruct(X0) ? TagStruct(XStructFunctor(X0))
		  : IsList(X0) ? TagList(nil)
		  : X0 ;
/* Search next clause relevant for the call */
		for( ; Q.c != nil ; Q.c = ClauseNext(Q.c) )
			if( ClauseAlive2MatchThis(Q.c, A(Z.w), D) ) {
				Bf(P) = ClauseCode(Q.c) ;	/* Setup next alternative */
				JumpNext() ;
			}
	}
/* Discard */
	SetChoicePoint(Bf(B)) ;
	JumpNext() ;
}

static void DynamicIUEnterInst() /* Immediate update semantic view */
{
	Q.c = GetClause() ;
	Z.w = LookWord() ;
	D = cPt(B) ;	/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - Z.w) - 1 ;
	SaveState(cChoicePointPt(D), ClauseCode(Q.c), Z.w) ;
	P = ClauseCodeSkipHeader(Q.c) ;
	JumpNext() ;
}

static void DynamicIUElseInst() /* Immediate update semantic view */
{
	Q.c = GetClause() ;
	if( Q.c == nil )  {
		SetChoicePoint(Bf(B)) ;	/* Discard */
		DoFail() ;
	}
	else {
		Bf(P) = ClauseCode(Q.c) ;	/* Setup next alternative */
		RestoreState(LookWord()) ;
		P = ClauseCodeSkipHeader(Q.c) ;
		JumpNext() ;
	}
}

static void DynamicIUEnterIndexedInst() /* Immediate update semantic view */
{
	VarValue2(D, X0) ;
	X0 = D ;		/* Necessary for DynamicElseIndexedInst */
	if( IsVar(D) )
		Q.c = cClausePt(P[3]) ;
	elif( IsStruct(D) ) {
		D = TagStruct(XStructFunctor(D)) ;
		if( P[2] == nil ) {
		/* Search first var or struct clause */
			for( Q.c = cClausePt(P[3]) ; ; Q.c = ClauseNext(Q.c) ) {
				if( Q.c == nil ) { Q.c = failClause ; break ; }
				if( ClauseMatchStruct(Q.c) ) break ;
			}
			P[2] = cPt(Q.c) ;
		}
		else Q.c = cClausePt(P[2]) ;
		for( ; ; Q.c = ClauseNext(Q.c) ) {
			if( Q.c == nil ) DoFail() ;
			if( ClauseMatchThis(Q.c, D) ) break ;
		}
	}
	elif( IsList(D) ) {
		D = TagList(nil) ;
		if( P[1] == nil ) {
		/* Search first clause var or list */
			for( Q.c = cClausePt(P[3]) ; ; Q.c = ClauseNext(Q.c) ) {
				if( Q.c == nil ) { Q.c = failClause ; break ; }
				if( ClauseMatchThis(Q.c, D) ) break ;
			}
			P[1] = cPt(Q.c) ;
		}
		else Q.c = cClausePt(P[1]) ;
		if( Q.c == failClause ) DoFail() ;
	}
	else {
		if( P[0] == nil ) {
		/* Search first var or atomic clause */
			for( Q.c = cClausePt(P[3]) ; ; Q.c = ClauseNext(Q.c) ) {
				if( Q.c == nil ) { Q.c = failClause ; break ; }
				if( ClauseMatchAtomic(Q.c) ) break ;
			}
			P[0] = cPt(Q.c) ;
		}
		else Q.c = cClausePt(P[0]) ;
		for( ; ; Q.c = ClauseNext(Q.c) ) {
			if( Q.c == nil ) DoFail() ;
			if( ClauseMatchThis(Q.c, D) ) break ;
		}
	}
	Z.w = ClauseArity(Q.c) ;
	D = cPt(B) ;	/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - Z.w) - 1 ;
	SaveState(cChoicePointPt(D), ClauseCode(Q.c), Z.w) ;
	P = ClauseCodeSkipHeader(Q.c) ;
	JumpNext() ;
}

static void DynamicIUElseIndexedInst() /* Immediate update semantic view */
{
	Q.c = GetClause() ;
	if( Q.c == nil )  {
		SetChoicePoint(Bf(B)) ;	/* Discard */
		DoFail() ;
	}
	else {
		D = A(0) ;
		if( !IsVar(D) ) {
			D = IsStruct(D) ? TagStruct(XStructFunctor(D))
			  : IsList(D) ? TagList(nil)
			  : D ;
			for( ; Q.c != nil ; Q.c = ClauseNext(Q.c) )
				if( ClauseMatchThis(Q.c, D) )
					goto setupNextAlternative ;
			SetChoicePoint(Bf(B)) ;	/* Discard */
			DoFail() ;
		}
	}
	
setupNextAlternative:
	Bf(P) = ClauseCode(Q.c) ;	/* Setup next alternative */
	RestoreState(LookWord()) ;
	P = ClauseCodeSkipHeader(Q.c) ;
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

Inst
#define InstInfo(inst, args)	inst
#include "InstructionsInfo.h"
#undef InstInfo
;
Inst FirstInst, LastInst ;

typedef struct {
	Hdl instAddr ;
	Inst inst ;
	CharPt name, types ;
} InstInfo, *InstInfoPt ;

static InstInfo insts[] = {
#define InstInfo(inst,args)	{ &inst, InstEncode(inst##Inst), #inst, args }
#include "InstructionsInfo.h"
#undef InstInfo
	, {nil}
} ;

void InstructionsInit()
{
	register InstInfoPt pt ;
	FirstInst = InstEncode(NopInst) ;
	LastInst = InstEncode(NopInst) ;
	for( pt = insts ; pt->instAddr != nil ; pt++ ) {
		*pt->instAddr = pt->inst ;
		if( Lt(pt->inst, FirstInst) ) FirstInst = pt->inst ;
		if( Gt(pt->inst, LastInst) ) LastInst = pt->inst ;
	}
}

CharPt GetInstInfo(Inst inst, CharPt *types)
{
	register InstInfoPt pt ;
	PredicatePt pr ;
	for( pt = insts ; pt->instAddr != nil ; pt++ )
		if( pt->inst == inst ) {
			if( types != nil )
				*types = pt->types ;
			return pt->name ;
		}
	if( types != nil ) *types = "" ;
	if( (pr = FindCPredByInst(inst)) != nil )
		return PredNameArity(pr) ;
	else return "UNKNOWN INSTRUCTION" ;
}

CharPt GetInstInfoSearch(InstPt code, CharPt *types)
{
	CharPt s ; 
	int i = 4 ;
	do {
		s = GetInstInfo(*code--, types) ;
	} while( s[0] == 'U' && --i > 0 ) ;
	return s ;
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
