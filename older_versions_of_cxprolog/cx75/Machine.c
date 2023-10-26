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
#define GetInt()		cInt(*P++)
#define LookPred()		cPredicatePt(*P)
#define LookFunctor()	cFunctorPt(*P)
#define SkipInst()		(P++)


/* MACHINE REGISTERS */

Hdl P, CP, H, HB, TR ;
EnvironmentPt E ;
ChoicePointPt B ;
Pt C, CC, CH, X[maxX] ;
Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
Pt PFailAddr ;
Int hostSpeed ;

static Hdl S ;
static ChoicePointPt B0 ;
static EnvironmentPt saveE ;
static ChoicePointPt saveB ;
static PrologHashTable ht, ht1 ;

static Hdl hh ;
static Pt drf, t ;
static int s ;
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
	PredicatePt pr ;

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
	elif( IsAtomOrText(t) ) {
		return LookupFunctor(XAtomOrTextAsAtom(t), 0) ;
	}
	elif( IsList(t) ) {
		X0 = Drf(XListHead(t)) ;
		X1 = Drf(XListTail(t)) ;
		return listFunctor ;
	}
	elif( IsInt(t) ) 
		Error("INTEGER number in call/1: %ld", XInt(t)) ;
	elif( IsReal(t) ) 
		Error("REAL number in call/1: %.5g", XReal(t)) ;
	elif( IsVar(t) )
		Error("Unbound VARIABLE in call/1") ;
	elif( IsExtra(t) ) 
		Error("EXTRA term in call/1: '%s'", XExtraAsStr(t)) ;
	else Default("ExecuteVarInst") ;
	return nil ;
}

void RunMachine()
{
	ResetDebug() ;
	Run() ;
	InternalError("RunMachine") ;
}

Bool MachineIsOn()
{
	return P != nil ;
}

void CheckHost()
{
	if( cWord(&stacksBegin) % sizeof(Word) != 0 )
		Warning("Machine registers are not aligned") ;
	if( cWord(buffer) < 0 )
		Warning("This machine is backwards") ;
	if( GetTag(buffer) != 0 )
		Warning("Tags are clobbered by Pt values") ;
	if( cWord(buffer) % sizeof(Word) != 0 )
		Warning("Memory areas are not aligned") ;
}

void CheckHostSpeed()
{
	Real tt = CurrTime() ;
	Hdl saveH = H ;
	hostSpeed = 0 ;
	do {
		CharPt s = "a(P,T,O):-a(O),a(0,P),a(O,T,P,L,R),a(T,P,L,R),a(0,T)." ;
		Pt t = ReadTermFromStr(s) ;
		TermAsStr(t) ;
		H = saveH ;
		hostSpeed++ ;
	} while( CurrTime() - tt < 0.03 ) ;
	if( hostSpeed == 0 ) hostSpeed = 1 ;
}


/* PROCEDURAL & CONTROL INSTRUCTIONS */

#define ALLOCATE()	\
	saveE = E ;		\
	E = cEnvironmentPt(TopOfLocalStack()) ;	\
	Ef(E) = saveE ;	\
	Ef(CP) = CP ;	\
	Ef(CC) = CC ;	\
	Ef(B0) = B0 ;

#define AT_EVERY_CALL(p, f)		\
	{	if( traceActive_flag )	\
			DebugCall(p, f) ;	\
		CheckLocalStackOverflow() ; }

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
	AT_EVERY_CALL(LookPred(), nil) ;
	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
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
	t = cPt(PrepareCall()) ;
	Jump(ExecuteVarAux)
}

static void SysCallVarInst()
{
	CP = P + 1 ;	/* Skip argument */
	CC = C ;
	t = cPt(PrepareCall()) ;
	Jump(SysExecuteVarAux)
}

static void ExecuteVarInst()
{
	t = cPt(PrepareCall()) ;
	Jump(ExecuteVarAux)
}

static void SysExecuteVarInst()
{
	t = cPt(PrepareCall()) ;
	Jump(SysExecuteVarAux)
}

static void ExecuteVarAuxInst()
{
	PredicatePt pr ;
	if( (pr = SearchContext(cFunctorPt(t))) == nil ) {
		AT_EVERY_CALL(nil, cFunctorPt(t))
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
	PredicatePt pr ;
	if( (pr = SearchContext(cFunctorPt(t))) == nil ) {
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
	/* TidyTrail
		t = cPt(Bf(TR)) ;
		while( cHdl(t) < TR )
			if( IsToTrailVar(*cHdl(t)) ) t++ ;
			else *t = cWord(Pop(TR)) ; */
	}
	JumpNext()
}

static void PutCutLevelInst()
{
	X(GetHdl()) = TagStruct(H) ;
	Push(H, cPt(cutFunctor)) ;
	Push(H, TagAtom(Ef(B0))) ;	/* aux tag */
	JumpNext()
}

static void FailInst()
{
	DoFail()
}

static void UndefInst()
{
	if( (t = cPt(SearchContextBelow(LookFunctor()))) == nil )
		DoFail()
	P = PredCode(cPredicatePt(t)) ;
	JumpNext()
}



/* PUT INSTRUCTIONS */

static void PutYVariableInst()
{
	hh = &Y(GetInt()) ;
	X(GetHdl()) = ResetVar(*hh) ;
	JumpNext()
}

static void PutXVariableInst()
{
	hh = GetHdl() ;
	X(GetHdl()) = X(hh) = PushVar(H) ;
	JumpNext()
}

static void PutXVariableOneInst()
{
	X(GetHdl()) = PushVar(H) ;
	JumpNext()
}

static void PutXValueInst()
{
	hh = GetHdl() ;
	X(GetHdl()) = X(hh) ;
	JumpNext()
}

static void PutYValueInst()
{
	s = GetInt() ;
	X(GetHdl()) = Y(s) ;
	JumpNext()
}


static void PutUnsafeValueInst()
{
	VarValue2(drf, Y(GetInt())) ;
	if( IsVar(drf) && IsCurrEnvVar(drf) ) {
		Assign(drf, X(GetHdl()) = PushVar(H)) ;
		JumpNext()
	}
	else {
		X(GetHdl()) = drf ;
		JumpNext()
	}
}

static void PutAtomicInst()
{
	t = GetPt() ;
	X(GetHdl()) = t ;
	JumpNext()
}

static void PutNilInst()
{
	X(GetHdl()) = tNilAtom ;
	JumpNext()
}

static void PutExtraInst()
{
	t = GetPt() ;
	X(GetHdl()) = t ;
	JumpNext()
}

static void PutStructureInst()
{
	t = GetPt() ;
	X(GetHdl()) = TagStruct(H) ;
	Push(H, t) ;
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
	s = GetInt() ;
	Y(s) = X(GetHdl()) ;
	JumpNext()
}

static void GetXValueInst()
{
	if( Unify(X(GetHdl()), X(hh = GetHdl())) ) {
		VarValue2(drf, X(hh)) ;
		X(hh) = drf ;
		JumpNext()
	}
	DoFail()
}

static void GetYValueInst()
{
	s = GetInt() ;
	if( Unify(Y(s), X(GetHdl())) ) JumpNext()
	DoFail()
}

static void GetAtomicInst()
{
	t = GetPt() ;
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) ) {
		Assign(drf, t) ;
		JumpNext()
	}
	if( drf == t ) JumpNext()
	if( IsText(drf)
		&& IsAtom(t)
		&& EqualStr(XAtomOrTextName(drf), XAtomOrTextName(t)) )
			JumpNext()
	DoFail()
}

static void GetNilInst()
{
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) ) {
		Assign(drf, tNilAtom) ;
		JumpNext()
	}
	if( drf == tNilAtom ) JumpNext()
	DoFail()
}

static void GetExtraInst()
{
	t = GetPt() ;
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) ) {
		Assign(drf, t) ;
		JumpNext()
	}
	if( EqExtra(drf, t) ) JumpNext()
	DoFail()
}

static void GetStructureInst()
{
	t = GetPt() ;
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) ) {
		Assign(drf, TagStruct(H)) ;
		Push(H, t) ;
		writeMode = true ;
		JumpNext()
	}
	if( IsThisStruct(drf, cFunctorPt(t)) ) {
		S = XStructArgs(drf) ;
		writeMode = false ;
		JumpNext()
	}
	DoFail()
}

static void GetListInst()
{
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) ) {
		Assign(drf, TagList(H)) ;
		writeMode = true ;
		JumpNext()
	}
	if( IsList(drf) ) {
		S = XListArgs(drf) ;
		writeMode = false ;
		JumpNext()
	}
	DoFail()
}



/* UNIFY INSTRUCTIONS */

static void UnifyVoidInst()
{
	if( writeMode ) {
		s = GetInt() ;
		while( s-- ) PushVar(H) ;
		JumpNext()
	}
	else {
		S += GetInt() ;
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
		Y(GetInt()) = PushVar(H) ;
		JumpNext()
	}
	else {
		Y(GetInt()) = *S++ ;
		JumpNext()
	}
}

static void UnifyXValueInst()
{
	if( writeMode ) {
		Push(H, X(GetHdl())) ;
		JumpNext()
	}
	else {
		if( Unify(*S++, X(hh = GetHdl())) ) {
			VarValue2(drf, X(hh)) ;
			X(hh) = drf ;
			JumpNext()
		}
		DoFail()
	}
}

static void UnifyYValueInst()
{
	if( writeMode ) {
		Push(H, Y(GetInt())) ;
		JumpNext()
	}
	else {
		if( Unify(*S++, Y(GetInt())) ) JumpNext()
		DoFail()
	}
}

static void UnifyXLocalValueInst()
{
	if( writeMode ) {
		VarValue2(drf, X(hh = GetHdl())) ;
		if( IsVar(drf) && IsLocalVar(drf) ) {
			Assign(drf, X(hh) = PushVar(H)) ;
			JumpNext()
		}
		else {
			Push(H, X(hh) = drf) ;
			JumpNext()
		}
	}
	else {
		if( Unify(*S++, X(hh = GetHdl())) ) {
			VarValue2(drf, X(hh)) ;
			X(hh) = drf ;
			JumpNext()
		}
		DoFail()
	}
}

static void UnifyYLocalValueInst()
{
	if( writeMode ) {
		VarValue2(drf, Y(GetInt())) ;
		if( IsVar(drf) && IsLocalVar(drf) ) {
			Assign(drf, PushVar(H)) ;
			JumpNext()
		}
		else {
			Push(H, drf) ;
			JumpNext()
		}
	}
	else {
		if( Unify(*S++, Y(GetInt())) ) JumpNext()
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
		t = GetPt() ;
		VarValue2(drf, *S++) ;
		if( IsVar(drf) ) {
			Assign(drf, t) ;
			JumpNext()
		}
		if( drf == t ) JumpNext()
		if( IsText(drf)
			&& IsAtom(t)
			&& EqualStr(XAtomOrTextName(drf), XAtomOrTextName(t)) )
				JumpNext()
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
		VarValue2(drf, *S++) ;
		if( IsVar(drf) ) {
			Assign(drf, tNilAtom) ;
			JumpNext()
		}
		if( drf == tNilAtom ) JumpNext()
		DoFail()
	}
}

static void UnifyExtraInst()
{
	if( writeMode ) {
		Push(H, GetPt()) ;
		JumpNext()
	}
	else {
		VarValue2(drf, *S++) ;
		if( IsVar(drf) ) {
			Assign(drf, GetPt()) ;
			JumpNext()
		}
		if( EqExtra(drf, GetPt()) ) JumpNext()
		DoFail()
	}
}



/* BUILD INSTRUCTIONS */

static void BuildVoidInst()
{
	s = GetInt() ;
	while( s-- ) PushVar(H) ;
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
	Y(GetInt()) = PushVar(H) ;
	JumpNext()
}

static void BuildXValueInst()
{
	Push(H, X(GetHdl())) ;
	JumpNext()
}

static void BuildYValueInst()
{
	Push(H, Y(GetInt())) ;
	JumpNext()
}

static void BuildXLocalValueInst()
{
	VarValue2(drf, X(hh = GetHdl())) ;
	if( IsVar(drf) && IsLocalVar(drf) ) {
		Assign(drf, X(hh) = PushVar(H)) ;
		JumpNext()
	}
	else {
		Push(H, X(hh) = drf) ;
		JumpNext()
	}
}

static void BuildYLocalValueInst()
{
	VarValue2(drf, Y(GetInt())) ;
	if( IsVar(drf) && IsLocalVar(drf) ) {
		Assign(drf, PushVar(H)) ;
		JumpNext()
	}
	else {
		Push(H, drf) ;
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

static void BuildExtraInst()
{
	Push(H, GetPt()) ;
	JumpNext()
}



/* CONTEXT INSTRUCTIONS */

static void PushCtxCallVarInst()
{
	Push(H, cPt(TermToUnit(X0))) ;
	Push(H, X0) ;
	Push(H, C) ;
	C = TagList(H-2) ;
	X0 = X1 ;
	Jump(CallVar)
}

static void AllocSwitchCtxCallInst()
{
	ALLOCATE()
	Push(H, cPt(TermToUnit(P[0]))) ;
	Push(H, P[0]) ;
	if( t == tNilAtom )
		Error("Cannot switch top of emtpy context") ;
	Push(H, XListTail(C)) ;
	C = TagList(H-2) ;

	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	t = P[5] ;	/* functor */
	Jump(ExecuteVarAux)
}

static void SwitchCtxCallVarInst()
{
	Push(H, cPt(TermToUnit(X0))) ;
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
				while( s-- ) A(s) = Xc(s)

#define RestoreState()						\
				E = Bf(E) ;					\
				CP = Bf(CP) ;				\
				B0 = Bf(B0) ;				\
				C = Bf(C) ;					\
				CC = Bf(CC) ;				\
				CH = Bf(CH) ;				\
				H = Bf(H) ;					\
				RestoreTrail(Bf(TR), hh) ;	\
				while( s-- ) Xc(s) = A(s)

#define DiscardLastChoice()					\
				B = Bf(B) ;					\
				HB = Bf(H)

static void TryMeElseInst()
{
	hh = ClauseCode(GetClause()) ;
	s = GetInt() ;
	saveB = B ;
	B = cChoicePointPt(TopOfLocalStack() - s) - 1 ;
	SaveState(hh, saveB) ;
	JumpNext()
}

static void RetryMeElseInst()
{
	Bf(P) = ClauseCode(GetClause()) ;
	s = GetInt() ;
	RestoreState() ;
	JumpNext()
}

static void TrustMeInst()
{
	SkipInst() ;
	s = GetInt() ;
	RestoreState() ;
	DiscardLastChoice() ;
	JumpNext()
}

static void TryInst()
{	
	hh = GetHdl() ;
	s = cInt(hh[-1]) ;
	saveB = B ;
	B = cChoicePointPt(TopOfLocalStack() - s) - 1 ;
	SaveState(P, saveB) ;
	P = hh ;
	JumpNext()
}

static void RetryInst()
{
	Bf(P) = P + 1 ;
	hh = GetHdl() ;
	P = hh ;
	s = cInt(P[-1]) ;
	RestoreState() ;
	JumpNext()
}

static void TrustInst()
{
	hh = GetHdl() ;
	P = hh ;
	s = cInt(P[-1]) ;
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
	if( IsList(drf) ) {
		P = cHdl(P[1]) ;
		JumpNext()
	}
	elif( IsAtomic(drf) ) {
		P = cHdl(P[0]) ;
		JumpNext()
	}
	elif( IsVar(drf) ) {
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
	VarValue2(drf, X0) ;
	Jump(SwitchOnTermAuxInst)
}

static void SwitchOnTerm1Inst()
{
	VarValue2(drf, X1) ;
	Jump(SwitchOnTermAuxInst)
}

static void SwitchOnTerm2Inst()
{
	VarValue2(drf, X2) ;
	Jump(SwitchOnTermAuxInst)
}

static void SwitchOnAtomicInst()
{
	/* Got here from SwitchOnTerm: drf already constains Drf(X?) */
	if( IsText(drf) ) drf = CheckTextAsAtom(drf) ;
	ht = (PrologHashTable)(P+2) ;
	dolist(ht1, ht + PrologHash(drf, cInt(P[0])), ht1->next)
		if( ht1->value == drf ) {
			P = ht1->address ;
			JumpNext()
		}
	P = cHdl(P[1]) ;
	JumpNext()
}

static void SwitchOnStructureInst()
{
	/* Got here from SwitchOnTerm: drf already constains Drf(X?) */
	drf = cPt(XStructFunctor(drf)) ;
	ht = (PrologHashTable)(P+2) ;
	dolist(ht1, ht + PrologHash(drf, cInt(P[0])), ht1->next)
		if( ht1->value == drf ) {
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

	GetYVariable, GetXValue, GetYValue, GetAtomic, GetNil, GetExtra,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,

	PutXValue, PutYValue, PutUnsafeValue, PutAtomic, PutNil, PutExtra, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,

	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue,
	UnifyAtomic, UnifyNil, UnifyExtra, BuildVoid, BuildVoidOne, BuildXVariable,

	BuildYVariable, BuildXValue, BuildYValue, BuildXLocalValue,
	BuildYLocalValue, BuildAtomic, BuildNil, BuildExtra,

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

static void InitInstructions()
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
	II(&GetExtra,		GetExtraInst,			"GetExtra",			"tx") ;
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
	II(&PutExtra,		PutExtraInst,			"PutExtra",			"tx") ;
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
	II(&UnifyExtra,		UnifyExtraInst,			"UnifyExtra",		"t") ;

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
	II(&BuildExtra,		BuildExtraInst,			"BuildExtra",		"t") ;

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

void InitMachine()
{
	PFailAddr = Z(FailInst) ;
	C = tNilAtom ;	/* This makes CurrUnit() == bottomUnit */
	InitInstructions() ;
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
