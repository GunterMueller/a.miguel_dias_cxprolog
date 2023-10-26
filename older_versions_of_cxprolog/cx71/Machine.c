/*
 *   This file is part of the CxProlog system

 *   Machine.c
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

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

static Bool canAttention, attention, trace, debug ;

Hdl P, CP, H, HB, TR ;
EnvironmentPt E ;
ChoicePointPt B ;
Pt C, CC, CH, X[maxX] ;
Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
Pt PFailAddr ;

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

/* MISC FUNCTIONS */

static PredicatePt SearchContext(FunctorPt f)
{
	register PredicatePt pr ;

	if( (pr = FindPredicate(f)) != nil )
		return pr ;

	if( CanBeVisible(f) )
		for( C = Drf(XListTail(C)) ; C != tNilAtom ; C = Drf(XListTail(C)) )
			if( (pr = FindVisiblePredicate(f)) != nil )
				return pr ;
	return nil ;
}

static PredicatePt SearchContextBelow(FunctorPt f)
{
	register PredicatePt pr ;

	if( CanBeVisible(f) )
		for( C = Drf(XListTail(C)) ; C != tNilAtom ; C = Drf(XListTail(C)) )
			if( (pr = FindVisiblePredicate(f)) != nil )
				return pr ;
	return nil ;
}

static FunctorPt PrepareCall(register Pt t)
{
	VarValue(t) ;
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
	else TypeError("atom or functor", t) ;
}

void RunMachine()
{
	InitDebug() ;
	Run() ;
	InternalError("RunMachine") ;
}

Bool MachineIsOn()
{
	return P != nil ;
}

void SetTrace(Bool on)
{
	trace = on ;
}

Bool CheckTrace()
{
	return trace ;
}

void SetDebug(Bool on)
{
	debug = on ;
	attention = canAttention && debug ;
}

void SetInTopGoal(Bool b)
{
	debug = trace ;
	canAttention = b ;
	attention = canAttention && debug ;
}

Bool CheckInTopGoal()
{
	return canAttention ;
}

static void HandleAttention(PredicatePt pr)
{
	if( debug )
		DebugCall(pr) ;
}

void CheckHost(Hdl h)
{
	if( cWord(&stacksBegin) % sizeof(Word) != 0 )
		Warning("Machine registers are not aligned") ;
	if( cWord(h) < 0 )
		Warning("This machine is backwards") ;
	if( GetTag(h) != 0 )
		Warning("Tags are clobbered by Pt values") ;
	if( cWord(h) % sizeof(Word) != 0 )
		Warning("Memory areas are not aligned") ;
}

void WritePrompt()
{
	Pt t ;

	if( CheckTrace() )
		WriteStd("(trace) ") ;
	t = ImperativeVarGet(LookupAtom("@@_env_context")) ;
	GWriteTerm(t, wNormal, true) ;
	WriteStd(" ?- ") ;
	Flush("") ;
}


/* PROCEDURAL & CONTROL INSTRUCTIONS */

#define ALLOCATE()	\
	saveE = E ;		\
	E = cEnvironmentPt(TopOfLocalStack()) ;	\
	Ef(E) = saveE ;	\
	Ef(CP) = CP ;	\
	Ef(CC) = CC ;	\
	Ef(B0) = B0 ;

#define AT_EVERY_CALL(t)						\
	{	if( attention )							\
			HandleAttention(cPredicatePt(t)) ;	\
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
	CP = P + 2 ;	/* Skip arguments */
	CC = C ;
	AT_EVERY_CALL(LookPred())
	P = PredCode(LookPred()) ;
	B0 = B ;
	JumpNext()
}

static void ExecuteInst()
{
	AT_EVERY_CALL(LookPred())
	P = PredCode(LookPred()) ;
	B0 = B ;
	JumpNext()
}

static void CallVarInst()
{
	CP = P + 1 ;	/* Skip argument */
	CC = C ;
	t = cPt(PrepareCall(X0)) ;
	Jump(ExecuteVarAux)
}

static void ExecuteVarInst()
{
	t = cPt(PrepareCall(X0)) ;
	Jump(ExecuteVarAux)
}

static void ExecuteVarAuxInst()
{
	if( (t = cPt(SearchContext(cFunctorPt(t)))) == nil )
		DoFail()
	AT_EVERY_CALL(t)
	P = PredCode(cPredicatePt(t)) ;
	B0 = B ;
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
	Push(H, TagAtom(Ef(B0))) ;
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
		VarValue2(drf, *S++) ;
		if( IsVar(drf) ) {
			Assign(drf, GetPt()) ;
			JumpNext()
		}
		if( drf == GetPt() ) JumpNext()
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
	t = P[4] ;	/* functor */
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

static void SwitchOnTermInst()
{
	VarValue2(drf, X0) ;
	if( IsList(drf) ) {
		P = cHdl(P[1]) ;
		JumpNext()
	}
	elif( IsAtomic(drf) ) {
		P = cHdl(P[0]) ;
		JumpNext()
	}
	elif( IsVar(drf) ) {
		P = ClauseCode(cClausePt(P[3])) ;
		JumpNext()
	}
	else {
		P = cHdl(P[2]) ;
		JumpNext()
	}
}

static void SwitchOnAtomicInst()
{
	VarValue2(drf, X0) ;
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
	VarValue2(drf, X0) ;
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
	Call, Execute, CallVar, ExecuteVarAux, ExecuteVar, CCall, CExecute,	
	Cut, PutCutLevel, Fail, Undef,

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
	SwitchOnTerm, SwitchOnAtomic, SwitchOnStructure, DiscardAndFail ;

static struct instInfo
{
	Pt inst ;
	CharPt name, types ;
} insts[maxInstructions] ;
static nInsts = 0 ;

static void II(Pt *inst, Proc proc, CharPt name, CharPt types)
{
	insts[nInsts].inst = *inst = Z(proc) ;
	insts[nInsts].name = name ;
	insts[nInsts].types = types ;
	nInsts++ ;
}

void InitMachine()
{
	PFailAddr = Z(FailInst) ;
	C = tNilAtom ;

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
	II(&ExecuteVarAux,	ExecuteVarAuxInst,		"ExecuteVarAux",	"") ;
	II(&ExecuteVar,		ExecuteVarInst,			"ExecuteVar",		"@") ;
	II(&CCall,			CallInst,				"CCall",			"c") ;
	II(&CExecute,		ExecuteInst,			"CExecute",			"c@") ;
	II(&Cut,			CutInst,				"Cut",				"") ;
	II(&PutCutLevel,	PutCutLevelInst,		"PutCutLevel",		"x") ;
	II(&Fail,			FailInst,				"Fail",				"") ;
	II(&Undef,			UndefInst,				"Undef",			"f@") ;

/* GET INSTRUCTIONS */
	II(&GetYVariable,	GetYVariableInst,		"GetYVariable",		"yx") ;
	II(&GetXValue,		GetXValueInst,			"GetXValue",		"xx") ;
	II(&GetYValue,		GetYValueInst,			"GetYValue",		"yx") ;
	II(&GetAtomic,		GetAtomicInst,			"GetAtomic",		"ax") ;
	II(&GetNil,			GetNilInst,				"GetNil",			"x") ;
	II(&GetExtra,		GetExtraInst,			"GetExtra",			"ax") ;
	II(&GetStructure,	GetStructureInst,		"GetStructure",		"fx") ;
	II(&GetList,		GetListInst,			"GetList",			"x") ;
	
/* PUT INSTRUCTIONS */
	II(&PutXVariable,	PutXVariableInst,		"PutXVariable",		"xx") ;
	II(&PutXVariableOne,PutXVariableOneInst,	"PutXVariableOne",	"x"	) ;
	II(&PutYVariable,	PutYVariableInst,		"PutYVariable",		"yx") ;
	II(&PutXValue,		PutXValueInst,			"PutXValue",		"xx") ;
	II(&PutYValue,		PutYValueInst,			"PutYValue",		"yx") ;
	II(&PutUnsafeValue,	PutUnsafeValueInst,		"PutUnsafeValue",	"yx") ;
	II(&PutAtomic,		PutAtomicInst,			"PutAtomic",		"ax") ;
	II(&PutNil,			PutNilInst,				"PutNil",			"x") ;
	II(&PutExtra,		PutExtraInst,			"PutExtra",			"ax") ;
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
	II(&UnifyAtomic,	UnifyAtomicInst,		"UnifyAtomic",		"a") ;
	II(&UnifyNil,		UnifyNilInst,			"UnifyNil",			"") ;
	II(&UnifyExtra,		UnifyExtraInst,			"UnifyExtra",		"a") ;

/* BUILD INSTRUCTIONS */
	II(&BuildVoid,		BuildVoidInst,			"BuildVoid",		"n") ;
	II(&BuildVoidOne,	BuildVoidOneInst,		"BuildVoidOne",		"") ;
	II(&BuildXVariable,	BuildXVariableInst,		"BuildXVariable",	"x"	) ;
	II(&BuildYVariable,	BuildYVariableInst,		"BuildYVariable",	"y"	) ;
	II(&BuildXValue,	BuildXValueInst,		"BuildXValue",		"x") ;
	II(&BuildYValue,	BuildYValueInst,		"BuildYValue",		"y") ;
	II(&BuildXLocalValue,BuildXLocalValueInst,	"BuildXLocalValue",	"x"	) ;
	II(&BuildYLocalValue,BuildYLocalValueInst,	"BuildYLocalValue",	"y"	) ;
	II(&BuildAtomic,	BuildAtomicInst,		"BuildAtomic",		"a") ;
	II(&BuildNil,		BuildNilInst,			"BuildNil",			"") ;
	II(&BuildExtra,		BuildExtraInst,			"BuildExtra",		"a") ;

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
	II(&Try,			TryInst,				"Try",				"!l") ;
	II(&Retry,			RetryInst,				"Retry",			"l"	) ;
	II(&Trust,			TrustInst,				"Trust",			"l"	) ;
	II(&DiscardAndFail,	DiscardAndFailInst,		"DiscardAndFail",	"") ;
	II(&SwitchOnTerm,	SwitchOnTermInst,		"SwitchOnTerm",		"!lllc@") ;
	II(&SwitchOnAtomic,	SwitchOnAtomicInst,		"SwitchOnAtomic",	"!H") ;
	II(&SwitchOnStructure,SwitchOnStructureInst,"SwitchOnStructure","!H") ;
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
