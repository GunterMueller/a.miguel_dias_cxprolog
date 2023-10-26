/*
 *   This file is part of the NanoProlog system

 *   Interpreter.c
 *   by A.Miguel Dias - 89/11/25
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 000324: added support for extra types: see Extra instruction set.
 970408: PutCutLevel instruction added to support cuts within ;/2, not/1, etc.
 970408: Cut instruction was updated accordingly
 931117: release of version 0.5
*/

#include "NanoProlog.h"


#define INDEXING	1


/* ARGUMENT ACCESS */

#define GetPt()			(*P++)
#define LookPt()		(*P)
#define GetHdl()		cHdl(*P++)
#define GetInt()		cInt(*P++)
#define LookPred()		cPredicatePt(*P)
#define SkipInst()		(P++)


/* MACHINE REGISTERS */

Bool trace, hideTrace ;

Hdl P, CP, H, HB, TR ;
EnvironmentPt E ;
ChoicePointPt B ;
Pt X[maxX] ;
Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;

Pt PFailAddr ;

static Bool writeMode ;
static Hdl S ;
static ChoicePointPt B0 ;
static EnvironmentPt saveE ;
static ChoicePointPt saveB ;
static PrologHashTable ht, ht1 ;

static Hdl hh ;
static Pt t ;
static short s, ss ;

#if 1
static short filler ;
#endif

/* INTERPRETER INITIALIZATION & LAUNCHING */

static void ShowCall(PredicatePt p)
{
	int i, arity ;

	if( hideTrace ) return ;
	printf("%s", PredNameArity(p)) ;
	if( (arity = PredArity(p)) > 0 )
	{
		printf("(") ;
		dotimes(i, arity)
		{
			WriteTerm(Xc(i)) ;
			if( i != arity - 1 ) printf(", ") ;
		}
		printf(")") ;
	}
	printf("\n") ;
}	

void RunInterpreter()
{
	Run() ;
	InternalError("RunInterpreter") ;
}

static PredicatePt PrepareCall()
{
	PredicatePt pr ;
	register Pt t ;
	
	VarValue2(t, Xc(0)) ;
	if( IsStruct(t) )
	{
		s = XStructArity(t) ;
		while( s-- )
			Xc(s) = Drf(XStructArg(t,s)) ;
		return( LookupPredicateInModule(XStructFunctor(t), SourceModule()) ) ;
	}
	elif( IsAtom(t) )
	{
		return( LookupPredicateInModule(LookupFunctor(XAtom(t), 0), SourceModule()) ) ;
	}
	elif( IsList(t) )
	{
		Xc(0) = Drf(XListHead(t)) ;
		Xc(1) = Drf(XListTail(t)) ;
		return( LookupPredicateInModule(listFunctor, sysModule) ) ;
	}
	elif( IsNumber(t) ) Error("Clause is a number") ;
	elif( IsExtra(t) ) Error("Clause is an element of an extra type") ;
	elif( IsVar(t) ) Error("Clause is a variable") ;
	else Default("PrepareCall") ;
}

#define CallMacro()							\
{											\
	if( trace ) ShowCall(LookPred()) ;		\
	TestOverflow(E) ;						\
	P = PredCode(LookPred()) ;				\
	B0 = B ;								\
}

#define CallVarMacro()						\
{											\
	t = cPt(PrepareCall()) ;				\
	if( trace ) ShowCall(cPredicatePt(t)) ;	\
	TestOverflow(E) ;						\
	if( PredIsC(cPredicatePt(t)) )			\
	{										\
		t = cPt(PredProc(cPredicatePt(t))) ;\
		Jump(t) ;							\
	}										\
	CP = P ;								\
	P = PredCode(cPredicatePt(t)) ;			\
	B0 = B ;								\
}

/* MACHINE INSTRUCTIONS */

/* PROCEDURAL INSTRUCTIONS */

static void NopInst()
{
	JumpNext()
}

static void AllocateInst()		/* Have no corresponding choice point */
{
	saveE = E ;
	E = cEnvironmentPt(cPt(B) < cPt(E) ? cPt(B) : cPt(E) - cInt(CP[-1])) ;
	Ef(E) = saveE ;
	Ef(CP) = CP ;
	Ef(B) = B0 ;
	JumpNext()
}

static void DeallocateInst()
{
	CP = Ef(CP) ;
	E = Ef(E) ;
	JumpNext()
}

static void CallInst()
{
	CP = P + 2 ;	/* Skip arguments */
	CallMacro() ;
	JumpNext()
}

static void CallVarInst()
{
	P++ ;
	CallVarMacro() ;
	JumpNext()
}

static void ExecuteInst()
{
	CallMacro() ;
	JumpNext()
}

static void ExecuteVarInst()
{
	P = CP ;		/* Proceed */
	CallVarMacro() ;
	JumpNext()
}

static void ProceedInst()
{
	P = CP ;
	JumpNext()
}

/* Makes lastChoice point to the choice point below fp */
static void CutInst()
{
	if( Ef(B) > B )
	{
		B = Ef(B) ;
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
	Push(H, TagAtom(Ef(B))) ;
	JumpNext()
}

static void FailInst()
{
	DoFail()
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
	if( IsVar(drf) && IsCurrEnvVar(drf) )
	{
		Assign(drf, X(GetHdl()) = PushVar(H)) ;
		JumpNext()
	}
	else
	{
		X(GetHdl()) = drf ;
		JumpNext()
	}
}

static void PutConstantInst()
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
	if( Unify(X(GetHdl()), X(hh = GetHdl())) )
	{
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

static void GetConstantInst()
{
	t = GetPt() ;
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) )
	{
		Assign(drf, t) ;
		JumpNext()
	}
	if( drf == t ) JumpNext()
	DoFail()
}

static void GetNilInst()
{
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) )
	{
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
	if( IsVar(drf) )
	{
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
	if( IsVar(drf) )
	{
		Assign(drf, TagStruct(H)) ;
		Push(H, t) ;
		writeMode = true ;
		JumpNext()
	}
	if( IsThisStruct(drf, cFunctorPt(t)) )
	{
		S = XStructArgs(drf) ;
		writeMode = false ;
		JumpNext()
	}
	DoFail()
}

static void GetListInst()
{
	VarValue2(drf, X(GetHdl())) ;
	if( IsVar(drf) )
	{
		Assign(drf, TagList(H)) ;
		writeMode = true ;
		JumpNext()
	}
	if( IsList(drf) )
	{
		S = XListArgs(drf) ;
		writeMode = false ;
		JumpNext()
	}
	DoFail()
}



/* UNIFY INSTRUCTIONS */

static void UnifyVoidInst()
{
	if( writeMode )
	{
		s = GetInt() ;
		while( s-- ) PushVar(H) ;
		JumpNext()
	}
	else
	{
		S += GetInt() ;
		JumpNext()
	}
}

static void UnifyVoidOneInst()
{
	if( writeMode )
	{
		PushVar(H) ;
		JumpNext()
	}
	else
	{
		S++ ;
		JumpNext()
	}
}

static void UnifyXVariableInst()
{
	if( writeMode )
	{
		X(GetHdl()) = PushVar(H) ;
		JumpNext()
	}
	else
	{
		X(GetHdl()) = *S++ ;
		JumpNext()
	}
	JumpNext()
}

static void UnifyYVariableInst()
{
	if( writeMode )
	{
		Y(GetInt()) = PushVar(H) ;
		JumpNext()
	}
	else
	{
		Y(GetInt()) = *S++ ;
		JumpNext()
	}
}

static void UnifyXValueInst()
{
	if( writeMode )
	{
		Push(H, X(GetHdl())) ;
		JumpNext()
	}
	else
	{
		if( Unify(*S++, X(hh = GetHdl())) )
		{
			VarValue2(drf, X(hh)) ;
			X(hh) = drf ;
			JumpNext()
		}
		DoFail()
	}
}

static void UnifyYValueInst()
{
	if( writeMode )
	{
		Push(H, Y(GetInt())) ;
		JumpNext()
	}
	else
	{
		if( Unify(*S++, Y(GetInt())) ) JumpNext()
		DoFail()
	}
}

static void UnifyXLocalValueInst()
{
	if( writeMode )
	{
		VarValue2(drf, X(hh = GetHdl())) ;
		if( IsVar(drf) && IsLocalVar(drf) )
		{
			Assign(drf, X(hh) = PushVar(H)) ;
			JumpNext()
		}
		else
		{
			Push(H, X(hh) = drf) ;
			JumpNext()
		}
	}
	else
	{
		if( Unify(*S++, X(hh = GetHdl())) )
		{
			VarValue2(drf1, X(hh)) ;
			X(hh) = drf1 ;
			JumpNext()
		}
		DoFail()
	}
}

static void UnifyYLocalValueInst()
{
	if( writeMode )
	{
		VarValue2(drf, Y(GetInt())) ;
		if( IsVar(drf) && IsLocalVar(drf) )
		{
			Assign(drf, PushVar(H)) ;
			JumpNext()
		}
		else
		{
			Push(H, drf) ;
			JumpNext()
		}
	}
	else
	{
		if( Unify(*S++, Y(GetInt())) ) JumpNext()
		DoFail()
	}
}

static void UnifyConstantInst()
{
	if( writeMode )
	{
		Push(H, GetPt()) ;
		JumpNext()
	}
	else
	{
		VarValue2(drf, *S++) ;
		if( IsVar(drf) )
		{
			Assign(drf, GetPt()) ;
			JumpNext()
		}
		if( drf == GetPt() ) JumpNext()
		DoFail()
	}
}

static void UnifyNilInst()
{
	if( writeMode )
	{
		Push(H, tNilAtom) ;
		JumpNext()
	}
	else
	{
		VarValue2(drf, *S++) ;
		if( IsVar(drf) )
		{
			Assign(drf, tNilAtom) ;
			JumpNext()
		}
		if( drf == tNilAtom ) JumpNext()
		DoFail()
	}
}

static void UnifyExtraInst()
{
	if( writeMode )
	{
		Push(H, GetPt()) ;
		JumpNext()
	}
	else
	{
		VarValue2(drf, *S++) ;
		if( IsVar(drf) )
		{
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
	if( IsVar(drf) && IsLocalVar(drf) )
	{
		Assign(drf, X(hh) = PushVar(H)) ;
		JumpNext()
	}
	else
	{
		Push(H, X(hh) = drf) ;
		JumpNext()
	}
}

static void BuildYLocalValueInst()
{
	VarValue2(drf, Y(GetInt())) ;
	if( IsVar(drf) && IsLocalVar(drf) )
	{
		Assign(drf, PushVar(H)) ;
		JumpNext()
	}
	else
	{
		Push(H, drf) ;
		JumpNext()
	}
}

static void BuildConstantInst()
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



/* INDEXING INSTRUCTIONS */

static void MakeIndexInst()
{
#if INDEXING
	DoIndex(LookPred()) ;
#else
	NoIndex(LookPred()) ;
#endif
	P = PredCode(LookPred()) ;
	JumpNext()
}

#define SaveState(next)						\
				Bf(E) = E ;					\
				Bf(CP) = CP ;				\
				Bf(B) = saveB ;				\
				Bf(B0) = B0 ;				\
				Bf(P) = next ;				\
				Bf(TR) = TR ;				\
				HB = Bf(H) = H ;			\
				while( s-- ) A(s) = Xc(s)

#define RestoreState()						\
				E = Bf(E) ;					\
				CP = Bf(CP) ;				\
				B0 = Bf(B0) ;				\
				H = Bf(H) ;					\
	 			RestoreTrail(Bf(TR), hh) ;	\
				while( s-- ) Xc(s) = A(s)

#define DiscardLastChoice()					\
				B = Bf(B) ;					\
				HB = Bf(H)

static void TryMeElseInst()
{
	hh = GetHdl() ;
	s = GetInt() ;
	B = cChoicePointPt( (cPt(saveB=B) < cPt(E)
								? cPt(B) : cPt(E) - cInt(CP[-1]))
						- s ) - 1 ;
	SaveState(hh) ;
	JumpNext()
}

static void RetryMeElseInst()
{
	Bf(P) = GetHdl() ;
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
	B = cChoicePointPt( (cPt(saveB=B) < cPt(E)
								? cPt(B) : cPt(E) - cInt(CP[-1]))
						- s ) - 1 ;
	SaveState(P) ;
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
	VarValue2(drf, Xc(0)) ;
	if( IsList(drf) )
	{
		P = XRelloc(P[2]) ;
		JumpNext()
	}
	if( IsAtomic(drf) )
	{
		P = XRelloc(P[1]) ;
		JumpNext()
	}
	elif( IsVar(drf) )
	{
		P = XRelloc(P[0]) ;
		JumpNext()
	}
	else
	{
		P = XRelloc(P[3]) ;
		JumpNext()
	}
}

static void SwitchOnConstantInst()
{
	VarValue2(drf, Xc(0)) ;
	ht = (PrologHashTable)P[0] ;
	foreach(ht1, ht + PrologHash(drf, cInt(P[1])), ht1->next)
		if( ht1->value == drf )
		{
			P = XRelloc(ht1->address) ;
			JumpNext()
		}
	P = XRelloc(P[2]) ;
	JumpNext()
}

static void SwitchOnStructureInst()
{
	VarValue2(drf, Xc(0)) ;
	drf = cPt(XStructFunctor(drf)) ;
	ht = (PrologHashTable)P[0] ;
	foreach(ht1, ht + PrologHash(drf, cInt(P[1])), ht1->next)
		if( ht1->value == drf )
		{
			P = XRelloc(ht1->address) ;
			JumpNext()
		}
	P = XRelloc(P[2]) ;
	JumpNext()
}


/* INSTRUCTIONS */

#define maxInstructions		100

Pt	Nop, Proceed, IAllocate, Call, CallVar,
	Execute, ExecuteVar, Deallocate,
	Cut, PutCutLevel, Fail,
	GetYVariable, GetXValue, GetYValue, GetConstant, GetNil, GetExtra,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,
	PutXValue, PutYValue, PutUnsafeValue, PutConstant, PutNil, PutExtra, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,
	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue,
	UnifyConstant, UnifyNil, UnifyExtra, BuildVoid, BuildVoidOne, BuildXVariable,
	BuildYVariable, BuildXValue, BuildYValue, BuildXLocalValue,
	BuildYLocalValue, BuildConstant, BuildNil, BuildExtra,
	TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust, SwitchOnTerm, DiscardAndFail,
	SwitchOnConstant, SwitchOnStructure, MakeIndex ;

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

void InitInsts()
{
	PFailAddr = Z(FailInst) ;

	II(&Nop,			NopInst,				"Nop",				"") ;
	II(&Proceed,		ProceedInst,			"Proceed",			"") ;
	II(&IAllocate,		AllocateInst,			"Allocate",			"") ;
	II(&Call,			CallInst,				"Call",				"pe") ;
	II(&CallVar,		CallVarInst,			"CallVar",			"e") ;
	II(&Execute,		ExecuteInst,			"Execute",			"p") ;
	II(&ExecuteVar,		ExecuteVarInst,			"ExecuteVar",		"") ;
	II(&Deallocate,		DeallocateInst,			"Deallocate",		"") ;
	II(&Cut,			CutInst,				"Cut",				"") ;
	II(&PutCutLevel,	PutCutLevelInst,		"PutCutLevel",		"x") ;
 	II(&Fail,			FailInst,				"Fail",				"") ;


	II(&GetYVariable,	GetYVariableInst,		"GetYVariable",		"yx") ;
	II(&GetXValue,		GetXValueInst,			"GetXValue",		"xx") ;
	II(&GetYValue,		GetYValueInst,			"GetYValue",		"yx") ;
	II(&GetConstant,	GetConstantInst,		"GetConstant",		"cx") ;
	II(&GetNil,			GetNilInst,				"GetNil",			"x") ;
	II(&GetExtra,		GetExtraInst,			"GetExtra",			"cx") ;
	II(&GetStructure,	GetStructureInst,		"GetStructure",		"fx") ;
	II(&GetList,		GetListInst,			"GetList",			"x") ;
	
	II(&PutXVariable,	PutXVariableInst,		"PutXVariable",		"xx") ;
	II(&PutXVariableOne,PutXVariableOneInst,	"PutXVariableOne",	"x"	) ;
	II(&PutYVariable,	PutYVariableInst,		"PutYVariable",		"yx") ;
	II(&PutXValue,		PutXValueInst,			"PutXValue",		"xx") ;
	II(&PutYValue,		PutYValueInst,			"PutYValue",		"yx") ;
	II(&PutUnsafeValue,	PutUnsafeValueInst,		"PutUnsafeValue",	"yx") ;
	II(&PutConstant,	PutConstantInst,		"PutConstant",		"cx") ;
	II(&PutNil,			PutNilInst,				"PutNil",			"x") ;
	II(&PutExtra,		PutExtraInst,			"PutExtra",			"cx") ;
	II(&PutStructure,	PutStructureInst,		"PutStructure",		"fx") ;
	II(&PutList,		PutListInst,			"PutList",			"x"	) ;

	II(&UnifyVoid,		UnifyVoidInst,			"UnifyVoid",		"n") ;
	II(&UnifyVoidOne,	UnifyVoidOneInst,		"UnifyVoidOne",		"") ;
	II(&UnifyXVariable,	UnifyXVariableInst,		"UnifyXVariable",	"x") ;
	II(&UnifyYVariable,	UnifyYVariableInst,		"UnifyYVariable",	"y") ;
	II(&UnifyXLocalValue,UnifyXLocalValueInst,	"UnifyXLocalValue",	"x") ;
	II(&UnifyYLocalValue,UnifyYLocalValueInst,	"UnifyYLocalValue",	"y") ;
	II(&UnifyXValue,	UnifyXValueInst,		"UnifyXValue",		"x") ;	
	II(&UnifyYValue,	UnifyYValueInst,		"UnifyYValue",		"y"	) ;
	II(&UnifyConstant,	UnifyConstantInst,		"UnifyConstant",	"c") ;
	II(&UnifyNil,		UnifyNilInst,			"UnifyNil",			"") ;
	II(&UnifyExtra,		UnifyExtraInst,			"UnifyExtra",	"c") ;

	II(&BuildVoid,		BuildVoidInst,			"BuildVoid",		"n") ;
	II(&BuildVoidOne,	BuildVoidOneInst,		"BuildVoidOne",		"") ;
	II(&BuildXVariable,	BuildXVariableInst,		"BuildXVariable",	"x"	) ;
	II(&BuildYVariable,	BuildYVariableInst,		"BuildYVariable",	"y"	) ;
	II(&BuildXValue,	BuildXValueInst,		"BuildXValue",		"x") ;
	II(&BuildYValue,	BuildYValueInst,		"BuildYValue",		"y") ;
	II(&BuildXLocalValue,BuildXLocalValueInst,	"BuildXLocalValue",	"x"	) ;
	II(&BuildYLocalValue,BuildYLocalValueInst,	"BuildYLocalValue",	"y"	) ;
	II(&BuildConstant,	BuildConstantInst,		"BuildConstant",	"c") ;
	II(&BuildNil,		BuildNilInst,			"BuildNil",			"") ;
	II(&BuildExtra,		BuildExtraInst,			"BuildExtra",	"c") ;

	II(&TryMeElse,		TryMeElseInst,			"TryMeElse",		"ln") ;
	II(&RetryMeElse,	RetryMeElseInst,		"RetryMeElse",		"ln") ;
	II(&TrustMe,		TrustMeInst,			"TrustMe",			"0n") ;
	II(&Try,			TryInst,				"Try",				"l") ;
	II(&Retry,			RetryInst,				"Retry",			"l"	) ;
	II(&Trust,			TrustInst,				"Trust",			"l"	) ;
 	II(&DiscardAndFail,	DiscardAndFailInst,		"DiscardAndFail",	"") ;
	II(&SwitchOnTerm,	SwitchOnTermInst,		"SwitchOnTerm",		"llll") ;
	II(&SwitchOnConstant,SwitchOnConstantInst,	"SwitchOnConstant",	"H") ;
	II(&SwitchOnStructure,SwitchOnStructureInst,"SwitchOnStructure","H") ;

	II(&MakeIndex,		MakeIndexInst,			"MakeIndex",		"") ;
}

Bool GetInstInfo(Pt inst, CharPt *name, CharPt *types)
{
	register int i ;
	
	for( i = 0 ; i < nInsts ; i++ )
		if( insts[i].inst == inst )
		{
			*name = insts[i].name ;
			*types = insts[i].types ;
			return( true ) ;
		}
	return( false ) ;
}
