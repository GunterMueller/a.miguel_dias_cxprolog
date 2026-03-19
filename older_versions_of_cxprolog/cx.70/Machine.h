/*
 *   This file is part of the CxProlog system

 *   Machine.h
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Machine_
#define _Machine_

/* FRAME DEFINITION E ACCESS */

typedef struct Environment
{
/*	Pt Y[] ;	*/
	struct Environment *E ;
	Hdl CP ;
	Pt CC ;
	struct ChoicePoint* B0 ;
} Environment, *EnvironmentPt ;
	
typedef struct ChoicePoint
{
	struct Environment *E ;
	Hdl CP ;
	struct ChoicePoint *B, *B0 ;
	Pt C, CC ;
	Hdl P, TR, H ;
/*	Pt A[] ;	*/
} ChoicePoint, *ChoicePointPt ;
	
#define cEnvironmentPt(x)	((EnvironmentPt)(x))
#define cChoicePointPt(x)	((ChoicePointPt)(x))

#define X(pt)				( *(pt) )
#define Xc(c)				( X[c] )
#define X0					Xc(0)
#define X1					Xc(1)
#define X2					Xc(2)
#define X3					Xc(3)
#define X4					Xc(4)
#define Ef(field)			( E[-1].field )
#define Bf(field)			( B->field )
#define A(idx)				( cHdl(B + 1)[idx] )
#define Y(idx)				( cHdl(E)[idx] )

#define IsToTrailVar(v)		( Lt(B,v) || Lt(v,HB) )
#define IsGlobalVar(v)		Lt(v, H)
#define IsLocalVar(v)		Le(H, v)
#define IsCurrEnvVar(v)		( Lt(v, E) && IsLocalVar(v) )

#define Assign(v, term)		if( IsToTrailVar(v) ) {					\
								TestTrailOverflow()					\
								Push(TR, cPt(v)) ;					\
								SetVar(v, term) ;					\
							}										\
							else SetVar(v, term)

#define Bind(v1,v2)			if( Lt(v1, v2) )							\
								if( IsGlobalVar(v1) ) Assign(v2, v1) ;	\
								else Assign(v1, v2) ;					\
							else										\
								if( IsGlobalVar(v2) ) Assign(v1, v2) ;	\
								else Assign(v2, v1)

#define RestoreTrail(base,v)								\
							while( TR != (base) ) {			\
								v = cVar(Pop(TR)) ;			\
								ResetVar(*v) ;				\
							}

#define DoFail()			{ P = Bf(P) ; JumpNext() }

#define CurrUnit()			(C == tNilAtom ? bottomUnit : cUnitPt(XPt(C)[-1]))

#define	JumpNext()	return ;
#define Jump(v)		{ (*cProc(v))() ; JumpNext() }
#define Run()		for(;;) (*cProc(*P++))()
#define Z(p)		cPt(p)

#define maxX	256

extern Hdl P, CP, H, HB, TR ;
extern EnvironmentPt E ;
extern ChoicePointPt B ;
extern Pt C, CC, X[] ;
extern Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
extern Pt PFailAddr ;

typedef struct PrologHashElem
{
	Pt value ;
	Hdl address ;
	struct PrologHashElem* next ;
} PrologHashElem, *PrologHashElemPt, *PrologHashTable ;

#define PrologHash(t, size)		( (cWord(t) >> 4) & ((size) - 1) )

extern Pt
	Nop, Allocate, Deallocate, Proceed, DeallocateAndProceed, LocalJump,
	Call, Execute, CallVar, ExecuteVar, CCall, CExecute, 
	SwitchContextAndCall, PushUnitAndCallVar, SwitchContextAndCallVar, EmptyContextCallVar,
	Cut, PutCutLevel, Fail, Undef,
	GetYVariable, GetXValue, GetYValue, GetAtomic, GetNil, GetExtra,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,
	PutXValue, PutYValue, PutUnsafeValue, PutAtomic, PutNil, PutExtra, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,
	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue,
	UnifyAtomic, UnifyNil, UnifyExtra, BuildVoid, BuildVoidOne, BuildXVariable,
	BuildYVariable, BuildXValue, BuildYValue, BuildXLocalValue,
	BuildYLocalValue, BuildAtomic, BuildNil, BuildExtra,
	TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust, SwitchOnTerm, DiscardAndFail,
	SwitchOnAtomic, SwitchOnStructure, MakeIndex ;

void InitMachine(void) ;
Bool GetInstInfo(Pt inst, CharPt *name, CharPt *types) ;
void RunMachine(void) ;
Bool MachineIsOn(void) ;
void SetTraceOnFlag(Bool) ;
void SetAttentionOnFlag(Bool) ;
Bool GetTraceOnFlag(void) ;
Bool GetAttentionOnFlag(void) ;

#endif
