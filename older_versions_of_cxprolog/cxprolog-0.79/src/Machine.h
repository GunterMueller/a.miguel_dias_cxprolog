/*
 *   This file is part of the CxProlog system

 *   Machine.h
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
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
	Pt C, CC, CH ;
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
#define Y(idx)				( cHdl(E)[-(int)(idx)] )

#define OutTemp(tempIndex)	( X + tempIndex )
#define OutPerm(permIndex)	( WordsOf(Environment) + permIndex + 1 )

#define IsToTrailVar(v)		( Lt(B,v) || Lt(v,HB) )
#define IsGlobalVar(v)		Lt(v,H)
#define IsGlobalVarStrong(v) ( Le(stacksBegin,v) && Lt(v,H) )
#define IsLocalVar(v)		Le(H,v)
#define IsLocalVarStrong(v)	( Le(H,v) && Le(v,stacksEnd) )
#define IsCurrEnvVar(v)		( Lt(v,E) && IsLocalVar(v) )

/* TopOfLocalStack is only valid just after a predicate call. */
#define TopOfLocalStack()	( Lt(B,E) ? cPt(B) : (cPt(E) - cWord(CP[-1])) )

#define maxEnvSize			(maxVarsPerTerm + 10)

#define FTopOfLocalStack()	( Lt(B,E) ? cPt(B) : (cPt(E) - maxEnvSize) )
#define FreeSpaceOnStacks(r) ( Lt(B,E)							\
								? Df(B,H) - (r)					\
								: Df(E,H) - maxEnvSize - (r) ) 

#define ZEnsureFreeSpaceOnStacks(n)								\
						( FreeSpaceOnStacks(1024) < (n)			\
						  && ZControlStacksExpand(n - FreeSpaceOnStacks(1024)) )
#define CheckFreeSpaceOnStacks(n)								\
						if( FreeSpaceOnStacks(512) < (n) )		\
							Error("Global stack overflow")

#define TrailVar(v)		{ if( TR == trailEnd ) TrailExpand() ;	\
						  Push(TR,v) ; }

#define Assign(v,term)	{ if( IsToTrailVar(v) ) TrailVar(v)		\
						  SetVar(v,term) ; }

#define DoFail()		{ P = Bf(P) ; JumpNext() }

#define	JumpNext()		return ;
#define Jump(v)			{ (*cProc(v))() ; JumpNext() }
#define InstRun()		(*cProc(*P++))()
#define InstEncode(p)	cPt(p)

#define maxX				256

extern Hdl P, CP, H, HB, TR ;
extern EnvironmentPt E ;
extern ChoicePointPt B ;
extern Pt C, CC, CH, Z, X[] ;
extern Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
extern Pt PFailAddr ;
extern Size hostSpeed ;

typedef struct PrologHashElem
{
	Pt value ;
	Hdl address ;
	struct PrologHashElem* next ;
} PrologHashElem, *PrologHashElemPt, *PrologHashTable ;

#define PrologHash(t, size)		( (cWord(t) >> 4) & ((size) - 1) )

extern Pt
	Nop, Allocate, Deallocate, Proceed, DeallocProceed, LocalJump,
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
	PushHCtx, PopHCtx, EnterHCtx, ExitHCtx,

	MakeIndex, TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust,
	SwitchOnTermAux, SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
	SwitchOnAtomic, SwitchOnStructure, DiscardAndFail ;

Bool GetInstInfo(Pt inst, CharPt *name, CharPt *types) ;
void TrailSave(void) ;
void TrailRestore(void) ;
void TrailTidy(void) ;
void TrailExpand(void) ;
void XRegsShow(void) ;
void LocalStackShow(void) ;
void HSave(void) ;
void HRestore(void) ;
Size HGrown(void) ;
void GlobalStackShow(void) ;
Bool ZControlStacksExpand(Size ensureExtraSpace) ;
void ControlStacksAtomGCMark(void) ;
void MachineRun(void) ;
Bool MachineIsOn(void) ;
void CheckHost(void) ;
void ZCheckHostSpeed(void) ;
void UserModeInstructions(void) ;
void MachineInit(void) ;

#endif
