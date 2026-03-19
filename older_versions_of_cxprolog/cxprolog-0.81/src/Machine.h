/*
 *   This file is part of the CxProlog system

 *   Machine.h
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Machine_
#define _Machine_


/* CHOICEPOINTS */

typedef struct ChoicePoint /* LIVES ON LOCAL STACK */
{
	struct Environment *E ;
	Hdl CP ;
	struct ChoicePoint *B, *B0 ;
	Pt C, CC, CH ;
	Hdl P, TR, H ;
/*	Pt A[] ;	*/
} ChoicePoint, *ChoicePointPt ;

#define cChoicePointPt(x)	((ChoicePointPt)(x))

#define Bf(field)			( B->field )
#define A(idx)				( cHdl(B + 1)[idx] )

#define SaveState(old, next, n)	{			\
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
				N = (n) ;					\
				while( N-- ) A(N) = Xc(N) ; }

#define RestoreState(n)	{					\
				E = Bf(E) ;					\
				CP = Bf(CP) ;				\
				B0 = Bf(B0) ;				\
				C = Bf(C) ;					\
				CC = Bf(CC) ;				\
				CH = Bf(CH) ;				\
				H = Bf(H) ;					\
				saveTR = Bf(TR) ;			\
				TrailRestore() ;			\
				N = (n) ;					\
				while( N-- ) Xc(N) = A(N) ; }

#define SetChoicePoint(b) {					\
				B = b ;						\
				HB = Bf(H) ; }


/* ENVIRONMENTS */

typedef struct Environment /* LIVES ON LOCAL STACK */
{
/*	Pt Y[] ;	*/
	struct Environment *E ;
	Hdl CP ;
	Pt CC ;
	struct ChoicePoint* B0 ;
} Environment, *EnvironmentPt ;	
	
#define cEnvironmentPt(x)	((EnvironmentPt)(x))

#define maxEnvSize			(maxVarsPerTerm + 10)

#define Ef(field)			( E[-1].field )
#define Y(idx)				( cHdl(E)[-(int)(idx)] )
#define OutPerm(permIndex)	( WordsOf(Environment) + permIndex + 1 )


/* REGISTERS */

#define maxX				256

#define X(pt)				( *(pt) )
#define Xc(c)				( X[c] )
#define X0					Xc(0)
#define X1					Xc(1)
#define X2					Xc(2)
#define X3					Xc(3)
#define X4					Xc(4)

#define OutTemp(tempIndex)	( X + tempIndex )

extern Hdl P, CP, H, HB, TR ;
extern EnvironmentPt E ;
extern ChoicePointPt B, B0 ;
extern Pt C, CC, CH, Z, X[] ;
extern Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
extern ChoicePointPt saveB ;
extern Hdl saveTR ;
extern Size hostSpeed ;
extern int N ;


/* VARIABLES */

#define IsToTrailVar(v)		( Lt(B,v) || Lt(v,HB) )
#define IsGlobalVar(v)		( Lt(v,H) )
#define IsGlobalVarStrong(v) ( Le(stacksBegin,v) && Lt(v,H) )
#define IsLocalVar(v)		( Le(H,v) )
#define IsLocalVarStrong(v)	( Le(H,v) && Le(v,stacksEnd) )
#define IsCurrEnvVar(v)		( Lt(v,E) && IsLocalVar(v) )
#define TrailVar(v)			{ if( TR == trailEnd ) TrailExpand() ;	\
							  Push(TR,v) ; }
#define Assign(v,term)		{ if( IsToTrailVar(v) ) TrailVar(v)		\
							  SetVar(v,term) ; }


/* STACKS */

#define FreeSpaceOnStacks(reserve)						\
				( Lt(B,E) ? Df(B,H) - (reserve)			\
						  : Df(E,H) - maxEnvSize - (reserve) ) 
#define CheckFreeSpaceOnStacks(n)						\
				( FreeSpaceOnStacks(512) < (n)			\
				  && Error("Global stack overflow") )

/* ZEnsureFreeSpaceOnStacks pode ser usado em Pfunctions e Zfunctions */
#define ZEnsureFreeSpaceOnStacks(n)						\
				( FreeSpaceOnStacks(1024) < (n)			\
				  && ZControlStacksExpand(n - FreeSpaceOnStacks(1024)) )

/* TopOfLocalStack is only valid just after a predicate call. */
#define TopOfLocalStack()	( Lt(B,E) ? cPt(B) : (cPt(E) - cWord(CP[-1])) )



/* INTERPRETER */

#define DoFail()		{ P = Bf(P) ; JumpNext() }

#define	JumpNext()		return ;
#define Jump(v)			{ (*cProc(v))() ; JumpNext() }
#define InstRun()		(*cProc(*P++))()
#define InstEncode(p)	cPt(p)

typedef struct PrologHashElem
{
	Pt value ;
	Hdl address ;
	struct PrologHashElem* next ;
} PrologHashElem, *PrologHashElemPt, *PrologHashTable ;

#define PrologHash(t, size)		( (cWord(t) >> 4) & ((size) - 1) )

extern Pt
	Nop, FAllocate, FDeallocate, Proceed, DeallocProceed, LocalJump,
	Call, Execute, CallVar, ExecuteVar,
	Cut, PutCutLevel, Fail, Undef, NotRunning,

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
Bool ZControlStacksExpand(Size ensureThisExtraSpace) ;
void ControlStacksAtomGCMark(void) ;
void MachineRun(void) ;
void CheckHost(void) ;
void ZCheckHostSpeed(void) ;
void UserModeInstructions(void) ;
void MachineInit(void) ;

#endif
