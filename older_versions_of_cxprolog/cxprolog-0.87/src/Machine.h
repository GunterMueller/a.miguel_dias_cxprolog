/*
 *   This file is part of the CxProlog system

 *   Machine.h
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Machine_
#define _Machine_


/* CHOICEPOINTS */

typedef struct ChoicePoint /* LIVES ON LOCAL STACK */
{
	struct Environment *E ; /* must be first */
	Hdl CP ;
	struct ChoicePoint *B, *B0, *R ;
	Pt C, CC, CH ;
	Hdl P, TR, H ;
/*	Pt A[] ;	*/
} ChoicePoint, *ChoicePointPt ;

#define cChoicePointPt(x)	((ChoicePointPt)(x))

#define Bf(field)			( B->field )
#define A(idx)				( cHdl(B + 1)[idx] )

#define Rf(field)			( R->field )
#define RA(idx)				( cHdl(R + 1)[idx] )

#define SaveState(old, next, n)	{			\
				Bf(E) = E ;					\
				Bf(CP) = CP ;				\
				Bf(B) = (old) ;				\
				Bf(B0) = B0 ;				\
				Bf(R) = R ;					\
				Bf(C) = C ;					\
				Bf(CC) = CC ;				\
				Bf(CH) = CH ;				\
				Bf(P) = (next) ;			\
				Bf(TR) = TR ;				\
				HB = Bf(H) = H ;			\
				N = (n) ;					\
				while( N-- ) A(N) = Xc(N) ; }

#define RestoreState(n)	{					\
				E = Bf(E) ;					\
				CP = Bf(CP) ;				\
				B0 = Bf(B0) ;				\
				R = Bf(R) ;					\
				C = Bf(C) ;					\
				CC = Bf(CC) ;				\
				CH = Bf(CH) ;				\
				H = Bf(H) ;					\
				saveTR = Bf(TR) ;			\
				TrailRestore() ;			\
				N = (n) ;					\
				while( N-- ) Xc(N) = A(N) ; }

#define SetChoicePoint(b) {					\
				B = (b) ;					\
				HB = Bf(H) ; }


/* ENVIRONMENTS */

typedef struct Environment /* LIVES ON LOCAL STACK */
{
/*	Pt Y[] ; */
	struct Environment *E ; /* must be first */
	Hdl CP, P ;
	Pt CC ;
	struct ChoicePoint* B0 ;
} Environment, *EnvironmentPt ;	
	
#define cEnvironmentPt(x)	((EnvironmentPt)(x))

#define maxEnvSize			(maxVarsPerTerm + 10)

#define Ef(field)			( E[-1].field )
#define Y(idx)				( cHdl(E)[-(int)(idx)] )
#define OutPerm(permIndex)	( WordsOf(Environment) + permIndex + 1 )


/* CONTEXTS */

#define Z(idx)				( XStructArg(XListHead(C), idx) )
#define OutParam(paramIndex) ( paramIndex )


/* CUT FINALIZERS */

typedef struct Finalizer
{
	ChoicePointPt cp ;
	ProcV proc ;
	VoidPt arg ;
} Finalizer, *FinalizerPt ;

#define cFinalizerPt(x)		((FinalizerPt)(x))


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

extern Hdl P, CP, H, HB, TR, S ;
extern FinalizerPt F ;
extern EnvironmentPt E ;
extern ChoicePointPt B, B0, R ;
extern Pt C, CC, CH, D, Z, X[] ;
extern Hdl stacksBegin0, stacksEnd0 ;
extern Hdl stacksBegin, stacksEnd ;
extern Hdl trailBegin, trailEnd ;
extern Hdl saveTR ;
extern Size hostSpeed ;
extern int N ;


/* VARIABLES */

/* Parameter of ResetVar is not a Pt, is a proper Var */
#define ResetVar(v)			( *cHdl(v) = cPt(v) )

#define IsToTrailVar(v)		( Lt(v,HB) || Lt(B,v) )
#define IsLocalVar(v)		( Lt(H,v) )
#define IsCurrEnvVar(v)		( Lt(v,E) && IsLocalVar(v) )
#define PushTrail(v)		{ if( Eq(TR,F) ) TrailExpand() ;	\
							  Push(TR,v) ; }

#define IsLocalRef(v)		( Lt(H,v) && Le(v,stacksEnd) )
#define IsGlobalRef(v)		( Le(stacksBegin,v) && Le(v,H) )
#define IsTrailRef(v)		( Le(trailBegin,v) && Le(v,F) )

#define Assign(v,term)		{ if( IsToTrailVar(v) ) PushTrail(v) \
							  SetVar(v,term) ; }


/* STACKS */

#define memReserve		512

#define FreeSpace(reserve)										\
				( Lt(B,E) ? Df(B,H) - (reserve)					\
						  : Df(E,H) - maxEnvSize - (reserve) ) 
#define CheckFreeSpaceOnStacks(n)								\
				( FreeSpace(memReserve) < (n)					\
				  && Error("Global stack overflow") )

/* ZEnsureFreeSpaceOnStacks can only be used inside Pfunctions and Zfunctions */
#if 0
#define ZEnsureFreeSpaceOnStacks(n, where)						\
				( FreeSpace(memReserve*2) < (n)					\
				  && ZStacksExpansion(n - FreeSpace(memReserve*2), where) )
#else
#define ZEnsureFreeSpaceOnStacks(n, where)								\
				( FreeSpace(memReserve*2) < (n)							\
				  ? ZStacksExpansion(n-FreeSpace(memReserve*2), where)	\
				  : (testRelocation_flag && ZTestStacksExpansion(where)) )
#endif

/* TopOfLocalStack is valid only just after a pred call */
#define TopOfLocalStack()	( Lt(B,E) ? cHdl(B) : (cHdl(E)-cWord(CP[-1])) )



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

void MachineRun(void) ;
void CheckHost(void) ;
void ZCheckHostSpeed(void) ;

void TrailSave(void) ;
void TrailRestore(void) ;
void TrailExpand(void) ;

void HSave(void) ;
void HRestore(void) ;
Size HGrown(void) ;

Bool IsUnitInUse(UnitPt u) ;
Bool ZStacksExpansion(Size ensureThisExtraSpace, CharPt where) ;
Bool ZTestStacksExpansion(CharPt where) ;
void TestRelocationUpdateFlags(int newValue) ;
void TestGCUpdateFlags(int newValue) ;
void MachineInit(void) ;

#endif
