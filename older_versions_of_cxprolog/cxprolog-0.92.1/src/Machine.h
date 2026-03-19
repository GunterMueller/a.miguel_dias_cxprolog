/*
 *   This file is part of the CxProlog system

 *   Machine.h
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
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
	Hdl CP ;
	Pt CC ;
	struct ChoicePoint* B0 ;
} Environment, *EnvironmentPt ;	
	
#define cEnvironmentPt(x)	((EnvironmentPt)(x))

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
	VFunV proc ;
	VoidPt arg ;
} Finalizer, *FinalizerPt ;

#define cFinalizerPt(x)		((FinalizerPt)(x))


/* REGISTERS */

typedef union MixReg {
	Pt t ;
	Hdl h ;
	EnvironmentPt e ;
	FunctorPt f ;
	PredicatePt p ;
	ClausePt c ;
	UnitPt u ;
	Word w ;
} MixReg ;

#define maxX				256

#define X(pt)				( *(pt) )
#define Xc(c)				( X[c] )
#define X0					Xc(0)
#define X1					Xc(1)
#define X2					Xc(2)
#define X3					Xc(3)
#define X4					Xc(4)
#define X5					Xc(5)
#define X6					Xc(6)
#define X7					Xc(7)

#define OutTemp(tempIndex)	( X + tempIndex )

extern Hdl P, CP, H, HB, TR, S ;
extern EnvironmentPt E ;
extern ChoicePointPt B, B0, R ;
extern MixReg Q, Z ;
extern Pt C, CC, CH, D, X[] ;
extern Pt GlobalClock ;
extern FinalizerPt F ;
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
#define PushTrail(v)		{ if( Eq(TR,F) ) TrailExpand() ;		\
							  Push(TR,v) ; }

#define IsLocalRef(v)		( Lt(H,v) && Le(v,stacksEnd) )
#define IsGlobalRef(v)		( Le(stacksBegin,v) && Le(v,H) )
#define IsStacksRef(v)		( Le(stacksBegin,v) && Le(v,stacksEnd) )
#define IsTrailRef(v)		( Le(trailBegin,v) && Le(v,F) )

#define IsGlobalCompound(t)	( IsCompound(t) && IsGlobalRef(XPt(t)) )
#define IsAllocCompound(t)	( IsCompound(t) && !IsStacksRef(XPt(t)) )

#define Assign(v,term)		{ if( IsToTrailVar(v) ) PushTrail(v)	\
							  SetVar(v,term) ; }


/* STACKS */

/* Invariant: CP[-1] must represent the size of the curr env whenever E<=B */
#define TopOfLocalStack()	( Lt(B,E) ? cHdl(B) : (cHdl(E)-cWord(CP[-1])) )

/* Each C-builtin predicate can count on a 512-word reserve in the stacks.
   All the other predicates can count on a 512-word reserve to build terms,
   one choice point and one environment. If 512 words are not enough,
   the compiler inserts a EnsureFreeSpace instruction that checks if a
   stack expansion is necessary. */
#define memReserve		512
#define safeReserve		512

#define FreeSpace()			( Df(TopOfLocalStack(),H) - safeReserve ) 
#define FreeSpace2()		( FreeSpace() - memReserve ) 

#define CheckFreeSpaceOnStacks(n)									\
				Ignore( FreeSpace() < (n) && Error("Global stack overflow") )

/* ZEnsureFreeSpaceOnStacks can only be used inside Pfunctions and Zfunctions */
#if 0
#define ZEnsureFreeSpaceOnStacks(n, where)							\
				( FreeSpace2() < (n)								\
				  && ZStacksExpansion(n - FreeSpace2(), where) )
#else
#define ZEnsureFreeSpaceOnStacks(n, where)							\
				( FreeSpace2() < (n)								\
				  ? ZStacksExpansion(n - FreeSpace2(), where)		\
				  : (testRelocation_flag && ZTestStacksExpansion(where)) )
#endif


/* INTERPRETER */

/* experimental */
#define USE_THREADED_CODE	0

#if USE_THREADED_CODE && __i386__ && __GNUC__ == 3
	#define JumpNext()		Do( asm("movl P, %eax") ; asm("addl $4, P") ;	\
								asm("leave") ;								\
								asm("jmp *(%eax)") ; return ; )
	#define Jump(v)			Do( asm("leave") ;								\
								asm("jmp *(%0)" : : "g" (v)) ; return ; )
	#define InstRun()		JumpNext()
#elif USE_THREADED_CODE && __i386__ && __GNUC__ == 4
	#define JumpNext()		Do( asm("movl _P, %eax") ; asm("addl $4, _P") ;	\
                                asm("movl -4(%ebp), %ebx") ; asm("leave") ;	\
								asm("jmp *(%eax)") ; return ; )
	#define Jump(v)			Do( asm("movl -4(%ebp), %ebx") ; asm("leave") ;	\
								asm("jmp *(%0)" : : "g" (v)) ; return ; )
	#define InstRun()		JumpNext()
#else
	#undef USE_THREADED_CODE
	#define	JumpNext()		return
	#define Jump(v)			Do( (*cVFun(v))() ; return ; )
	#define InstRun()		(*cVFun(*P++))()
#endif

#define DoFail()		Do( P = Bf(P) ; JumpNext() ; )
#define Ensure(c)		Do( if( !(c) ) DoFail() ; )
#define MustBe(c)		Do( if( c ) JumpNext() ; else DoFail() ; )
 
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

typedef void (*RelocateStacksProc)(Size, Size) ;
void InstallRelocateStacksHandler(RelocateStacksProc p) ;

void TrailIVar(AtomPt atom, Pt value) ;
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
