/*
 *   This file is part of the CxProlog system

 *   Thread.c
 *   by A.Miguel Dias - 1993/07/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2010 A.Miguel Dias, CITI, DI/FCT/UNL

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

/*	Threads organization: tree

	Killing a thread also kills all of its descendents
	ThreadTransfer always produces one of these results:
							going, completed, failed, killed, error
*/

/* @@@ Basic garbage collection may need to be revised for inactive threads */

#define initialStacksCapacity	(stacks_debugging ? 32 K K : 16 K)
#define initialTrailCapacity	(trail_debugging ? 128 K : 256)

typedef struct Thread {
	ExtraDef(Thread) ;
	Hdl stacksBegin0, stacksEnd0 ;
	Hdl stacksBegin, stacksEnd ;
	Hdl trailBegin, trailEnd ;
	Hdl P, CP, H, HB, TR ;
	FinalizerPt F ;
	ChoicePointPt B, B0, R, L ;
	EnvironmentPt E ;
	Pt C, CH, X[maxX] ;
	Pt startGoal, restartGoal ;
	struct Thread *father, *sons, *brother ;
	Bool result ;
} Thread ;

#define	cThreadPt(t)			((ThreadPt)(t))

#define ThreadStacksBegin0(th)	((th)->stacksBegin0)
#define ThreadStacksEnd0(th)	((th)->stacksEnd0)
#define ThreadStacksBegin(th)	((th)->stacksBegin)
#define ThreadStacksEnd(th)		((th)->stacksEnd)
#define ThreadTrailBegin(th)	((th)->trailBegin)
#define ThreadTrailEnd(th)		((th)->trailEnd)
#define ThreadReg(th,R)			((th)->R)
#define ThreadX(th)				((th)->X)
#define ThreadStartGoal(th)		((th)->startGoal)
#define ThreadRestartGoal(th)	((th)->restartGoal)
#define ThreadFather(th)		((th)->father)
#define ThreadSons(th)			((th)->sons)
#define ThreadNext(th)			((th)->brother)
#define ThreadResult(th)		((th)->result)

static ExtraTypePt threadType ;
static ThreadPt root = nil, active = nil ;
static InstPt NotRunningAddr ;

/* PRIVATE */

static Bool IsAncestor(ThreadPt a, ThreadPt b)
{
	ThreadPt th ;
	if( a == b )
		return true ;
	doseq(th, ThreadSons(a), ThreadNext(th))
		if( IsAncestor(th, b) )
			return true ;
	return false ;
}

static void ThreadReset(register ThreadPt th)
{
	ThreadReg(th,H) = ThreadStacksBegin(th) + 3 ;	/* Initial context is [main] */
	ThreadReg(th,HB) = ThreadStacksBegin(th) ;
	ThreadReg(th,TR) = ThreadTrailBegin(th) ;
	ThreadReg(th,F) = cFinalizerPt(ThreadTrailEnd(th)) ;
	ThreadReg(th,B) = cChoicePointPt(ThreadStacksEnd(th)) - 1 ; /* Space for a CP, required */
	ChoicePointField(ThreadReg(th,B), C) = TagList(ThreadStacksBegin(th) + 1) ; /* Required by GC */
	*cHdl(ThreadReg(th,B)) = endOfChainMark ;	/* Fill end-of-chains slot */	
	ThreadReg(th,B)->H = ThreadReg(th,HB) ;		/* Will be restored at end of execution */
	ThreadReg(th,B0) = ThreadReg(th,B) ;
	ThreadReg(th,R) = ThreadReg(th,B) ;
	ThreadReg(th,L) = ThreadReg(th,B) ;
	ThreadReg(th,E) = cEnvironmentPt(ThreadStacksEnd(th)-1) ; /* B<E for TopOfLocalStack(), within */
	*cHdl(ThreadReg(th,E)) = endOfChainMark ;	/* Fill end-of-chains slot */
	ThreadReg(th,P) = NotRunningAddr ;	/* Flag and does not put nil on stacks */
	ThreadReg(th,CP) = NotRunningAddr ; /* It's ok because B is on top */
	ThreadReg(th,C) = TagList(ThreadReg(th,H)-2) ;	/* Initial context is [main] */
	ThreadReg(th,CH) = tNilAtom ;
	EmptyRangeN(ThreadX(th), maxX) ; /* This is needed */
}

static void ThreadRestore(register ThreadPt th)
{
	stacksBegin0 = ThreadStacksBegin0(th) ;
	stacksEnd0 = ThreadStacksEnd0(th) ;
	stacksBegin = ThreadStacksBegin(th) ;
	stacksEnd = ThreadStacksEnd(th) ;
	trailBegin = ThreadTrailBegin(th) ;
	trailEnd = ThreadTrailEnd(th) ;

	H = ThreadReg(th,H) ;
	HB = ThreadReg(th,HB) ;
	TR = ThreadReg(th,TR) ;
	F = ThreadReg(th,F) ;
	B = ThreadReg(th,B) ;
	B0 = ThreadReg(th,B0) ;
	R = ThreadReg(th,R) ;
	L = ThreadReg(th,L) ;
	E = ThreadReg(th,E) ;
	P = ThreadReg(th,P) ;
	CP = ThreadReg(th,CP) ;
	C = ThreadReg(th,C) ;
	CH = ThreadReg(th,CH) ;
	CopyUntilEmpty(X, ThreadX(th)) ;
	active = th ;
}

static void ThreadSave(register ThreadPt th)
{
	ThreadStacksBegin0(th) = stacksBegin0 ;
	ThreadStacksEnd0(th) = stacksEnd0 ;
	ThreadStacksBegin(th) = stacksBegin ;
	ThreadStacksEnd(th) = stacksEnd ;
	ThreadTrailBegin(th) = trailBegin ;
	ThreadTrailEnd(th) = trailEnd ;

	ThreadReg(th,H) = H ;
	ThreadReg(th,HB) = HB ;
	ThreadReg(th,TR) = TR ;
	ThreadReg(th,F) = F ;
	ThreadReg(th,B) = B ;
	ThreadReg(th,B0) = B0 ;
	ThreadReg(th,R) = R ;
	ThreadReg(th,L) = L ;
	ThreadReg(th,E) = E ;
	ThreadReg(th,P) = P ;
	ThreadReg(th,CP) = CP ;
	ThreadReg(th,C) = C ;
	ThreadReg(th,CH) = CH ;
	CopyUntilEmpty(ThreadX(th), X) ;
}

static ThreadPt ThreadNew(Pt startGoal, Pt restartGoal)
{
	ThreadPt th = ExtraNew(threadType, 0) ;
	ThreadStacksBegin0(th) =
		AllocateSegmentEmpty(initialStacksCapacity, &ThreadStacksEnd0(th)) ;
	ThreadStacksBegin(th) = ThreadStacksBegin0(th) ;
	ThreadStacksEnd(th) = ThreadStacksEnd0(th) ;
	ThreadTrailBegin(th) =
		AllocateSegmentEmpty(initialTrailCapacity, &ThreadTrailEnd(th)) ;

/* Setup the first three words of the global stack, the initial [main] context. */
	Hdl h = ThreadStacksBegin(th) ;
	h[0] = cPt(TermToUnit(tMainAtom, nil)) ; /* Unit is ref stored in the global stack */
	h[1] = tMainAtom ;					/* Link unit-term in the context list */
	h[2] = tNilAtom ;
	
	ThreadReset(th) ;
	ThreadStartGoal(th) = AllocateTermForAssign(startGoal) ;
	ThreadRestartGoal(th) = AllocateTermForAssign(restartGoal) ;
	ThreadFather(th) = active ;
	ThreadSons(th) = nil ;
	ThreadNext(th) = nil ;
	ThreadResult(th) = false ;
	if( active != nil ) {
		ThreadNext(th) = ThreadSons(active) ;
		ThreadSons(active) = th ;
	}
	return th ;
}

static void ThreadRelease(ThreadPt th)
{
	register ThreadPt t ;
	doseq(t, ThreadSons(th), ThreadNext(t))
		ThreadRelease(t) ;
	ReleaseSegment(ThreadStacksBegin0(th), ThreadStacksEnd0(th) - ThreadStacksBegin0(th)) ;
	ReleaseSegment(ThreadTrailBegin(th), ThreadTrailEnd(th) - ThreadTrailBegin(th)) ;
	ReleaseTerm(ThreadStartGoal(th)) ;
	ReleaseTerm(ThreadRestartGoal(th)) ;
}

static void ThreadDelete(ThreadPt th)
{
	if( ThreadSons(ThreadFather(th)) == th )	/* first is removed */
		ThreadSons(ThreadFather(th)) = ThreadNext(th) ;
	else {
		register ThreadPt x ;
		for( x = ThreadSons(ThreadFather(th)) ;
							ThreadNext(x) != th ; x = ThreadNext(x) ) ;
		ThreadNext(x) = ThreadNext(th) ;
	}
	ThreadRelease(th) ;
}

static void ThreadReplaceGoals(ThreadPt th, Pt startGoal, Pt restartGoal)
{
	ReleaseTerm(ThreadStartGoal(th)) ;
	ThreadStartGoal(th) = AllocateTermForAssign(startGoal) ;
	ReleaseTerm(ThreadRestartGoal(th)) ;
	ThreadRestartGoal(th) = AllocateTermForAssign(restartGoal) ;
}

static Size ThreadSizeFun(CVoidPt x)
{
	Unused(x) ;
	return WordsOf(Thread) ;
}

static void RunThread(Pt goal)
{
	CheckPredicate(XTestFunctor(goal)) ;
	X0 = ZPushTerm(goal) ;
	P = PredCode(CheckPredicateByName("$$_run_thread", 1)) ;
	EventContinue() ;
}

static void ThreadTransferTo(ThreadPt th, Pt message)
{
	if( active != nil )
		ThreadSave(active) ;
	ThreadRestore(th) ;
	if( P == NotRunningAddr )
		RunThread(ThreadStartGoal(th)) ;

/* This is a transfer_to_thread syncronize point */
/* Message from leaving thread to incoming thread */
	if( UnifyWithAtomic(X1, message) )
		EventContinue() ;
	else EventForceFail() ;
}


/* PUBLIC */

void ThreadRootCreate(Pt startGoal, Pt restartGoal)
{
	if( root != nil )
		FatalError("The root thread can only be created once") ;
	active = root = ThreadNew(startGoal, restartGoal) ;
	ExtraPermanent(root) ;
	ThreadRestore(root) ;
}

void ActiveThreadReplace(Pt startGoal, Pt restartGoal)
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadReplace") ;
	ThreadReplaceGoals(active, startGoal, restartGoal) ;
}

void ActiveThreadReset()
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadReset") ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadReset(active) ;
	ThreadRestore(active) ;
}

void ActiveThreadStart()
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadStart") ;
	ActiveThreadReset() ;
	RunThread(ThreadStartGoal(active)) ;
}

void ActiveThreadRestart()
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadRestart") ;
	ActiveThreadReset() ;
	RunThread(ThreadRestartGoal(active)) ;
}

static void ActiveThreadFinish(Pt message)
{
	ThreadPt father = ThreadFather(active) ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadDelete(active) ;
	active = nil ;
	ThreadTransferTo(father, message) ;
}


/* STATISTICS */

static void AreaStatistics(CharPt name, Size space, Size used)
{
	Write("%30s: %6ldKB (%8ld bytes used)\n",
				name,
				WordsAsKBytes(space),
				WordsAsBytes(used)) ;
}

static void ThreadStatistics(ThreadPt th)
{
	if( th != root )
		Write("--- thread <%s> -------\n", ExtraAsStr(th)) ;
	AreaStatistics("Global stack + Local stack",
					ThreadStacksEnd(th) - ThreadStacksBegin(th),
					(ThreadReg(th,H) - ThreadStacksBegin(th))
						+ (ThreadStacksEnd(th)
						- Min(cHdl(ThreadReg(th,B)), cHdl(ThreadReg(th,E))))) ;
	AreaStatistics("Trail stack",
				ThreadTrailEnd(th) - ThreadTrailBegin(th),
				ThreadReg(th,TR) - ThreadTrailBegin(th)) ;
	doseq(th, ThreadSons(th), ThreadNext(th))
		ThreadStatistics(th) ;
}

void StatisticsShow()
{
	ShowVersion() ;
	Write("STATISTICS:\n") ;
	AreaStatistics("Heap", CodeAreaSize(), CodeAreaUsed()) ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadStatistics(root) ;
	AreaStatistics("Scratch pads", ScratchCapacity()
							+ GStrCapacity() + BigStrCapacity(), 0) ;
	Write("%30s: %8g sec\n", "Total runtime", CpuTime()) ;
#if 0
	Write("\n") ;
	Write("%30s: %5ld\n", "Number of Units", CountUnits()) ;
	Write("%30s: %5ld\n", "Number of Predicates", CountPredicates()) ;
	Write("%30s: %5ld\n", "Number of Clauses", CountClauses()) ;
#endif
}


/* CXPROLOG C'BUILTINS */

static void PThreadCheck()
{
	MustBe( XExtraCheck(threadType, X0) ) ;
}

static void PThreadNew()
{
	BindVarWithExtra(X0, ThreadNew(Drf(X1), Drf(X2))) ;
	JumpNext() ;
}

static void PThreadTransfer()
{
	ThreadTransferTo(XTestExtra(threadType,X0), tGoingAtom) ;
	InternalError("PThreadTransfer") ;
}

static void PThreadKill()
{
	ThreadPt th = XTestExtra(threadType,X0) ;
	if( th == root )
		ImperativeError("The root thread cannot be killed") ;
	elif( th == active )
		ActiveThreadFinish(tKilledAtom) ;
	elif( IsAncestor(th, active) )
		ImperativeError("A thread cannot kill an ancestor thread") ;
	else ThreadDelete(th) ;
	JumpNext() ;
}

static void PActiveThread()
{
	MustBe( UnifyWithAtomic(X0, TagExtra(threadType, active)) ) ;
}

static void PActiveThreadSetSuccess()
{
	ThreadResult(active) = true ;
	JumpNext() ; ;
}

static void PActiveThreadReturn()
{
	ActiveThreadFinish(ThreadResult(active) ? tCompletedAtom : tFailedAtom) ;
	InternalError("PActiveThreadFailed") ;
}

static void PStatistics()
{
	StatisticsShow() ;
	JumpNext() ;
}

static void PStatistics2()
{
	register CharPt name = XTestAtomName(X0) ;
	if( StrEqual(name, "runtime") ) {
		static long past = 0 ;
		long curr = (long)(CpuTime() * 1000) ;
		if( Unify(X1,
			MakeList(MakeInt(curr),
				MakeList(MakeInt(curr - past), tNilAtom))) ) {
			past = curr ;
			JumpNext() ;
		}
	}
	DoFail() ;
}

static void PMaxMem()
{
	ZEnsureFreeSpaceOnStacks(128 K, -1, false) ; /* stacks grow now */
	JumpNext() ;
}

static void PNDCurrentThread()
{
	ExtraPNDCurrent(threadType, nil, 1, 0) ;
	JumpNext() ;
}

static void ThreadsShow(register ThreadPt th, int n)
{
	int i ;
	dotimes(i, n) Write("  ") ;
	Write("%16s", ExtraAsStr(th)) ;
	IVarsWithWrite(TagExtra(threadType, th)) ;
	Write("\n") ;
	doseq(th, ThreadSons(th), ThreadNext(th))
		ThreadsShow(th, n+1) ;
}
static void PThreads()
{
	ShowVersion() ;
	Write("THREADS:\n") ;
	ThreadsShow(root, 1) ;
	JumpNext() ;
}

void ThreadsInit()
{
	threadType = ExtraTypeNew("THREAD", ThreadSizeFun, nil, nil, 1) ;

	NotRunningAddr = Allocate(1, false) ;
	NotRunningAddr[0] = Fail ;

	InstallCBuiltinPred("thread", 1, PThreadCheck) ;
	InstallCBuiltinPred("thread_new", 3, PThreadNew) ;
	InstallCBuiltinPred("thread_transfer", 2, PThreadTransfer) ;
	InstallCBuiltinPred("thread_kill", 1, PThreadKill) ;
	InstallCBuiltinPred("active_thread", 1, PActiveThread) ;
	InstallCBuiltinPred("$$_active_thread_set_success", 0, PActiveThreadSetSuccess) ;
	InstallCBuiltinPred("$$_active_thread_return", 0, PActiveThreadReturn) ;
	InstallCBuiltinPred("statistics", 0, PStatistics) ;
	InstallCBuiltinPred("statistics", 2, PStatistics2) ;
	InstallCBuiltinPred("$$_max_mem", 0, PMaxMem) ;
	InstallGNDeterCBuiltinPred("current_thread", 1, 2, PNDCurrentThread) ;
	InstallCBuiltinPred("threads", 0, PThreads) ;
}
