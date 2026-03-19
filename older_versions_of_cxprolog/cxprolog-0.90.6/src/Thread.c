/*
 *   This file is part of the CxProlog system

 *   Thread.c
 *   by A.Miguel Dias - 1993/07/15
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

/*	Threads organization: tree

	Killing a thread also kills all of its descendents
	ThreadTransfer always produces one of these results:
							going, completed, failed, killed, error
*/

#define initialStacksSize	16 K
#define initialTrailSize	256

typedef struct Thread
{
	ExtraDef(Thread) ;
	Hdl stacksBegin0, stacksEnd0 ;
	Hdl stacksBegin, stacksEnd ;
	Hdl trailBegin, trailEnd ;
	Hdl P, CP, H, TR ;
	FinalizerPt F ;
	ChoicePointPt B, R ;
	EnvironmentPt E ;
	Pt C, CC, CH, X[maxX] ;
	Pt startGoal, restartGoal ;
	struct Thread *father, *sons, *brother ;
	Bool result ;
} Thread, *ThreadPt ;

#define	cThreadPt(t)			((ThreadPt)(t))

#define ThreadStacksBegin0(th)	((th)->stacksBegin0)
#define ThreadStacksEnd0(th)	((th)->stacksEnd0)
#define ThreadStacksBegin(th)	((th)->stacksBegin)
#define ThreadStacksEnd(th)		((th)->stacksEnd)
#define ThreadTrailBegin(th)	((th)->trailBegin)
#define ThreadTrailEnd(th)		((th)->trailEnd)
#define ThreadReg(th,R)			((th)->R)
#define ThreadX(th,i)			((th)->X[i])
#define ThreadStartGoal(th)		((th)->startGoal)
#define ThreadRestartGoal(th)	((th)->restartGoal)
#define ThreadFather(th)		((th)->father)
#define ThreadSons(th)			((th)->sons)
#define ThreadNext(th)			((th)->brother)
#define ThreadResult(th)		((th)->result)

static ExtraTypePt threadType ;
static ThreadPt root = nil, active = nil ;

/* DATA */

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
	register int i ;
	ThreadReg(th,H) = ThreadStacksBegin(th) ;
	ThreadReg(th,TR) = ThreadTrailBegin(th) ;
	ThreadReg(th,F) = cFinalizerPt(ThreadTrailEnd(th)) ;
	ThreadReg(th,B) = cChoicePointPt(ThreadStacksEnd(th)) ;
	ThreadReg(th,R) = nil ;			/* Exception chain */
	ThreadReg(th,E) = maxAddr ;		/*  for TopOfLocalStack() */
	ThreadReg(th,P) = &NotRunning ;	/* nil would put nils on the local stack */
	ThreadReg(th,CP) = &NotRunning ;
	ThreadReg(th,C) = tNilAtom ;
	ThreadReg(th,CC) = tNilAtom ;
	ThreadReg(th,CH) = tNilAtom ;
	dotimes(i, maxX)
		ThreadX(th,i) = nil ;
}

static void ThreadRestore(register ThreadPt th)
{
	register int i ;
	stacksBegin0 = ThreadStacksBegin0(th) ;
	stacksEnd0 = ThreadStacksEnd0(th) ;
	stacksBegin = ThreadStacksBegin(th) ;
	stacksEnd = ThreadStacksEnd(th) ;
	trailBegin = ThreadTrailBegin(th) ;
	trailEnd = ThreadTrailEnd(th) ;
	H = ThreadReg(th,H) ;
	TR = ThreadReg(th,TR) ;
	F = ThreadReg(th,F) ;
	SetChoicePoint(ThreadReg(th,B)) ;
	R = ThreadReg(th,R) ;
	E = ThreadReg(th,E) ;
	P = ThreadReg(th,P) ;
	CP = ThreadReg(th,CP) ;
	C = ThreadReg(th,C) ;
	CC = ThreadReg(th,CC) ;
	CH = ThreadReg(th,CH) ;
	for( i = 0 ; (Xc(i) = ThreadX(th,i)) != nil ; i++ ) ;
	active = th ;
}

static void ThreadSave(register ThreadPt th)
{
	register int i ;
	ThreadStacksBegin0(th) = stacksBegin0 ;
	ThreadStacksEnd0(th) = stacksEnd0 ;
	ThreadStacksBegin(th) = stacksBegin ;
	ThreadStacksEnd(th) = stacksEnd ;
	ThreadTrailBegin(th) = trailBegin ;
	ThreadTrailEnd(th) = trailEnd ;
	ThreadReg(th,H) = H ;
	ThreadReg(th,TR) = TR ;
	ThreadReg(th,F) = F ;
	ThreadReg(th,B) = B ;
	ThreadReg(th,R) = R ;
	ThreadReg(th,E) = E ;
	ThreadReg(th,P) = P ;
	ThreadReg(th,CP) = CP ;
	ThreadReg(th,C) = C ;
	ThreadReg(th,CC) = CC ;
	ThreadReg(th,CH) = CH ;
	for( i = 0 ; (ThreadX(th,i) = Xc(i)) != nil ; i++ ) ;
}

static ThreadPt ThreadNew(Pt startGoal, Pt restartGoal)
{
	ThreadPt th = ExtraNew(threadType) ;
	ThreadStacksBegin0(th) = PrimitiveAllocateAndClear(initialStacksSize) ;
	ThreadStacksEnd0(th) = ThreadStacksBegin0(th) + initialStacksSize ;
	ThreadStacksBegin(th) = ThreadStacksBegin0(th) ;
	ThreadStacksEnd(th) = ThreadStacksEnd0(th) ;
	ThreadTrailBegin(th) = TempAllocate(initialTrailSize) ;
	ThreadTrailEnd(th) = ThreadTrailBegin(th) + initialTrailSize ;
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
	PrimitiveRelease(ThreadStacksBegin0(th)) ;
	Release(ThreadTrailBegin(th)) ;
	ReleaseTerm(ThreadStartGoal(th)) ;
	ReleaseTerm(ThreadRestartGoal(th)) ;
	ExtraDelete(threadType, th) ;
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

void ThreadRootCreate(Pt startGoal, Pt restartGoal)
{
	root = ThreadNew(startGoal, restartGoal) ;
	ThreadRestore(root) ;
}

void ThreadRootStart()
{
	if( root == nil || root != active )
		FatalError("Root thread not active in ThreadRootStart") ;
	ThreadSave(root) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadReset(root) ;
	ThreadRestore(root) ;
	CallProlog(ZPushTerm(ThreadStartGoal(root))) ;
	FatalError("The root thread has finished") ;
}

void ActiveThreadReplace(Pt startGoal, Pt restartGoal)
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadReplace") ;
	ThreadReplaceGoals(active, startGoal, restartGoal) ; 
}



/* CONTROL */

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
	if( P == &NotRunning )
		RunThread(ThreadStartGoal(th)) ;

/* This is a transfer_to_thread syncronize point */
/* Message from leaving thread to incoming thread */
	if( UnifyWithAtomic(X1, message) )
		EventContinue() ;
	else EventForceFail() ;
}

void ActiveThreadStart()
{
    if( active == nil )
        FatalError("No active thread in ActiveThreadStart") ;
    ThreadSave(active) ;    /* trailBegin, stacksBegin0 might have moved */
    ThreadReset(active) ;
    ThreadRestore(active) ;
    RunThread(ThreadStartGoal(active)) ;
 }
                                                                                
void ActiveThreadRestart()
{
    if( active == nil )
        FatalError("No active thread in ActiveThreadRestart") ;
    ThreadSave(active) ;    /* trailBegin, stacksBegin0 might have moved */
    ThreadReset(active) ;
    ThreadRestore(active) ;
    RunThread(ThreadRestartGoal(active)) ;
}

static void ActiveThreadFinish(Pt message)
{
	ThreadPt father = ThreadFather(active) ;
	ThreadSave(active) ;    /* trailBegin, stacksBegin0 might have moved */
	ThreadDelete(active) ;
	active = nil ;
	ThreadTransferTo(father, message) ;
}


/* STATISTICS */

static void AreaStatistics(CharPt name, Size space, Size used)
{
	Write("%30s: %5ldKB (%8ld bytes used)\n",
				name,
				WordsAsKBytes(space),
				WordsAsBytes(used)) ;
}

static void ThreadStatistics(ThreadPt th)
{
	if( th != root )
		Write("--- thread <%s> -------\n", XExtraAsStr(TagExtra(th))) ;
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
	Write("Statistics:\n") ;
	AreaStatistics("Code area", CodeAreaSize(), CodeAreaUsed()) ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadStatistics(root) ;
	AreaStatistics("Scratch pads", ScratchSize() + GStrSize() + BigStrSize(), 0) ;
	Write("%30s: %8gs.\n", "Total runtime", CpuTime()) ;
}


/* CXPROLOG C'BUILTINS */

static void PThreadCheck()
{
	MustBe( XExtraCheck(threadType, X0) )
}

static void PThreadNew()
{
	BindVarWithExtra(X0, ThreadNew(Drf(X1), Drf(X2))) ;
	JumpNext()
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
	JumpNext()
}

static void PActiveThread()
{
	MustBe( UnifyWithAtomic(X0, TagExtra(active)) )
}

static void PActiveThreadSetSuccess()
{
	ThreadResult(active) = true ;
	JumpNext() ;
}
                                                                                
static void PActiveThreadReturn()
{
    ActiveThreadFinish(ThreadResult(active) ? tCompletedAtom : tFailedAtom) ;
    InternalError("PActiveThreadFailed") ;
}

static void PStatistics()
{
	StatisticsShow() ;
	JumpNext()
}

static void PStatistics2()
{
	register CharPt name = XTestAtomName(X0) ;
	if( StrEqual(name, "runtime") ) {
		static long past = 0 ;
		long curr = CpuTime() * 1000 ;
		if( Unify(X1,
			MakeList(MakeInt(curr),
				MakeList(MakeInt(curr - past), tNilAtom))) ) {
			past = curr ;
			JumpNext()
		}
	}
	DoFail()
}

static void PMaxMem()
{
	ZEnsureFreeSpaceOnStacks(128 K, "$$_max_mem/2") ; /* stacks grow now */
	JumpNext()
}

static void PNDCurrentThread()
{
	PNDCurrentExtra(threadType, nil, 1) ;
	JumpNext()
}

static void ThreadsShow(register ThreadPt th, int n)
{
	int i ;
	AtomPt at = IVarWith(TagExtra(th)) ;
	dotimes(i, n) Write("  ") ;
	Write("%16s", TermAsStr(TagExtra(th))) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
	doseq(th, ThreadSons(th), ThreadNext(th))
		ThreadsShow(th, n+1) ;
}
static void PThreads()
{
	ShowVersion() ;
	Write("Threads:\n") ;
	ThreadsShow(root, 1) ;
	JumpNext()
}

void ThreadsInit()
{
	threadType = ExtraTypeNew("THREAD", WordsOf(Thread), nil) ;

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
	InstallNDeterCBuiltinPred("current_thread", 1, PNDCurrentThread) ;
	InstallCBuiltinPred("threads", 0, PThreads) ;
}
