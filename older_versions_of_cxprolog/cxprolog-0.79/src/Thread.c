/*
 *   This file is part of the CxProlog system

 *   Thread.c
 *   by A.Miguel Dias - 1993/07/15
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

/*	Threads organization: tree

	Killing a thread also kills all of its descendents
	ThreadTransfer always produces one of these results:
							going, completed, failed, killed, error
*/

/* @@@ Memory management not finished yet @@ */

typedef struct Thread
{
	Word tagHolder ;
	Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
	Hdl P, CP, H, TR ;
	EnvironmentPt E ;
	ChoicePointPt B ;
	Pt C, CC, CH, X[maxX] ;
	Pt startGoal, restartGoal ;
	struct Thread *father, *sons, *brother ;
} Thread, *ThreadPt ;

#define	cThreadPt(t)				((ThreadPt)(t))

#define ThreadTagHolder(th)		((th)->tagHolder)
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

static ThreadPt root = nil, active = nil ;
static ThreadPt freeThreadList = nil ;

static Bool IsAncestor(ThreadPt a, ThreadPt b)
{
	ThreadPt th ;
	if( a == b )
		return true ;
	dolist(th, ThreadSons(a), ThreadNext(th))
		if( IsAncestor(th, b) )
			return true ;
	return false ;
}

static void ThreadRelease(ThreadPt th)
{
	ThreadPt t ;
	dolist(t, ThreadSons(th), ThreadNext(t))
		ThreadRelease(t) ;
	PrimitiveRelease(ThreadStacksBegin(th)) ;
	PrimitiveRelease(ThreadTrailBegin(th)) ;
	ThreadTrailBegin(th) = nil ;	/* Mark "queue not in use" */
	ThreadSons(th) = freeThreadList ;
	freeThreadList = th ;
}

static void ThreadDelete(ThreadPt th)
{
	if( ThreadSons(ThreadFather(th)) == th )	/* first is removed */
		ThreadSons(ThreadFather(th)) = ThreadNext(th) ;
	else {
		register ThreadPt x ;
		for( x = ThreadSons(ThreadFather(th)) ; ThreadNext(x) != th ; x = ThreadNext(x) ) ;
		ThreadNext(x) = ThreadNext(th) ;
	}
	ThreadRelease(th) ;
}

static ThreadPt UsingThread(ThreadPt th)
{
	if( ThreadStacksBegin(th) == nil )
		Error("Invalid operation over deleted thread %s", XExtraAsStr(TagExtra(th))) ;
	return th ;
}

static void ThreadReset(register ThreadPt th)
{
	register int i ;
	ThreadReg(th,H) = ThreadStacksBegin(th) ;
	ThreadReg(th,TR) = ThreadTrailBegin(th) ;
	ThreadReg(th,B) = cChoicePointPt(ThreadStacksEnd(th)) ; /* for TopOfLocalStack() */
	ThreadReg(th,E) = cEnvironmentPt(ThreadStacksEnd(th)) + 1 ;
	ThreadReg(th,P) = nil ;
	ThreadReg(th,CP) = nil ;
	ThreadReg(th,C) = tNilAtom ;
	ThreadReg(th,CC) = tNilAtom ;
	ThreadReg(th,CH) = tNilAtom ;
	dotimes(i, maxX)
		ThreadX(th,i) = nil ;
}

static void ThreadRestore(register ThreadPt th)
{
	register int i ;
	stacksBegin = ThreadStacksBegin(th) ;
	stacksEnd = ThreadStacksEnd(th) ;
	trailBegin = ThreadTrailBegin(th) ;
	trailEnd = ThreadTrailEnd(th) ;
	H = ThreadReg(th,H) ;
	TR = ThreadReg(th,TR) ;
	B = ThreadReg(th,B) ;
	HB = Bf(H) ;
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
	ThreadStacksBegin(th) = stacksBegin ;
	ThreadStacksEnd(th) = stacksEnd ;
	ThreadTrailBegin(th) = trailBegin ;
	ThreadTrailEnd(th) = trailEnd ;
	ThreadReg(th,H) = H ;
	ThreadReg(th,TR) = TR ;
	ThreadReg(th,B) = B ;
	ThreadReg(th,E) = E ;
	ThreadReg(th,P) = P ;
	ThreadReg(th,CP) = CP ;
	ThreadReg(th,C) = C ;
	ThreadReg(th,CC) = CC ;
	ThreadReg(th,CH) = CH ;
	for( i = 0 ; (ThreadX(th,i) = Xc(i)) != nil ; i++ ) ;
}

static void CalledFromC(Pt goal)
{
	PredicatePt pr = FindPredicate(XTestFunctor(goal)) ;
/*	if( pr == nil || PredIsUndefined(pr) )
		FatalError("Missing startup predicate '%s'", UPredNameArity(pr)) ;*/
	pr = FindPredicateByName("$$_called_from_c", 1) ;
	if( pr == nil || PredIsUndefined(pr) )
		FatalError("Missing predicate '%s'", UPredNameArity(pr)) ;
	X0 = goal ;
	P = PredCode(pr) ;
}

static ThreadPt ThreadNew(Pt startGoal, Pt restartGoal)
{
	register ThreadPt th = TempBlockAllocate(WordsOf(Thread)) ;
	ThreadTagHolder(th) = threadSubTag ;
	ThreadStacksBegin(th) = PrimitiveAllocate(16 K) ;
	ThreadStacksEnd(th) = ThreadStacksBegin(th) + 16 K ;
	ThreadTrailBegin(th) = PrimitiveAllocate(256) ;
	ThreadTrailEnd(th) = ThreadTrailBegin(th) + 256 ;
	ThreadReset(th) ;
	ThreadStartGoal(th) = AllocateTermForAssign(startGoal) ;
	ThreadRestartGoal(th) = AllocateTermForAssign(restartGoal) ;
	ThreadFather(th) = active ;
	ThreadSons(th) = nil ;
	ThreadNext(th) = nil ;

	if( active != nil ) {
		ThreadNext(th) = ThreadSons(active) ;
		ThreadSons(active) = th ;
	}
	return th ;
}

static void ThreadTransfer(ThreadPt th, Pt message)
{
	if( active != nil ) ThreadSave(active) ;
	ThreadRestore(th) ;
	if( P == nil )
		CalledFromC(ZPushTerm(ThreadStartGoal(th))) ;
	else		/* here is a transfer_to_thread syncronize point */
		if( not UnifyWithAtomic(X1, message) )
			EventForceFail() ;
	EventContinue() ;
}

void ThreadCreateRoot(Pt startGoal, Pt restartGoal)
{
	root = ThreadNew(startGoal, restartGoal) ;
	ThreadRestore(root) ;
}

void ThreadActivateRoot()
{
	ThreadTransfer(root, tGoingAtom) ;
}

void ActiveThreadRestart()
{
	ThreadSave(active) ;	/* trailBegin, stacksBegin might have moved */
	ThreadReset(active) ;
	ThreadRestore(active) ;
	CalledFromC(ZPushTerm(ThreadRestartGoal(active))) ;
	EventContinue() ;
}

static void ActiveThreadFinish(Pt message)
{
	if( active == root )
		FatalError("The root thread has finished") ;
	else {
		ThreadPt father = ThreadFather(active) ;
		ThreadSave(active) ;	/* trailBegin, stacksBegin might have moved */
		ThreadDelete(active) ;
		active = nil ;
		ThreadTransfer(father, message) ;
	}
}

static void AreaStatistics(CharPt name, Size space, Size used)
{
	Write("%30s: %4ldKb (%8ld bytes used)\n",
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
	AreaStatistics("Trail",
				ThreadTrailEnd(th) - ThreadTrailBegin(th),
				ThreadReg(th,TR) - ThreadTrailBegin(th)) ;
 
	dolist(th, ThreadSons(th), ThreadNext(th))
		ThreadStatistics(th) ;
}

void StatisticsShow()
{
	VersionShow() ;
	Write("Statistics:\n") ;
	AreaStatistics("Static area",
					StaticMemory(), StaticMemoryUsed()) ;
	AreaStatistics("Buffer",
					BufferTotalSize(), BufferTotalSize()) ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin might have moved */
	ThreadStatistics(root) ;
	Write("%30s: %8gs.\n", "Total runtime", CurrTime()) ;
}

static Bool ThreadCheckLoop(register ThreadPt th, VoidPt ref)
{
	if( th == ref ) return true ;
	dolist(th, ThreadSons(th), ThreadNext(th))
		if( ThreadCheckLoop(th, ref) )
			return true ;
	return false ;
}
Bool ThreadCheck(VoidPt ref)
{
	return ThreadCheckLoop(root, ref) ;
}


/* CXPROLOG C'BUILTINS */

static ThreadPt XTestThread(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) )
		t = IVarGet(XAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == threadSubTag )
		return UsingThread(cThreadPt(XPt(t))) ;
	TypeError("thread or ivar", t) ;
	return nil ;
}

static void PThreadNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		ThreadPt th = ThreadNew(Drf(X1), Drf(X2)) ;
		if( UnifyWithAtomic(t, TagExtra(th)) ) JumpNext()
		InternalError("PThreadNew") ;
	}
	if( IsAtom(t) ) {
		ThreadPt th = ThreadNew(Drf(X1), Drf(X2)) ;
		IVarSet(XAtom(t), TagExtra(th)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PThreadTransfer()
{
	ThreadTransfer(XTestThread(X0), tGoingAtom) ;
	InternalError("PThreadTransfer") ;
}

static void PThreadKill()
{
	ThreadPt th = XTestThread(X0) ;
	if( th == root )
		Error("The root thread cannot be killed") ;
	elif( th == active )
		ActiveThreadFinish(tKilledAtom) ;
	elif( IsAncestor(th, active) )
		Error("A thread cannot kill an ancestor thread") ;
	else ThreadDelete(th) ;
	JumpNext()
}

static void PActiveThread()
{
	if( UnifyWithAtomic(X0, TagExtra(active)) )
		JumpNext()
	else DoFail()
}

static void PActiveThreadCompleted()
{
	ActiveThreadFinish(tCompletedAtom) ;
	InternalError("PActiveThreadCompleted") ;
}

static void PActiveThreadFailed()
{
	ActiveThreadFinish(tFailedAtom) ;
	InternalError("PActiveThreadFailed") ;
}

static void PStatistics()
{
	StatisticsShow() ;
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
	dolist(th, ThreadSons(th), ThreadNext(th))
		ThreadsShow(th, n+1) ;
}
static void PThreads()
{
	VersionShow() ;
	Write("Threads:\n") ;
	ThreadsShow(root, 1) ;
	JumpNext()
}

void ThreadsInit()
{
	InstallCBuiltinPred("thread_new", 4, PThreadNew) ;
	InstallCBuiltinPred("thread_transfer", 2, PThreadTransfer) ;
	InstallCBuiltinPred("thread_kill", 1, PThreadKill) ;
	InstallCBuiltinPred("active_thread", 1, PActiveThread) ;
	InstallCBuiltinPred("active_thread_completed", 0, PActiveThreadCompleted) ;
	InstallCBuiltinPred("active_thread_failed", 0, PActiveThreadFailed) ;
	InstallCBuiltinPred("statistics", 0, PStatistics) ;
	InstallCBuiltinPred("threads", 0, PThreads) ;
}
