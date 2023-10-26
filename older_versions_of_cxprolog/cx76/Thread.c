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

/* @@@ Memory management yet to be done */

#define trailFactor			20

static ThreadPt root, active = nil ;
static ThreadPt freeThreadList = nil ;

static Bool IsAncestor(ThreadPt a, ThreadPt b)
{
	ThreadPt th ;

	if( a == b )
		return true ;
	dolist(th, a->sons, th->brother)
		if( IsAncestor(th, b) )
			return true ;
	return false ;
}

static void ThreadClear(ThreadPt th)
{
	ThreadPt t ;
	th->stacksBegin = nil ;
	dolist(t, th->sons, t->brother)
		ThreadClear(t) ;
}

static void ThreadDelete(ThreadPt th)
{
	if( th->father->sons == th )	/* first is removed */
		th->father->sons = th->brother ;
	else {
		register ThreadPt x ;
		for( x = th->father->sons ; x->brother != th ; x = x->brother ) ;
		x->brother = th->brother ;
	}
	ThreadClear(th) ;
}

static ThreadPt UsingThread(ThreadPt th)
{
	if( th->stacksBegin == nil )
		Error("Invalid operation over deleted thread %s", XExtraAsStr(TagExtra(th))) ;
	return th ;
}

static void ThreadReset(register ThreadPt th)
{
	th->H = th->stacksBegin ;
	th->TR =th->trailBegin ;
	th->B = cChoicePointPt(th->stacksEnd) ;
	th->E = cEnvironmentPt(th->stacksEnd) + 1 ;
	th->P = nil ;
	th->CP = nil ;
	th->C = tNilAtom ;
	th->CC = tNilAtom ;
	th->CH = tNilAtom ;
	th->X[0] = nil ;
}

static void ThreadRestore(register ThreadPt th)
{
	register int i ;
	stacksBegin = th->stacksBegin ;
	stacksEnd = th->stacksEnd ;
	trailBegin = th->trailBegin ;
	trailEnd = th->trailEnd ;
	H = th->H ;
	TR = th->TR ;
	B = th->B ;
	HB = Bf(H) ;
	E = th->E ;
	P = th->P ;
	CP = th->CP ;
	C = th->C ;
	CC = th->CC ;
	CH = th->CH ;
	for( i = 0 ; (Xc(i) = th->X[i]) != nil ; i++ ) ;
	active = th ;
}

static void ThreadSave(register ThreadPt th)
{
	register int i ;
	th->H = H ;
	th->TR = TR ;
	th->B = B ;
	th->E = E ;
	th->P = P ;
	th->CP = CP ;
	th->C = C ;
	th->CC = CC ;
	th->CH = CH ;
	for( i = 0 ; (th->X[i] = Xc(i)) != nil ; i++ ) ;
}

static void CalledFromC(Pt goal)
{
	PredicatePt pr = FindPredicate(XTestFunctor(goal)) ;
/*	if( pr == nil || PredIsUndefined(pr) )
		FatalError("Missing startup predicate '%s'", PredNameArity(pr)) ;*/
	pr = FindPredicateByName("$$_called_from_c", 1) ;
	if( pr == nil || PredIsUndefined(pr) )
		FatalError("Missing predicate '%s'", PredNameArity(pr)) ;
	X0 = goal ;
	P = PredCode(pr) ;
}

ThreadPt ThreadNew(long bytes, Pt startGoal, Pt restartGoal)
{
	long thSize, stkSize, trailSize ;
	register ThreadPt th ;
	Hdl pt ;

	if( bytes < 10 K ) bytes = 10 K ;
	thSize = WordsOf(Thread) ;
	stkSize = Words(bytes) ;
	trailSize = stkSize/trailFactor ;
	pt = PrimitiveAllocate(thSize + stkSize + trailSize) ;

	th = cThreadPt(pt) ;
	th->tagHolder = threadSubTag ;
	th->startGoal = AllocateTermForAssign(startGoal) ;
	th->restartGoal = AllocateTermForAssign(restartGoal) ;
	th->P = nil ;

	th->stacksBegin = pt + thSize ;
	th->stacksEnd = th->stacksBegin + stkSize ;
	th->trailBegin = th->stacksEnd ;
	th->trailEnd = th->trailBegin + trailSize ;

	th->father = active ;
	th->sons = nil ;

	ThreadReset(th) ;

	if( active == nil ) {
		th->brother = nil ;
		ThreadRestore(th) ;
		root = active ;
	}
	else {
		th->brother = active->sons ;
		active->sons = th ;
	}

	return th ;
}

void ThreadTransfer(ThreadPt th, Pt message)
{
	if( active != nil ) ThreadSave(active) ;
	ThreadRestore(th) ;
	if( P == nil )
		CalledFromC(PushTerm(th->startGoal)) ;
	else		/* here is a transfer_to_thread syncronize point */
		if( not UnifyWithAtomic(X1, message) )
			EventForceFail() ;
	EventContinue() ;
}

void ActiveThreadRestart()
{
	ThreadReset(active) ;
	ThreadRestore(active) ;
	CalledFromC(PushTerm(active->restartGoal)) ;
	EventContinue() ;
}

static void ActiveThreadFinish(Pt message)
{
	if( active == root )
		FatalError("The root thread has finished") ;
	else {
		ThreadPt father = active->father ;
		ThreadDelete(active) ;
		active = nil ;
		ThreadTransfer(father, message) ;
	}
}

static void AreaStatistics(CharPt name, long space, long used)
{
	Write("%30s: %4ldKb (%8ld bytes used)\n",
					name, WordsAsKBytes(space), WordsAsBytes(used)) ;
}

static void ThreadStatistics(ThreadPt th)
{
	if( th != root )
		Write("--- thread <%s> -------\n", XExtraAsStr(TagExtra(th))) ;
	AreaStatistics("Global stack + Local stack",
					th->stacksEnd - th->stacksBegin,
					(th->H - th->stacksBegin)
						+ (th->stacksEnd - Min(cHdl(th->B), cHdl(th->E)))) ;
	AreaStatistics("Trail",
				th->trailEnd - th->trailBegin,
				th->TR - th->trailBegin) ;
 
	dolist(th, th->sons, th->brother)
		ThreadStatistics(th) ;
}

void ShowStatistics()
{
	ShowVersion() ;
	Write("Statistics:\n") ;
	AreaStatistics("Static area (Heap)", TotalMemory(), MemoryUsed()) ;
	AreaStatistics("Buffer", BufferSize(), 0) ;
	ThreadSave(active) ;
	ThreadStatistics(root) ;
	Write("%30s: %8gs.\n", "Total runtime", CurrTime()) ;
}

static Bool ThreadCheckLoop(register ThreadPt th, VoidPt ref)
{
	if( th == ref ) return true ;
	dolist(th, th->sons, th->brother)
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
	if( IsAtomOrText(t) )
		t = IVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == threadSubTag )
		return UsingThread(cThreadPt(XPt(t))) ;
	TypeError("thread or ivar", t) ;
	return nil ;
}

static void PThreadNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		ThreadPt th = ThreadNew(XTestInt(X1) K, X2, X3) ;
		if( UnifyWithAtomic(t, TagExtra(th)) ) JumpNext()
		InternalError("PThreadNew") ;
	}
	if( IsAtomOrText(t) ) {
		ThreadPt th = ThreadNew(XTestInt(X1) K, X2, X3) ;
		IVarSet(XAtomOrTextAsAtom(t), TagExtra(th)) ;
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
	ShowStatistics() ;
	JumpNext()
}

static void ShowThreads(register ThreadPt th, int n)
{
	int i ;
	dotimes(i, n) Write("  ") ;
	Write("%16s\n", TermAsStr(TagExtra(th))) ;
	dolist(th, th->sons, th->brother)
		ShowThreads(th, n+1) ;
}
static void PThreads()
{
	ShowVersion() ;
	Write("Threads:\n") ;
	ShowThreads(root, 1) ;
	JumpNext()
}

void InitThreads()
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
