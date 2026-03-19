/*
 *   This file is part of the CxProlog system

 *   Thread.c
 *   by A.Miguel Dias - 1993/07/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

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

/*	Thread organization: tree

	Killing a thread also kills all of its descendents
	TransferToThread always succeeds with one of these results:
							going, completed, failed, killed, error
*/

static ThreadPt rootThread, currThread = nil ;
static Pt retVal ;

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

static ThreadPt FindThread(ThreadPt th, AtomPt threadName)
{
	ThreadPt t ;

	if( th->threadName == threadName )
		return th ;
	dolist(th, th->sons, th->brother)
		if( (t = FindThread(th, threadName)) != nil )
			return t ;
	return nil ;
}

static void DeleteThreadAux(ThreadPt th)
{
	ThreadPt t ;

	dolist(t, th->sons, t->brother)
		DeleteThreadAux(t) ;
	free(th) ;
}

static void DeleteThread(ThreadPt th)
{
	ThreadPt *t ;

	for( t = &th->father->sons ; *t != nil ; t = &(*t)->brother )
		if( *t == th ) {
			*t = th->brother ;
			break ;
		}
	DeleteThreadAux(th) ;
}

static void ResetThread(register ThreadPt th)
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

static void RestoreThread(register ThreadPt th)
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
	currThread = th ;
}

static void SaveCurrThread()
{
	register ThreadPt th = currThread ;
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
	if( pr == nil || PredIsUndefined(pr) )
		FatalError("Missing startup predicate '%s'", PredNameArity(pr)) ;
	pr = FindPredicateByName("@@_called_from_c", 1) ;
	if( pr == nil || PredIsUndefined(pr) )
		FatalError("Missing predicate '%s'", PredNameArity(pr)) ;
	X0 = goal ;
	P = PredCode(pr) ;
}

ThreadPt CreateThread(AtomPt threadName, long size /* in bytes */,
							Pt startGoal, Pt restartGoal)
{
	long thSize, trailSize ;
	register ThreadPt th ;
	Hdl pt ;

	if( rootThread != nil && FindThread(rootThread, threadName) != nil )
		Error("Thread \"%s\" was already created", AtomName(threadName)) ;

	if( size < 10 Kb ) size = 10 Kb ;
	thSize = RoundUp(sizeof(Thread), sizeof(Pt)) ;
	size = RoundUp(size, sizeof(Pt)) ;
	trailSize = RoundUp(size / trailFactor, sizeof(Pt)) ;
	if( (pt = AllocAligned(thSize + size + trailSize)) == nil )
		Error("Not enough computer memory to create this thread") ;

	th = cThreadPt(pt) ;
	th->threadName = threadName ;
	th->startGoal = startGoal ;
	th->restartGoal = restartGoal ;
	th->P = nil ;

	th->stacksBegin = pt + thSize / sizeof(Word) ;
	th->stacksEnd = th->stacksBegin + size / sizeof(Word) ;
	th->trailBegin = th->stacksEnd ;
	th->trailEnd = th->trailBegin + trailSize / sizeof(Word) ;

	th->father = currThread ;
	th->sons = nil ;

	ResetThread(th) ;

	if( currThread == nil ) {
		th->brother = nil ;
		RestoreThread(th) ;
		rootThread = currThread ;
	}
	else {
		th->brother = currThread->sons ;
		currThread->sons = th ;
	}

	return th ;
}

void TransferToThread(ThreadPt th)
{
	PredicatePt pr ;
	Pt res = nil ;

	if( currThread == nil )
		res = retVal ;
	else SaveCurrThread() ;
	RestoreThread(th) ;
	if( P == nil )
		CalledFromC(th->startGoal) ;
	else {
		Pt t = Drf(X1) ;
		if( not IsVar(t) )
			InternalError("TransferToThread") ;
		if( res == nil ) res = MakeAtom("going") ;
		Assign(t, res) ;
	}	
	RunMachine() ;
}

void RestartCurrThread()
{
	PredicatePt pr ;
	register ThreadPt th = currThread ;

	/* if( no restart goal ) FinishCurrThread("error") ; */
	ResetThread(th) ;
	RestoreThread(th) ;
	CalledFromC(th->restartGoal) ;
	RunMachine() ;
}

ThreadPt LookupThread(AtomPt atom)
{
	ThreadPt th ;
	
	if( (th = FindThread(rootThread, atom)) != nil )
		return th ;
	Error("Thread \"%s\" not found", AtomName(atom)) ;
}

void FinishCurrThread(CharPt res)
{
	if( currThread == rootThread )
		Mesg("rootThread cannot be killed. (Ignored)") ;
	else {
		ThreadPt father = currThread->father ;
		DeleteThread(currThread) ;
		currThread = nil ;
		retVal = MakeAtom(res) ;
		TransferToThread(father) ;
	}
}

void KillThread(ThreadPt th)
{
	if( th == rootThread )
		Mesg("rootThread cannot be killed. (Ignored)") ;
	elif( th == currThread )
		FinishCurrThread("killed") ;
	elif( IsAncestor(th, currThread) )
		Mesg("A thread cannot kill an ancestor thread. (Ignored)") ;
	else DeleteThread(th) ;
}
	
AtomPt GetCurrThreadName()
{
	return currThread->threadName ;
}

static void AreaStatistics(CharPt name, long space, long used)
{
	Write("%30s: %4ldKb (%8ld bytes used)\n",
					name, space Wd/(1 Kb), used Wd) ;
}

static void ThreadStatistics(ThreadPt th)
{
	if( th != rootThread )
		Write("--- thread <%s> -------\n", AtomName(th->threadName)) ;
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

void Statistics()
{
	AreaStatistics("Heap", TotalMemory(), MemoryUsed()) ;
	SaveCurrThread() ;
	ThreadStatistics(rootThread) ;
	Write("%30s: %8gs.\n", "Total runtime", CurrTime()) ;
}

