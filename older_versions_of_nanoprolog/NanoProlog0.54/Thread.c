/*
 *   This file is part of the NanoProlog system

 *   Thread.c
 *   by A.Miguel Dias - 93/7/15
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931117: release of version 0.5

*/

/*	Thread organization: tree

	Killing a thread also kills all off its descendents
	TransferToThread succeeds always with one off those results:
							going, completed, failed, killed, error
	
	Seems to be working.
*/
  
#include "NanoProlog.h"

static ThreadPt rootThread ;
static ThreadPt currThread = nil ;
static Pt retVal ;

static Bool IsAncestor(ThreadPt a, ThreadPt b) ;
static Bool IsAncestor(ThreadPt a, ThreadPt b)
{
	ThreadPt th ;

 	if( a == b )
 		return( true ) ;
 	foreach(th, a->sons, th->brother)
 		if( IsAncestor(th, b) )
 			return( true ) ;
 	return( false ) ;
}

static ThreadPt FindThread(ThreadPt th, AtomPt atom) ;
static ThreadPt FindThread(ThreadPt th, AtomPt atom)
{
	ThreadPt t ;

 	if( th->name == atom )
 		return( th ) ;
 	foreach(th, th->sons, th->brother)
 		if( (t = FindThread(th, atom)) != nil )
  			return( t ) ;
 	return( nil ) ;
}

static void DeleteThreadAux(ThreadPt th) ;
static void DeleteThreadAux(ThreadPt th)
{
	ThreadPt t ;

	foreach(t, th->sons, t->brother)
 		DeleteThreadAux(t) ;
	free(th) ;
}

static void DeleteThread(ThreadPt th) ;
static void DeleteThread(ThreadPt th)
{
	ThreadPt *t ;

 	for( t = &th->father->sons ; *t != nil ; t = &(*t)->brother )
 		if( *t == th )
 		{
 			*t = th->brother ;
 			break ;
 		}
 	DeleteThreadAux(th) ;
}

static void ResetThread(ThreadPt th) ;
static void ResetThread(register ThreadPt th)
{
	th->H = th->stacksBegin ;
	th->TR =th->trailBegin ;
	th->B = cChoicePointPt(th->stacksEnd) ;
	th->E = cEnvironmentPt(th->stacksEnd) + 1 ;
	th->P = nil ;
	th->CP = nil ;
	th->X[0] = nil ;
}

static void ActivateThread(ThreadPt th) ;
static void ActivateThread(register ThreadPt th)
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
	for( i = 0 ; (Xc(i) = th->X[i]) != nil ; i++ ) ;
	currThread = th ;
}

static void GetThread()
{
	register ThreadPt th = currThread ;
	register int i ;

	th->H = H ;
	th->TR = TR ;
	th->B = B ;
	th->E = E ;
	th->P = P ;
	th->CP = CP ;
	for( i = 0 ; (th->X[i] = Xc(i)) != nil ; i++ ) ;
}

ThreadPt CreateThread(AtomPt name, long size /* in bytes */,
							AtomPt module, AtomPt goal, AtomPt rgoal /* restart goal*/)
{
	long thSize, trailSize ;
	register ThreadPt th ;
	Hdl pt ;

	if( rootThread != nil && FindThread(rootThread, name) != nil )
 		Error("Thread \"%s\" was already created", AtomName(name)) ;

	if( size < 12 Kb ) size = 12 Kb ;
	thSize = RoundUp(sizeof(Thread), sizeof(Pt)) ;
	size = RoundUp(size, sizeof(Pt)) ;
	trailSize = RoundUp(size / 12, sizeof(Pt)) ;
	pt = TAlloc(thSize + size + trailSize) ;
	if( pt == nil ) return( nil ) ;

	th = cThreadPt(pt) ;
	th->name = name ;
	th->module = module ;
	th->goal = goal ;
	th->rgoal = rgoal ;
	th->P = nil ;

	th->stacksBegin = pt + thSize / sizeof(Word) ;
	th->stacksEnd = th->stacksBegin + size / sizeof(Word) ;
	th->trailBegin = th->stacksEnd ;
	th->trailEnd = th->trailBegin + trailSize / sizeof(Word) ;

	th->father = currThread ;
	th->sons = nil ;

	ResetThread(th) ;

	if( currThread == nil )
	{
		th->brother = nil ;
		ActivateThread(th) ;
		rootThread = currThread ;
	}
	else
	{
		th->brother = currThread->sons ;
		currThread->sons = th ;
	}

	return( th ) ;
}

void TransferToThread(ThreadPt th)
{
	Predicate *pr ;
	Pt res = nil ;

	if( currThread == nil )
		res = retVal ;
	else GetThread() ;
	ActivateThread(th) ;
	if( P == nil )
	{
		pr = LookupPredicateInModule(LookupFunctor(th->goal, 0),
												LookupModule(th->module)) ;
		if( PredNClauses(pr) == 0 )
			FatalError("Missing thread-startup predicate %s", PredNameArity(pr)) ;
		pr = LookupPredicate2("$called_from_c", 2) ;
		if( PredNClauses(pr) == 0 )
			FatalError("Missing predicate %s", PredNameArity(pr)) ;
		Xc(0) = TagAtom(th->module) ;
		Xc(1) = TagAtom(th->goal) ;
		P = PredCode(pr) ;
	}
	else
	{
		Pt t = Drf(Xc(1)) ;

		if( not IsVar(t) )
			InternalError("TransferToThread") ;
		if( res == nil ) res = MakeAtom("going") ;
		Assign(t, res) ;
	}	
	RunInterpreter() ;
}

void RestartCurrThread()
{
	Predicate *pr ;
	register ThreadPt th = currThread ;

	if( th->rgoal == nilAtom ) /* no restart goal */
	{
		FinishCurrThread("error") ;
		return ;
	}
	ResetThread(th) ;
	ActivateThread(th) ;
	{
		pr = LookupPredicateInModule(LookupFunctor(th->rgoal, 0),
											LookupModule(th->module)) ;
		if( PredNClauses(pr) == 0 )
			FatalError("Missing thread-startup predicate %s", PredNameArity(pr)) ;
		pr = LookupPredicate2("$called_from_c", 2) ;
		if( PredNClauses(pr) == 0 )
			FatalError("Missing predicate %s", PredNameArity(pr)) ;
		Xc(0) = TagAtom(th->module) ;
		Xc(1) = TagAtom(th->rgoal) ;
		P = PredCode(pr) ;
	}	
	RunInterpreter() ;
}

ThreadPt LookupThread(AtomPt atom)
{
	ThreadPt th ;
	
	if( (th = FindThread(rootThread, atom)) != nil )
 			return( th ) ;
 	Error("Thread \"%s\" not found", AtomName(atom)) ;
}

void FinishCurrThread(char *res)
{
	ThreadPt fat ;

	if( currThread == rootThread )
	{
		Mesg("rootThread cannot be killed. (Ignored)") ;
		return ;
	}
	fat = currThread->father ;
	DeleteThread(currThread) ;
	currThread = nil ;
	retVal = MakeAtom(res) ;
	TransferToThread(fat) ;
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
	return( currThread->name ) ;
}

static void ThreadStatistics(ThreadPt th) ;
static void ThreadStatistics(ThreadPt th)
{
	AreaStatistics(AtomName(th->name), "Stacks",
				th->stacksEnd - th->stacksBegin,
				(th->H - th->stacksBegin)
					+ (th->stacksEnd - Min(cHdl(th->B), cHdl(th->E)))) ;
	AreaStatistics("", "Trail",
				th->trailEnd - th->trailBegin,
				th->TR - th->trailBegin) ;
 
 	foreach(th, th->sons, th->brother)
 		ThreadStatistics(th) ;
}

void Statistics()
{
	HeapStatistics() ;
	ThreadStatistics(rootThread) ;
	printf("%20s: %8gs.\n", "Runtime", NGetTime()) ;
}

