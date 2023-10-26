/*
 *   This file is part of the CxProlog system

 *   Thread.h
 *   by A.Miguel Dias - 1993/07/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Thread_
#define _Thread_
							
#define GrowGlobal(w)		GrowArea(H, stacksEnd, w, "Stacks")
#define TestTrailOverflow()	if( TR >= trailEnd) NoSpace("Trail") ;
#define TestOverflow(lTop)	if( cHdl(lTop) - 512 < H ) NoSpace("Stack")

typedef struct Thread
{
	AtomPt threadName ;
	Pt startGoal, restartGoal ;
	Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
	Hdl P, CP, H, TR ;
	EnvironmentPt E ;
	ChoicePointPt B ;
	Pt C, CC, X[maxX] ;
	struct Thread *father, *sons, *brother ;
} Thread, *ThreadPt ;

#define	cThreadPt(t)				((ThreadPt)(t))

ThreadPt CreateThread(AtomPt threadName, long size, Pt startGoal, Pt restartGoal) ;
ThreadPt LookupThread(AtomPt threadName) ;
void TransferToThread(ThreadPt th) ;
void RestartCurrThread(void) ;
void KillThread(ThreadPt th) ;
void FinishCurrThread(CharPt res) ;
AtomPt GetCurrThreadName(void) ;
void Statistics(void) ;

#endif
