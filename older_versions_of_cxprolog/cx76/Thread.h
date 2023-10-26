/*
 *   This file is part of the CxProlog system

 *   Thread.h
 *   by A.Miguel Dias - 1993/07/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Thread_
#define _Thread_

typedef struct Thread
{
	Word tagHolder ;
	Pt startGoal, restartGoal ;
	Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
	Hdl P, CP, H, TR ;
	EnvironmentPt E ;
	ChoicePointPt B ;
	Pt C, CC, CH, X[maxX] ;
	struct Thread *father, *sons, *brother ;
} Thread, *ThreadPt ;

#define	cThreadPt(t)				((ThreadPt)(t))

ThreadPt ThreadNew(long size, Pt startGoal, Pt restartGoal) ;
void ThreadTransfer(ThreadPt th, Pt message) ;
void ActiveThreadRestart(void) ;
void ShowStatistics(void) ;
Bool ThreadCheck(VoidPt ref) ;
void InitThreads(void) ;

#endif
