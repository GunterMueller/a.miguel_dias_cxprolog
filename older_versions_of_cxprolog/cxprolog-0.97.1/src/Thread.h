/*
 *   This file is part of the CxProlog system

 *   Thread.h
 *   by A.Miguel Dias - 1993/07/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Thread_
#define _Thread_

#define endOfChainMark			tNilAtom
#define EndOfChain(pt)			(*cHdl(pt) == tNilAtom)

void ThreadRootCreate(Pt startGoal, Pt restartGoal) ;
void ThreadRootStart(void) ;
void ActiveThreadReplace(Pt startGoal, Pt restartGoal) ;

void ActiveThreadStart(void) ;
void ActiveThreadRestart(void) ;
void StatisticsShow(void) ;
void ThreadsInit(void) ;

#endif
