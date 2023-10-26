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

void ThreadCreateRoot(Pt startGoal, Pt restartGoal) ;
void ThreadActivateRoot(void) ;
void ActiveThreadRestart(void) ;
void StatisticsShow(void) ;
Bool ThreadCheck(VoidPt ref) ;
void ThreadsInit(void) ;

#endif
