/*
 *   This file is part of the CxProlog system

 *   CallProlog.h
 *   by A.Miguel Dias -  2003/06/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _CallProlog_
#define _CallProlog_

#define CallLevel()		(callLevel)

void EventContinue(void) ;
void EventForceFail(void) ;
void EventRestart(void) ;
void EventSetSuccess(void) ;
void EventReturn(void) ;
void EventShrink(int toLevel) ;
void EventExit(void) ;
void EventHalt(void) ;

Bool CallProlog(Pt goal) ;
void CallPrologInit(void) ;

extern int callLevel ;

#endif
