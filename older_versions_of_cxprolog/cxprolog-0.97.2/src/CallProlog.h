/*
 *   This file is part of the CxProlog system

 *   CallProlog.h
 *   by A.Miguel Dias -  2003/06/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _CallProlog_
#define _CallProlog_


typedef enum {
	peNone = 0,

	peSucc, peFailure, peException,
	peExit, peHalt, peRestart,
	peFatalError, peOtherError,

	peContinue, peForceFail, peSetSuccess,
	peReturn, peShrink
} PrologEvent ;

#define CallLevel()					(callLevel)
#define HasATopLevel()				(hasATopLevel)

extern int callLevel ;
extern Bool hasATopLevel ;

Bool InMainThread(Bool warn) ;

void SendPrologEvent(PrologEvent ev) ;
void EventContinue(void) ;
void EventForceFail(void) ;
void EventRestart(void) ;
void EventShrink(int toLevel) ;
void EventExit(void) ;
void EventHalt(void) ;
void EventFatalError(void) ;

PrologEvent CallProlog(Pt goal, Pt *res) ;
void CallPrologV(Pt goal) ;
PrologEvent CallFunThruProlog(Fun fun) ;
PrologEvent CallFunThruProlog(Fun fun) ;
PrologEvent CallPrologStr(CharPt goal) ;
PrologEvent StartProlog(int argc, char **argv) ;
void StopProlog(void) ;
int RunInteractiveProlog(int argc, char **argv) ;
void RefreshProlog(void) ;
void AtTopLevel(void) ;
void CallPrologInit(void) ;

#endif
