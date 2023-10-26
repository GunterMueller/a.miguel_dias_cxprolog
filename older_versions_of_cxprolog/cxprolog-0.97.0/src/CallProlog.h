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

#define CallLevel()					(callLevel)

extern int callLevel ;
extern Bool restrictedMode ;

Bool InMainThread(Bool warn) ;

void PrologEvent(int ev) ;
void EventContinue(void) ;
void EventForceFail(void) ;
void EventRestart(void) ;
void EventShrink(int toLevel) ;
void EventExit(void) ;
void EventHalt(void) ;

int RunCxProlog(int argc, char **argv) ;
Bool CallProlog(Pt goal) ;
Bool CallPrologStr(CharPt goal) ;
int CallFunThruProlog(Fun fun) ;
void CallPrologInit(void) ;

#endif
