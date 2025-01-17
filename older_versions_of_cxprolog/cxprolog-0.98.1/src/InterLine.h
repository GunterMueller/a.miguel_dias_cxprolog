/*
 *   This file is part of the CxProlog system

 *   InterLine.h
 *   by A.Miguel Dias - 2007/09/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _InterLine_
#define _InterLine_

#define InteractiveSession()		(interactiveSession)

extern Bool interactiveSession ;

void InterLineBeginTop(Str mainPrompt, Str contPrompt) ;
void InterLineEndTop(void) ;

void InterLineChangedUserStreams(void) ;
void InterLineFinish(void) ;
WChar InterLineGet(void) ;
WChar InterLinePeek(void) ;
Bool InterLineGetSingleChar(WChar *c) ;
WChar InterLineGetCommand(Str prompt, int *arg) ;

void InterLineRestart(void) ;
void InterLineInit2(void) ;
void InterLineInit(void) ;

Str InterLineVersion(void) ;

#endif
