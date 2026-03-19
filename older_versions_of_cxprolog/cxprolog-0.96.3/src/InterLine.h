/*
 *   This file is part of the CxProlog system

 *   InterLine.h
 *   by A.Miguel Dias - 2007/09/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _InterLine_
#define _InterLine_

#define InteractiveSession()		(interactiveSession)
#define UseInteractiveStreams()		(useInteractiveStreams)

extern Bool interactiveSession, useInteractiveStreams ;

void InterLineBeginTopPrompt(CharPt main, CharPt cont) ;
void InterLineEndTopPrompt(void) ;
void InterLineBeginLinePrompt(CharPt temp) ;
void InterLineEndLinePrompt(void) ;

WChar InterLineGet(void) ;
WChar InterLinePeek(void) ;
Bool InterLineGetSingleChar(WChar *c) ;
void InterLineChangedUserStreams(void) ;

void InterLineRestart(void) ;
void InterLineInit2(void) ;
void InterLineInit(void) ;

#endif
