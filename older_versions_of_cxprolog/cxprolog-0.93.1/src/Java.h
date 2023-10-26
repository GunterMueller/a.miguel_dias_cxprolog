 /*
 *   This file is part of the CxProlog system

 *   Java.h
 *   by A.Miguel Dias - 2004/07/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Java_
#define _Java_

void JavaSetEventNotifier(Fun f) ;
Pt JavaGetEvent(void) ;
int JavaHowManyEvents(void) ;
void JavaDiscardAllEvents(void) ;
void JavaInit(void) ;

#endif
