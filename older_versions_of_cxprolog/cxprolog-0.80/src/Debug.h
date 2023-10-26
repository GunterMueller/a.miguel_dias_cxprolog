/*
 *   This file is part of the CxProlog system

 *   Debug.h
 *   by A.Miguel Dias - 2000/05/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Debug_
#define _Debug_

#define IsDebugCP(b)	( *(b)->P == DebugRetry )

Bool DebugCall(PredicatePt pr) ;
void DebugCut(void) ;
void DebugExitCode(void) ;
void DebugRedoCode(void) ;
void DebugRetryCode(void) ;
void DebugRestart(void)	;
void DebugInterrupt(int newValue) ;
Bool DebugUpdateFlags(int newValue) ;
void Debugging(void) ;
void DebugInit(void) ;

#endif
