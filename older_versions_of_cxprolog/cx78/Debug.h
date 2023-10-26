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

#define Interrupted()	(interrupted)
#define TraceActive()	(traceActive)

extern Bool traceActive, interrupted ;

void DebugCall(PredicatePt pr, FunctorPt f) ;
void DebugReset(void) ;
void DebugUpdate(void) ;
void DebugUpdate(void) ;
void HandleInterrupt(void) ;
void DebugInit(void) ;

#endif
