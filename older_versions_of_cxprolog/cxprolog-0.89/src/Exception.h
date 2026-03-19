/*
 *   This file is part of the CxProlog system

 *   Exception.h
 *   by A.Miguel Dias - 2003/08/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Exception_
#define _Exception_

/* MAIN OPERATIONS */
void ThrowPrologException(Pt t) ;
void ThrowPrologExceptionMesgV(CharPt kind, CharPt fmt, VoidPt v) ;
void ThrowPrologExceptionMesg(CharPt kind, CharPt fmt, ...) ;

/* INIT */
void ExceptionsInit(void) ;

#endif
