/*
 *   This file is part of the CxProlog system

 *   Interrupt.h
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Interrupt_
#define _Interrupt_


/* INTERRUPT */

void InterruptHandle(void) ;
void InterruptOff(void) ;
void InterruptOn(void) ;


/* ATTENTION */

#define Attention()				(attention)
#define AttentionActivate()		(attention = true)

extern Bool attention ;

void AttentionHandle(PredicatePt pr, FunctorPt f) ;

#endif
