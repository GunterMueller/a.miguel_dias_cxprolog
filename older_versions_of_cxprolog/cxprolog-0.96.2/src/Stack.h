/*
 *   This file is part of the CxProlog system

 *   Stack.h
 *   by A.Miguel Dias - 2000/11/30
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stack_
#define _Stack_

/* STACK */
typedef struct Stack {
	ExtraDef(Stack) ;
	Hdl begin, end, first, last  ;
} Stack, *StackPt ;

#define cStackPt(x)				((StackPt)(x))

#define StackBegin(s)			((s)->begin)
#define StackEnd(s)				((s)->end)
#define StackFirst(s)			((s)->first)
#define StackLast(s)			((s)->last)

/* MAIN OPERATIONS */
Size StackSize(StackPt s) ;
StackPt StackNew(void) ;
void StackClear(StackPt s) ;
void StackFilter(StackPt s, BFunVV filter, VoidPt x) ;
Bool StackTop(StackPt s, Hdl t) ;
Bool StackFromTop(StackPt s, int idx, Hdl t) ;
void StackPush(StackPt s, Pt t) ;
Bool StackPop(StackPt s) ;

/* TEST, EXTRACT & INIT */
Bool IsStack(Pt t) ;
StackPt XTestStack(Pt t) ;
void StacksInit(void) ;

#endif
