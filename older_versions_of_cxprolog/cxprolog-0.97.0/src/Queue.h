/*
 *   This file is part of the CxProlog system

 *   Queue.h
 *   by A.Miguel Dias - 2000/08/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Queue_
#define _Queue_

/* QUEUE */
typedef struct Queue {
	ExtraDef(Queue) ;
	Hdl begin, end, first, last  ;
} Queue, *QueuePt ;

#define cQueuePt(x)				((QueuePt)(x))

#define QueueBegin(q)			((q)->begin)
#define QueueEnd(q)				((q)->end)
#define QueueFirst(q)			((q)->first)
#define QueueLast(q)			((q)->last)

/* MAIN OPERATIONS */
Size QueueSize(QueuePt q) ;
QueuePt QueueNew(void) ;
void QueueClear(QueuePt q) ;
void QueueFilter(QueuePt q, BFunVV filter, VoidPt x) ;
Bool QueuePeek(QueuePt q, Hdl t) ;
Bool QueueGet(QueuePt q) ;
void QueuePut(QueuePt q, Pt t) ;
Pt ZGetQueueFrontSectionAsList(QueuePt q, Pt mark) ;

/* TEST, EXTRACT & INIT */
Bool IsQueue(Pt t) ;
QueuePt XTestQueue(Pt t) ;
void QueuesInit(void) ;

#endif
