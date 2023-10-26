/*
 *   This file is part of the CxProlog system

 *   Queue.c
 *   by A.Miguel Dias - 2000/08/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"

typedef struct Queue
{
	Word tagHolder ;
	Hdl begin, end, first, last  ;
	struct Queue *next ;
} Queue, *QueuePt ;

#define QueueTagHolder(q)		((q)->tagHolder)
#define QueueBegin(q)			((q)->begin)
#define QueueEnd(q)				((q)->end)
#define QueueFirst(q)			((q)->first)
#define QueueLast(q)			((q)->last)
#define QueueNext(q)			((q)->next)

#define cQueuePt(x)				((QueuePt)(x))


static QueuePt queueList = nil ;
static QueuePt errorQueue ;

static long QueueSize(QueuePt q)
{
	return QueueLast(q) - QueueFirst(q) ;
}

static long QueueCapacity(QueuePt q)
{
	return QueueEnd(q) - QueueBegin(q) ;
}

static void InitQueue(QueuePt q, long capacity)
{
	QueueBegin(q) = TemporaryAllocate(capacity) ;
	QueueEnd(q) = QueueBegin(q) + capacity ;
	QueueFirst(q) = QueueBegin(q) ;
	QueueLast(q) = QueueBegin(q) ;
}

static void GrowQueue(QueuePt q)
{
	Hdl b = QueueBegin(q) ;
	Hdl f = QueueFirst(q) ;
	Hdl l = QueueLast(q) ;
	Hdl h ;
	
	InitQueue(q, 2 * QueueCapacity(q)) ;
	for( h = QueueBegin(q) ; f < l ; *h++ = *f++ ) ;
	QueueLast(q) = h ;
	Release(b) ;
}

static void ParkQueue(QueuePt q)
{
	Hdl f, b ;

	for( b = QueueBegin(q), f = QueueFirst(q) ; f < QueueLast(q) ; *b++ = *f++ ) ;
	QueueFirst(q) = QueueBegin(q) ;
	QueueLast(q) = b ;
}

static QueuePt QueueNew(void)
{
	QueuePt q = TemporaryAllocate(WordsOf(Queue)) ;
	QueueTagHolder(q) = 0 ;
	QueueNext(q) = queueList ;
	queueList = q ;
	InitQueue(q, 4) ;
	return q ;
}

static void QueueClear(QueuePt q)
{
	register Hdl h ;
	
	for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ )
		ReleaseTerm(*h) ;
	QueueLast(q) = QueueFirst(q) ;
}

static void QueueFree(QueuePt q)
{
	if( queueList == q )	/* first is removed */
		queueList = QueueNext(q) ;
	else {
		register QueuePt x ;
		for( x = queueList ; QueueNext(x) != q ; x = QueueNext(x) ) ;
		QueueNext(x) = QueueNext(q) ;
	}
	QueueClear(q) ;
	Release(QueueBegin(q)) ;
	Release(q) ;
}

static void QueuePut(QueuePt q, Pt t)
{
	if( QueueLast(q) == QueueEnd(q) )
		if( QueueFirst(q) != QueueBegin(q) )
			ParkQueue(q) ;
		else
			GrowQueue(q) ;
	*QueueLast(q)++ = AllocateTermForAssign(t) ;
}

static Bool QueueGet(QueuePt q, Hdl t)
{
	if( QueueFirst(q) < QueueLast(q) ) {
		*t = PushTerm(*QueueFirst(q)) ;
		ReleaseTerm(*QueueFirst(q)++) ;
		return true ;
	}
	else return false ;
}

static Bool QueuePeek(QueuePt q, Hdl t)
{
	if( QueueFirst(q) < QueueLast(q) ) {
		*t = PushTerm(*QueueFirst(q)) ;
		return true ;
	}
	else return false ;
}

static void QueueWrite(QueuePt q)
{
	Hdl h ;
	
	Write("%s", XExtraAsStr(TagExtra(q, queueSubTag))) ;
	Write("     (current capacity %ld)\n", QueueCapacity(q)) ;
	for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ ) {
		Write("\t") ;
		WriteTerm(*h) ;
		Nl() ;
	}
}

void QueueError(CharPt s)
{
	QueuePut(errorQueue, MakeAtom(s)) ;
}

Bool QueueCheck(Pt t)
{
	register QueuePt q ;

	dolist(q, queueList, QueueNext(q))
		if( cPt(q) == t )
			return true ;
	return false ;
}


/* CXPROLOG C'BUILTINS */

static QueuePt XTestQueue(register Pt t)
{
	VarValue(t) ;
	if( IsAtomOrText(t) )
		t = ImperativeVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == queueSubTag )
		return cQueuePt(XPt(t)) ;
	TypeError("queue or ivar", t) ;
	return nil ;
}

static void PQueueCheck()
{
	Pt t = Drf(X0) ;
	if( IsAtomOrText(t) )
		t = ImperativeVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == queueSubTag ) JumpNext()
	DoFail()
}

static void PQueueNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		QueuePt q = QueueNew() ;
		if( UnifyWithAtomic(t, TagExtra(q, queueSubTag)) ) JumpNext()
		InternalError("PQueueNew") ;
	}
	if( IsAtomOrText(t) ) {
		QueuePt q = QueueNew() ;
		ImperativeVarSet(XAtomOrTextAsAtom(t), TagExtra(q, queueSubTag)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PQueueClear()
{
	QueueClear(XTestQueue(X0)) ;
	JumpNext()
}

static void PQueueFree()
{
	QueueFree(XTestQueue(X0)) ;
	JumpNext()
}

static void PQueuePut()
{
	QueuePut(XTestQueue(X0),X1) ;
	JumpNext()
}

static void PQueueGet()
{
	Pt t ;
	if( QueueGet(XTestQueue(X0),&t) && Unify(X1,t) ) JumpNext()
	DoFail()
}

static void PQueuePeek()
{
	Pt t ;
	if( QueuePeek(XTestQueue(X0),&t) && Unify(X1,t) ) JumpNext()
	DoFail()
}

static void PQueueWrite()
{
	QueueWrite(XTestQueue(X0)) ;
	JumpNext()
}

static void PNDCurrentQueue()
{
	QueuePt q =
		A(1) == tNilAtom ? queueList : QueueNext(cQueuePt(A(1))) ;
	A(1) = cPt(q) ;
	if( q == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagExtra(q, queueSubTag)) ) JumpNext()
	DoFail()
}

void InitQueues()
{
	errorQueue = QueueNew() ;
	ImperativeVarSet(LookupAtom("errors"), TagExtra(errorQueue, queueSubTag)) ;

	InstallCBuiltinPred("queue", 1, PQueueCheck) ;
	InstallCBuiltinPred("queue_new", 1, PQueueNew) ;
	InstallCBuiltinPred("queue_clear", 1, PQueueClear) ;
	InstallCBuiltinPred("queue_free", 1, PQueueFree) ;
	InstallCBuiltinPred("queue_put", 2, PQueuePut) ;
	InstallCBuiltinPred("queue_get", 2, PQueueGet) ;
	InstallCBuiltinPred("queue_peek", 2, PQueuePeek) ;
	InstallCBuiltinPred("queue_write", 1, PQueueWrite) ;
	InstallNDeterCBuiltinPred("current_queue", 1, PNDCurrentQueue) ;
}
