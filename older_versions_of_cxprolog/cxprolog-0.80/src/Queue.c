/*
 *   This file is part of the CxProlog system

 *   Queue.c
 *   by A.Miguel Dias - 2000/08/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
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
static QueuePt freeQueueList = nil ;

static Size QueueSize(QueuePt q)
{
	return QueueLast(q) - QueueFirst(q) ;
}

static Size QueueCapacity(QueuePt q)
{
	return QueueEnd(q) - QueueBegin(q) ;
}

static void QueueInit(QueuePt q, Size capacity)
{
	QueueBegin(q) = TempBlockAllocate(capacity) ;
	QueueEnd(q) = QueueBegin(q) + capacity ;
	QueueFirst(q) = QueueBegin(q) ;
	QueueLast(q) = QueueBegin(q) ;
}

static void QueueExpand(QueuePt q)
{
	Hdl b = QueueBegin(q) ;
	Hdl f = QueueFirst(q) ;
	Hdl l = QueueLast(q) ;
	Hdl h ;
	
	QueueInit(q, 2 * QueueCapacity(q)) ;
	for( h = QueueBegin(q) ; f < l ; *h++ = *f++ ) ;
	QueueLast(q) = h ;
	BlockRelease(b) ;
}

static void QueuePark(QueuePt q)
{
	Hdl f, b ;

	for( b = QueueBegin(q), f = QueueFirst(q) ; f < QueueLast(q) ; *b++ = *f++ ) ;
	QueueFirst(q) = QueueBegin(q) ;
	QueueLast(q) = b ;
}

static QueuePt QueueNew(void)
{
	QueuePt q = TempBlockAllocate(WordsOf(Queue)) ;
	QueueTagHolder(q) = queueSubTag ;
	QueueNext(q) = queueList ;
	queueList = q ;
	QueueInit(q, 4) ;
	return q ;
}

static void QueueClear(QueuePt q)
{
	register Hdl h ;
	
	for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ )
		ReleaseTerm(*h) ;
	QueueLast(q) = QueueFirst(q) ;
}

static void QueueRelease(QueuePt q)
{
	QueueClear(q) ;
	BlockRelease(QueueBegin(q)) ;
	QueueBegin(q) = nil ;	/* Mark "queue not in use" */
	QueueNext(q) = freeQueueList ;
	freeQueueList = q ;
}

static void QueueDelete(QueuePt q)
{
	if( queueList == q )	/* first is removed */
		queueList = QueueNext(q) ;
	else {
		register QueuePt x ;
		for( x = queueList ; QueueNext(x) != q ; x = QueueNext(x) ) ;
		QueueNext(x) = QueueNext(q) ;
	}
	QueueRelease(q) ;
}

static void QueuePut(QueuePt q, Pt t)
{
	if( QueueLast(q) == QueueEnd(q) )
		if( QueueFirst(q) != QueueBegin(q) )
			QueuePark(q) ;
		else
			QueueExpand(q) ;
	*QueueLast(q)++ = AllocateTermForAssign(t) ;
}

static Bool QueueGet(QueuePt q)
{
	if( QueueFirst(q) < QueueLast(q) ) {
		ReleaseTerm(*QueueFirst(q)++) ;
		return true ;
	}
	else return false ;
}

static Bool QueuePeek(QueuePt q, Hdl t)
{
	if( QueueFirst(q) < QueueLast(q) ) {
		*t = *QueueFirst(q) ;
		return true ;
	}
	else return false ;
}

static void QueueWrite(StreamPt stm, QueuePt q)
{
	Hdl h ;	
	StreamWrite(stm, "%s", XExtraAsStr(TagExtra(q))) ;
	StreamWrite(stm, "     (current capacity %ld)\n", QueueCapacity(q)) ;
	for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ )
		StreamWrite(stm, "\t%s\n", TermAsStr(*h)) ;
}

static QueuePt UsingQueue(QueuePt q)
{
	if( QueueBegin(q) == nil )
		ImperativeError("Invalid operation over deleted queue %s",
								XExtraAsStr(TagExtra(q))) ;
	return q ;
}

Bool QueueCheck(VoidPt ref)
{
	register QueuePt q ;
	dolist(q, queueList, QueueNext(q))
		if( q == ref )
			return QueueBegin(q) != nil ;
	return false ;
}

void QueuesAtomGCMark()
{
	register QueuePt q ;
	register Hdl h ;
	dolist(q, queueList, QueueNext(q))
		if( QueueBegin(q) != nil )
			for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ )
				TermAtomGCMark(*h) ;
}


/* CXPROLOG C'BUILTINS */

static QueuePt XTestQueue(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) )
		t = IVarGet(XAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == queueSubTag )
		return UsingQueue(cQueuePt(XPt(t))) ;
	TypeError2("QUEUE or IVAR", t) ;
	return nil ;
}

static void PQueueCheck()
{
	Pt t = Drf(X0) ;
	if( IsAtom(t) )
		t = IVarGet(XAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == queueSubTag && QueueCheck(XPt(t)) )
		JumpNext()
	DoFail()
}

static void PQueueNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		QueuePt q = QueueNew() ;
		if( UnifyWithAtomic(t, TagExtra(q)) ) JumpNext()
		InternalError("PQueueNew") ;
	}
	if( IsAtom(t) ) {
		QueuePt q = QueueNew() ;
		IVarSet(XAtom(t), TagExtra(q)) ;
		JumpNext()
	}
	TypeError2("VAR or IVAR", t) ;
}

static void PQueueClear()
{
	QueueClear(XTestQueue(X0)) ;
	JumpNext()
}

static void PQueueDelete()
{
	QueueDelete(XTestQueue(X0)) ;
	JumpNext()
}

static void PQueuePut()
{
	QueuePut(XTestQueue(X0),X1) ;
	JumpNext()
}

static void PQueueGet()
{
	QueuePt q = XTestQueue(X0) ;
	Pt t ;
	if( QueuePeek(q, &t) ) {
		t = ZPushTerm(t) ;
		if( not QueueGet(q) )
			InternalError("PQueueGet") ;
		if( Unify(X1,t) ) JumpNext()
	}
	DoFail()
}

static void PQueuePeek()
{
	Pt t ;
	if( QueuePeek(XTestQueue(X0), &t) ) {
		t = ZPushTerm(t) ;
		if( Unify(X1,t) ) JumpNext()
	}
	DoFail()
}

static void PQueueAsList()
{
	QueuePt q = XTestQueue(X0) ;
	Hdl h ;
	Z = tNilAtom ;
	for( h = QueueLast(q) - 1 ; h >= QueueFirst(q) ; h-- ) {
		Pt t = ZPushTerm(*h) ;
		Z = MakeBinStruct(listFunctor, t, Z) ;
	}
	if( Unify(Z, X1) ) JumpNext()
	DoFail()
}

static void PQueueWrite()
{
	QueueWrite(currOut, XTestQueue(X0)) ;
	JumpNext()
}

static void PSQueueWrite()
{
	QueueWrite(XTestStream(X0, mWrite), XTestQueue(X1)) ;
	JumpNext()
}

static void PNDCurrentQueue()
{
	QueuePt q =
		A(1) == tNilAtom ? queueList : QueueNext(cQueuePt(A(1))) ;
	A(1) = cPt(q) ;
	if( q == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagExtra(q)) ) JumpNext()
	DoFail()
}

static void PQueues()
{
	QueuePt q ;
	VersionShow() ;
	Write("Queues:\n") ;
	dolist(q, queueList, QueueNext(q)) {
		AtomPt at = IVarWith(TagExtra(q)) ;
		Write("  %16s -> size =%2d, capacity =%2d", 
					TermAsStr(TagExtra(q)),
					QueueSize(q),
					QueueCapacity(q)) ;
		if( at != nil )
			Write(" (in ivar '%s')", AtomName(at)) ;
		Write("\n") ;
	}
	JumpNext()
}

void QueuesInit()
{
	IVarConstSet(LookupAtom("$queue"), TagExtra(QueueNew())) ;

	InstallCBuiltinPred("queue", 1, PQueueCheck) ;
	InstallCBuiltinPred("queue_new", 1, PQueueNew) ;
	InstallCBuiltinPred("queue_clear", 1, PQueueClear) ;
	InstallCBuiltinPred("queue_delete", 1, PQueueDelete) ;
	InstallCBuiltinPred("queue_put", 2, PQueuePut) ;
	InstallCBuiltinPred("queue_get", 2, PQueueGet) ;
	InstallCBuiltinPred("queue_peek", 2, PQueuePeek) ;
	InstallCBuiltinPred("queue_as_list", 2, PQueueAsList) ;
	InstallCBuiltinPred("queue_write", 1, PQueueWrite) ;
	InstallCBuiltinPred("queue_write", 2, PSQueueWrite) ;
	InstallNDeterCBuiltinPred("current_queue", 1, PNDCurrentQueue) ;
	InstallNDeterCBuiltinPred("queues", 0, PQueues) ;
}
