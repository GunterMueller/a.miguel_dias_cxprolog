/*
 *   This file is part of the CxProlog system

 *   Queue.c
 *   by A.Miguel Dias - 2000/08/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

static ExtraTypePt queueType ;


/* PRIVATE FUNCTIONS */

static Size QueueCapacity(QueuePt q)
{
	return QueueEnd(q) - QueueBegin(q) ;
}

static void QueueInit(QueuePt q, Size capacity)
{
	QueueBegin(q) = TempAllocate(capacity) ;
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
	Release(b) ;
}

static void QueuePark(QueuePt q)
{
	Hdl f, b ;
	for( b = QueueBegin(q), f = QueueFirst(q) ; f < QueueLast(q) ; *b++ = *f++ ) ;
	QueueFirst(q) = QueueBegin(q) ;
	QueueLast(q) = b ;
}

static void QueueWrite(StreamPt stm, QueuePt q)
{
	Hdl h ;	
	StreamWrite(stm, "%s", XExtraAsStr(TagExtra(q))) ;
	StreamWrite(stm, "     (current capacity %ld)\n", QueueCapacity(q)) ;
	for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ )
		StreamWrite(stm, "\t%s\n", TermAsStr(*h)) ;
}

static Size QueuesBasicGCMarkAux(VoidPt x)
{
	QueuePt q = cQueuePt(x) ;
	register Hdl h ;
	for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ )
		TermBasicGCMark(*h) ;
	return 1 ;
}
static void QueuesBasicGCMark()
{
	ExtraForEach(queueType, QueuesBasicGCMarkAux) ;
}


/* MAIN OPERATIONS */

Size QueueSize(QueuePt q)
{
	return QueueLast(q) - QueueFirst(q) ;
}

QueuePt QueueNew(void)
{
	QueuePt q = ExtraNew(queueType) ;
	QueueInit(q, 4) ;
	return q ;
}

void QueueClear(QueuePt q)
{
	register Hdl h ;	
	for( h = QueueFirst(q) ; h < QueueLast(q) ; h++ )
		ReleaseTerm(*h) ;
	QueueLast(q) = QueueFirst(q) ;
}

void QueueDelete(QueuePt q)
{
	QueueClear(q) ;
	Release(QueueBegin(q)) ;
	ExtraDelete(queueType, q) ;
}

void QueueFilter(QueuePt q, BFunVV filter, VoidPt x)
{
	Hdl a, z, l = QueueLast(q) ;
	for( a = z = QueueFirst(q) ; a < l ; a++ )
		if( filter(*a, x) )
			*z++ = *a ;
		else ReleaseTerm(*a) ;
	QueueLast(q) = z ;
}

Bool QueuePeek(QueuePt q, Hdl t)
{
	if( QueueFirst(q) < QueueLast(q) ) {
		*t = *QueueFirst(q) ;
		return true ;
	}
	else return false ;
}

Bool QueueGet(QueuePt q)
{
	if( QueueFirst(q) < QueueLast(q) ) {
		ReleaseTerm(*QueueFirst(q)++) ;
		return true ;
	}
	else return false ;
}

void QueuePut(QueuePt q, Pt t)
{
	if( QueueLast(q) == QueueEnd(q) ) {
		if( QueueFirst(q) != QueueBegin(q) )
			QueuePark(q) ;
		else
			QueueExpand(q) ;
	}
	*QueueLast(q)++ = AllocateTermForAssign(t) ;
}


/* CXPROLOG C'BUILTINS */

static void PQueueCheck()
{
	MustBe( XExtraCheck(queueType, X0) ) ;
}

static void PQueueNew()
{
	BindVarWithExtra(X0, QueueNew()) ;
	JumpNext() ;
}

static void PQueueClear()
{
	QueueClear(XTestExtra(queueType,X0)) ;
	JumpNext() ;
}

static void PQueueDelete()
{
	QueueDelete(XTestExtra(queueType,X0)) ;
	JumpNext() ;
}

static void PQueuePut()
{
	QueuePut(XTestExtra(queueType,X0), X1) ;
	JumpNext() ;
}

static void PQueueGet()
{
	Pt t ;
	QueuePt q = XTestExtra(queueType,X0) ;
	Ensure( QueuePeek(q, &t) ) ;
	t = ZPushTerm(t) ; /* stacks may grow */
	if( !QueueGet(q) )
		InternalError("PQueueGet") ;
	MustBe( Unify(X1, t) ) ;
}

static void PQueuePeek()
{
	Pt t ;
	Ensure( QueuePeek(XTestExtra(queueType,X0), &t) ) ;
	t = ZPushTerm(t) ; /* stacks may grow */
	MustBe( Unify(X1, t) ) ;
}

static void PQueueAsList()
{
	QueuePt q = XTestExtra(queueType,X0) ;
	Hdl h ;
	Z.t = tNilAtom ;
	for( h = QueueLast(q) - 1 ; h >= QueueFirst(q) ; h-- ) {
		Pt t = ZPushTerm(*h) ; /* stacks may grow */
		Z.t = MakeList(t, Z.t) ;
	}
	MustBe( Unify(Z.t, X1) ) ;
}

static void PQueueAsSeq()
{
	QueuePt q = XTestExtra(queueType,X0) ;
	Hdl h ;
	if( QueueSize(q) == 0 ) DoFail() ;
	h = QueueLast(q) - 1 ;
	Z.t = ZPushTerm(*h) ;
	for( h = QueueLast(q) - 2 ; h >= QueueFirst(q) ; h-- ) {
		Pt t = ZPushTerm(*h) ; /* stacks may grow */
		Z.t = MakeBinStruct(commaFunctor, t, Z.t) ;
	}
	MustBe( Unify(Z.t, X1) ) ;
}

static void PQueueWrite()
{
	QueueWrite(currOut, XTestExtra(queueType,X0)) ;
	JumpNext() ;
}

static void PSQueueWrite()
{
	QueueWrite(XTestStream(X0, mWrite), XTestExtra(queueType,X1)) ;
	JumpNext() ;
}

static void PNDCurrentQueue()
{
	ExtraPNDCurrent(queueType, nil, 1, 0) ;
	JumpNext() ;
}

static Size QueuesAux(VoidPt x)
{
	QueuePt q = cQueuePt(x) ;
	AtomPt at = IVarWith(TagExtra(q)) ;
	Write("  %16s -> size =%2ld, capacity =%2ld", 
				TermAsStr(TagExtra(q)),
				QueueSize(q),
				QueueCapacity(q)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
	return 1 ;
}
static void PQueues()
{
	ExtraShow(queueType, QueuesAux) ;
	JumpNext() ;
}

/* TEST, EXTRACT & INIT */

Bool IsQueue(Pt t)
{
	return IsThisExtra(queueType, t) ;
}

QueuePt XTestQueue(Pt t)
{
	return XTestExtra(queueType, t) ;
}

void QueuesInit()
{
	queueType = ExtraTypeNew("QUEUE", WordsOf(Queue), nil, nil) ;
	InstallBasicGCHandler(QueuesBasicGCMark) ;
	/* add "queues." to CxProlog.c/PShow */

	InstallCBuiltinPred("queue", 1, PQueueCheck) ;
	InstallCBuiltinPred("queue_new", 1, PQueueNew) ;
	InstallCBuiltinPred("queue_clear", 1, PQueueClear) ;
	InstallCBuiltinPred("queue_delete", 1, PQueueDelete) ;
	InstallCBuiltinPred("queue_put", 2, PQueuePut) ;
	InstallCBuiltinPred("queue_get", 2, PQueueGet) ;
	InstallCBuiltinPred("queue_peek", 2, PQueuePeek) ;
	InstallCBuiltinPred("queue_as_list", 2, PQueueAsList) ;
	InstallCBuiltinPred("queue_as_seq", 2, PQueueAsSeq) ;
	InstallCBuiltinPred("queue_write", 1, PQueueWrite) ;
	InstallCBuiltinPred("queue_write", 2, PSQueueWrite) ;
	InstallNDeterCBuiltinPred("current_queue", 1, PNDCurrentQueue) ;
	InstallCBuiltinPred("queues", 0, PQueues) ;
}
