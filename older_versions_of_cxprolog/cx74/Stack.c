/*
 *   This file is part of the CxProlog system

 *   Stack.c
 *   by A.Miguel Dias - 2000/11/30
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

typedef struct Stack
{
	Word tagHolder ;
	Hdl begin, end, first, last  ;
	struct Stack *next ;
} Stack, *StackPt ;

#define StackTagHolder(s)		((s)->tagHolder)
#define StackBegin(s)			((s)->begin)
#define StackEnd(s)				((s)->end)
#define StackFirst(s)			((s)->first)
#define StackLast(s)			((s)->last)
#define StackNext(s)			((s)->next)

#define cStackPt(x)				((StackPt)(x))


static StackPt stackList = nil ;

static long StackSize(StackPt s)
{
	return StackLast(s) - StackFirst(s) ;
}

static long StackCapacity(StackPt s)
{
	return StackEnd(s) - StackBegin(s) ;
}

static void InitStack(StackPt s, long capacity)
{
	StackBegin(s) = TemporaryAllocate(capacity) ;
	StackEnd(s) = StackBegin(s) + capacity ;
	StackFirst(s) = StackBegin(s) ;
	StackLast(s) = StackBegin(s) ;
}

static void GrowStack(StackPt s)
{
	Hdl b = StackBegin(s) ;
	Hdl f = StackFirst(s) ;
	Hdl l = StackLast(s) ;
	Hdl h ;
	
	InitStack(s, 2 * StackCapacity(s)) ;
	for( h = StackBegin(s) ; f < l ; *h++ = *f++ ) ;
	StackLast(s) = h ;
	Release(b) ;
}

static void ParkStack(StackPt s)
{
	Hdl f, b ;

	for( b = StackBegin(s), f = StackFirst(s) ; f < StackLast(s) ; *b++ = *f++ ) ;
	StackFirst(s) = StackBegin(s) ;
	StackLast(s) = b ;
}

static StackPt StackNew(void)
{
	StackPt s = TemporaryAllocate(WordsOf(Stack)) ;
	StackTagHolder(s) = 0 ;
	StackNext(s) = stackList ;
	stackList = s ;
	InitStack(s, 4) ;
	return s ;
}

static void StackClear(StackPt s)
{
	register Hdl h ;
	
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		ReleaseTerm(*h) ;
	StackLast(s) = StackFirst(s) ;
}

static void StackFree(StackPt s)
{
	if( stackList == s )	/* first is removed */
		stackList = StackNext(s) ;
	else {
		register StackPt x ;
		for( x = stackList ; StackNext(x) != s ; x = StackNext(x) ) ;
		StackNext(x) = StackNext(s) ;
	}
	StackClear(s) ;
	Release(StackBegin(s)) ;
	Release(s) ;
}

static void StackPush(StackPt s, Pt t)
{
	if( StackLast(s) == StackEnd(s) )
		if( StackFirst(s) != StackBegin(s) )
			ParkStack(s) ;
		else
			GrowStack(s) ;
	*StackLast(s)++ = AllocateTermForAssign(t) ;
}

static Bool StackPop(StackPt s, Hdl t)
{
	if( StackFirst(s) < StackLast(s) ) {
		*t = PushTerm(*--StackLast(s)) ;
		ReleaseTerm(*StackLast(s)) ;
		return true ;
	}
	else return false ;
}

static Bool StackTop(StackPt s, Hdl t)
{
	if( StackFirst(s) < StackLast(s) ) {
		*t = PushTerm(*(StackLast(s)-1)) ;
		return true ;
	}
	else return false ;
}

static void StackWrite(StackPt s)
{
	Hdl h ;
	
	Write("%s", XExtraAsStr(TagExtra(s, stackSubTag))) ;
	Write("     (current capacity %ld)\n", StackCapacity(s)) ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ ) {
		Write("\t") ;
		WriteTerm(*h) ;
		Nl() ;
	}
}

Bool StackCheck(Pt t)
{
	register StackPt s ;

	dolist(s, stackList, StackNext(s))
		if( cPt(s) == t )
			return true ;
	return false ;
}


/* CXPROLOG C'BUILTINS */

static StackPt XTestStack(register Pt t)
{
	VarValue(t) ;
	if( IsAtomOrText(t) )
		t = ImperativeVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == stackSubTag )
		return cStackPt(XPt(t)) ;
	TypeError("stack or ivar", t) ;
	return nil ;
}

static void PStackCheck()
{
	Pt t = Drf(X0) ;
	if( IsAtomOrText(t) )
		t = ImperativeVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == stackSubTag ) JumpNext()
	DoFail()
}

static void PStackNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		StackPt s = StackNew() ;
		if( UnifyWithAtomic(t, TagExtra(s, stackSubTag)) ) JumpNext()
		InternalError("PStackNew") ;
	}
	if( IsAtomOrText(t) ) {
		StackPt s = StackNew() ;
		ImperativeVarSet(XAtomOrTextAsAtom(t), TagExtra(s, stackSubTag)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PStackClear()
{
	StackClear(XTestStack(X0)) ;
	JumpNext()
}

static void PStackFree()
{
	StackFree(XTestStack(X0)) ;
	JumpNext()
}

static void PStackPush()
{
	StackPush(XTestStack(X0),X1) ;
	JumpNext()
}

static void PStackPop()
{
	Pt t ;
	if( StackPop(XTestStack(X0),&t) && Unify(X1,t) ) JumpNext()
	DoFail()
}

static void PStackTop()
{
	Pt t ;
	if( StackTop(XTestStack(X0),&t) && Unify(X1,t) ) JumpNext()
	DoFail()
}

static void PStackWrite()
{
	StackWrite(XTestStack(X0)) ;
	JumpNext()
}

static void PNDCurrentStack()
{
	StackPt s =
		A(1) == tNilAtom ? stackList : StackNext(cStackPt(A(1))) ;
	A(1) = cPt(s) ;
	if( s == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagExtra(s, stackSubTag)) ) JumpNext()
	DoFail()
}

void InitStacks()
{
	InstallCBuiltinPred("stack", 1, PStackCheck) ;
	InstallCBuiltinPred("stack_new", 1, PStackNew) ;
	InstallCBuiltinPred("stack_clear", 1, PStackClear) ;
	InstallCBuiltinPred("stack_free", 1, PStackFree) ;
	InstallCBuiltinPred("stack_push", 2, PStackPush) ;
	InstallCBuiltinPred("stack_pop", 2, PStackPop) ;
	InstallCBuiltinPred("stack_top", 2, PStackTop) ;
	InstallCBuiltinPred("stack_write", 1, PStackWrite) ;
	InstallNDeterCBuiltinPred("current_stack", 1, PNDCurrentStack) ;
}
