/*
 *   This file is part of the CxProlog system

 *   Stack.c
 *   by A.Miguel Dias - 2000/11/30
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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
static StackPt freeStackList = nil ;

static long StackSize(StackPt s)
{
	return StackLast(s) - StackFirst(s) ;
}

static long StackCapacity(StackPt s)
{
	return StackEnd(s) - StackBegin(s) ;
}

static void StackInit(StackPt s, long capacity)
{
	StackBegin(s) = TemporaryAllocate(capacity) ;
	StackEnd(s) = StackBegin(s) + capacity ;
	StackFirst(s) = StackBegin(s) ;
	StackLast(s) = StackBegin(s) ;
}

static void StackGrow(StackPt s)
{
	Hdl b = StackBegin(s) ;
	Hdl f = StackFirst(s) ;
	Hdl l = StackLast(s) ;
	Hdl h ;
	
	StackInit(s, 2 * StackCapacity(s)) ;
	for( h = StackBegin(s) ; f < l ; *h++ = *f++ ) ;
	StackLast(s) = h ;
	Release(b) ;
}

static void StackPark(StackPt s)
{
	Hdl f, b ;

	for( b = StackBegin(s), f = StackFirst(s) ; f < StackLast(s) ; *b++ = *f++ ) ;
	StackFirst(s) = StackBegin(s) ;
	StackLast(s) = b ;
}

static StackPt StackNew(void)
{
	StackPt s = TemporaryAllocate(WordsOf(Stack)) ;
	StackTagHolder(s) = stackSubTag ;
	StackNext(s) = stackList ;
	stackList = s ;
	StackInit(s, 4) ;
	return s ;
}

static void StackClear(StackPt s)
{
	register Hdl h ;
	
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		ReleaseTerm(*h) ;
	StackLast(s) = StackFirst(s) ;
}

static void StackDelete(StackPt s)
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

	StackBegin(s) = nil ;	/* Mark "stack not in use" */
	StackNext(s) = freeStackList ;
	freeStackList = s ;
}

static void StackPush(StackPt s, Pt t)
{
	if( StackLast(s) == StackEnd(s) )
		if( StackFirst(s) != StackBegin(s) )
			StackPark(s) ;
		else
			StackGrow(s) ;
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

static Pt StackAsList(StackPt s)
{
	Hdl h ;	
	Pt list = tNilAtom ;
	CheckGlobalStackOverflow() ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		list = MakeBinStruct(listFunctor, PushTerm(*h), list) ;
	CheckGlobalStackOverflow() ;
	return list ;
}

static void StackWrite(StreamPt stm, StackPt s)
{
	Hdl h ;
	
	WriteStream(stm, "%s", XExtraAsStr(TagExtra(s))) ;
	WriteStream(stm, "     (current capacity %ld)\n", StackCapacity(s)) ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		WriteStream(stm, "\t%s\n", TermAsStr(*h)) ;
}

static StackPt UsingStack(StackPt s)
{
	if( StackBegin(s) == nil )
		Error("Invalid operation over deleted stack %s", XExtraAsStr(TagExtra(s))) ;
	return s ;
}

Bool StackCheck(VoidPt ref)
{
	register StackPt s ;

	dolist(s, stackList, StackNext(s))
		if( s == ref )
			return StackBegin(s) != nil ;
	return false ;
}

/* CXPROLOG C'BUILTINS */

static StackPt XTestStack(register Pt t)
{
	VarValue(t) ;
	if( IsAtomOrText(t) )
		t = IVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == stackSubTag )
		return UsingStack(cStackPt(XPt(t))) ;
	TypeError("stack or ivar", t) ;
	return nil ;
}

static void PStackCheck()
{
	Pt t = Drf(X0) ;
	if( IsAtomOrText(t) )
		t = IVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == stackSubTag  && StackCheck(XPt(t)) ) JumpNext()
	DoFail()
}

static void PStackNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		StackPt s = StackNew() ;
		if( UnifyWithAtomic(t, TagExtra(s)) ) JumpNext()
		InternalError("PStackNew") ;
	}
	if( IsAtomOrText(t) ) {
		StackPt s = StackNew() ;
		IVarSet(XAtomOrTextAsAtom(t), TagExtra(s)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PStackClear()
{
	StackClear(XTestStack(X0)) ;
	JumpNext()
}

static void PStackDelete()
{
	StackDelete(XTestStack(X0)) ;
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

static void PStackAsList()
{
	if( Unify(StackAsList(XTestStack(X0)), X1) ) JumpNext()
	DoFail()
}

static void PStackWrite()
{
	StackWrite(currOut, XTestStack(X0)) ;
	JumpNext()
}

static void PSStackWrite()
{
	StackWrite(XTestStream(X0, mWrite), XTestStack(X1)) ;
	JumpNext()
}

static void PNDCurrentStack()
{
	StackPt s =
		A(1) == tNilAtom ? stackList : StackNext(cStackPt(A(1))) ;
	A(1) = cPt(s) ;
	if( s == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagExtra(s)) ) JumpNext()
	DoFail()
}

static void PStacks()
{
	StackPt s ;
	ShowVersion() ;
	Write("Stacks:\n") ;
	dolist(s, stackList, StackNext(s))
		Write("  %16s -> size =%2d, capacity =%2d\n", 
					TermAsStr(TagExtra(s)),
					StackSize(s),
					StackCapacity(s)) ;
	JumpNext()
}

void InitStacks()
{
	IVarConstSet(LookupAtom("$$_stack"), TagExtra(StackNew())) ;

	InstallCBuiltinPred("stack", 1, PStackCheck) ;
	InstallCBuiltinPred("stack_new", 1, PStackNew) ;
	InstallCBuiltinPred("stack_clear", 1, PStackClear) ;
	InstallCBuiltinPred("stack_delete", 1, PStackDelete) ;
	InstallCBuiltinPred("stack_push", 2, PStackPush) ;
	InstallCBuiltinPred("stack_pop", 2, PStackPop) ;
	InstallCBuiltinPred("stack_top", 2, PStackTop) ;
	InstallCBuiltinPred("stack_as_list", 2, PStackAsList) ;
	InstallCBuiltinPred("stack_write", 1, PStackWrite) ;
	InstallCBuiltinPred("stack_write", 2, PSStackWrite) ;
	InstallNDeterCBuiltinPred("current_stack", 1, PNDCurrentStack) ;
	InstallNDeterCBuiltinPred("stacks", 0, PStacks) ;
}