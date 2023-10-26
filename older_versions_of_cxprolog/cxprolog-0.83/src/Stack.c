/*
 *   This file is part of the CxProlog system

 *   Stack.c
 *   by A.Miguel Dias - 2000/11/30
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

typedef struct Stack
{
	ExtraDef(Stack) ;
	Hdl begin, end, first, last  ;
} Stack, *StackPt ;

#define cStackPt(x)				((StackPt)(x))

#define StackBegin(s)			((s)->begin)
#define StackEnd(s)				((s)->end)
#define StackFirst(s)			((s)->first)
#define StackLast(s)			((s)->last)

static ExtraTypePt stackType ;

static Size StackSize(StackPt s)
{
	return StackLast(s) - StackFirst(s) ;
}

static Size StackCapacity(StackPt s)
{
	return StackEnd(s) - StackBegin(s) ;
}

static void StackInit(StackPt s, Size capacity)
{
	StackBegin(s) = TempBlockAllocate(capacity) ;
	StackEnd(s) = StackBegin(s) + capacity ;
	StackFirst(s) = StackBegin(s) ;
	StackLast(s) = StackBegin(s) ;
}

static void StackExpand(StackPt s)
{
	Hdl b = StackBegin(s) ;
	Hdl f = StackFirst(s) ;
	Hdl l = StackLast(s) ;
	Hdl h ;	
	StackInit(s, 2 * StackCapacity(s)) ;
	for( h = StackBegin(s) ; f < l ; *h++ = *f++ ) ;
	StackLast(s) = h ;
	BlockRelease(b) ;
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
	StackPt s = ExtraNew(stackType) ;
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
	StackClear(s) ;
	BlockRelease(StackBegin(s)) ;
	ExtraDelete(stackType, s) ;
}

static void StackPush(StackPt s, Pt t)
{
	if( StackLast(s) == StackEnd(s) )
		if( StackFirst(s) != StackBegin(s) )
			StackPark(s) ;
		else
			StackExpand(s) ;
	*StackLast(s)++ = AllocateTermForAssign(t) ;
}

static Bool StackPop(StackPt s)
{
	if( StackFirst(s) < StackLast(s) ) {
		ReleaseTerm(*--StackLast(s)) ;
		return true ;
	}
	else return false ;
}

static Bool StackTop(StackPt s, Hdl t)
{
	if( StackFirst(s) < StackLast(s) ) {
		*t = *(StackLast(s)-1) ;
		return true ;
	}
	else return false ;
}

static void StackWrite(StreamPt stm, StackPt s)
{
	Hdl h ;
	StreamWrite(stm, "%s", XExtraAsStr(TagExtra(s))) ;
	StreamWrite(stm, "     (current capacity %ld)\n", StackCapacity(s)) ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		StreamWrite(stm, "\t%s\n", TermAsStr(*h)) ;
}

static void StacksAtomGCMarkAux(VoidPt x)
{
	StackPt s = cStackPt(x) ;
	register Hdl h ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		TermAtomGCMark(*h) ;
}
void StacksAtomGCMark()
{
	ExtraForEach(stackType, StacksAtomGCMarkAux) ;
}

/* CXPROLOG C'BUILTINS */

static void PStackCheck()
{
	if( XExtraCheck(stackType, X0) ) JumpNext()
	DoFail()
}

static void PStackNew()
{
	BindVarWithExtra(X0, StackNew()) ;
	JumpNext()
}

static void PStackClear()
{
	StackClear(XTestExtra(stackType,X0)) ;
	JumpNext()
}

static void PStackDelete()
{
	StackDelete(XTestExtra(stackType,X0)) ;
	JumpNext()
}

static void PStackPush()
{
	StackPush(XTestExtra(stackType,X0), X1) ;
	JumpNext()
}

static void PStackPop()
{
	StackPt s = XTestExtra(stackType,X0) ;
	Pt t ;
	if( StackTop(s, &t) ) {
		t = ZPushTerm(t) ;
		if( not StackPop(s) )
			InternalError("PStackPop") ;
		if( Unify(X1,t) ) JumpNext()
	}
	DoFail()
}

static void PStackTop()
{
	Pt t ;
	if( StackTop(XTestExtra(stackType,X0), &t) ) {
		t = ZPushTerm(t) ;
		if( Unify(X1, t) ) JumpNext()
	}
	DoFail()
}

static void PStackAsList()
{
	StackPt s = XTestExtra(stackType,X0) ;
	Hdl h ;
	Z = tNilAtom ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ ) {
		Pt t = ZPushTerm(*h) ;
		Z = MakeBinStruct(listFunctor, t, Z) ;
	}
	if( Unify(Z, X1) ) JumpNext()
	DoFail()
}

static void PStackWrite()
{
	StackWrite(currOut, XTestExtra(stackType,X0)) ;
	JumpNext()
}

static void PSStackWrite()
{
	StackWrite(XTestStream(X0, mWrite), XTestExtra(stackType,X1)) ;
	JumpNext()
}

static void PNDCurrentStack()
{
	PNDCurrentExtra(stackType) ;
	JumpNext()
}

static void StacksAux(VoidPt x)
{
	StackPt s = cStackPt(x) ;
	AtomPt at = IVarWith(TagExtra(s)) ;
	Write("  %16s -> size =%2ld, capacity =%2ld", 
				TermAsStr(TagExtra(s)),
				StackSize(s),
				StackCapacity(s)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
}
static void PStacks()
{
	VersionShow() ;
	Write("Stacks:\n") ;
	ExtraForEach(stackType, StacksAux) ;
	JumpNext()
}

void StacksInit()
{
	stackType = ExtraTypeNew("stack", WordsOf(Stack)) ;

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
