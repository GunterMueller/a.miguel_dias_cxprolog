/*
 *   This file is part of the CxProlog system

 *   Stack.c
 *   by A.Miguel Dias - 2000/11/30
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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

static ExtraTypePt stackType ;


/* PRIVATE FUNCTIONS */

static Size StackCapacity(StackPt s)
{
	return StackEnd(s) - StackBegin(s) ;
}

static void StackInit(StackPt s, Size capacity)
{
	StackBegin(s) = TempAllocate(capacity) ;
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
	Release(b) ;
}

static void StackPark(StackPt s)
{
	Hdl f, b ;
	for( b = StackBegin(s), f = StackFirst(s) ; f < StackLast(s) ; *b++ = *f++ ) ;
	StackFirst(s) = StackBegin(s) ;
	StackLast(s) = b ;
}

static void StackWrite(StreamPt stm, StackPt s)
{
	Hdl h ;
	StreamWrite(stm, "%s", XExtraAsStr(TagExtra(s))) ;
	StreamWrite(stm, "     (current capacity %ld)\n", StackCapacity(s)) ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		StreamWrite(stm, "\t%s\n", TermAsStr(*h)) ;
}

static Size StacksAtomGCMarkAux(VoidPt x)
{
	StackPt s = cStackPt(x) ;
	register Hdl h ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		TermAtomGCMark(*h) ;
	return 0 ;
}
static void StacksAtomGCMark()
{
	ForEachExtra(stackType, StacksAtomGCMarkAux) ;
}


/* MAIN OPERATIONS */

Size StackSize(StackPt s)
{
	return StackLast(s) - StackFirst(s) ;
}

StackPt StackNew(void)
{
	StackPt s = ExtraNew(stackType) ;
	StackInit(s, 4) ;
	return s ;
}

void StackClear(StackPt s)
{
	register Hdl h ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ )
		ReleaseTerm(*h) ;
	StackLast(s) = StackFirst(s) ;
}

void StackDelete(StackPt s)
{
	StackClear(s) ;
	Release(StackBegin(s)) ;
	ExtraDelete(stackType, s) ;
}

Bool StackTop(StackPt s, Hdl t)
{
	if( StackFirst(s) < StackLast(s) ) {
		*t = *(StackLast(s)-1) ;
		return true ;
	}
	else return false ;
}

void StackPush(StackPt s, Pt t)
{
	if( StackLast(s) == StackEnd(s) ) {
		if( StackFirst(s) != StackBegin(s) )
			StackPark(s) ;
		else
			StackExpand(s) ;
	}
	*StackLast(s)++ = AllocateTermForAssign(t) ;
}

Bool StackPop(StackPt s)
{
	if( StackFirst(s) < StackLast(s) ) {
		ReleaseTerm(*--StackLast(s)) ;
		return true ;
	}
	else return false ;
}


/* CXPROLOG C'BUILTINS */

static void PStackCheck()
{
	MustBe( XExtraCheck(stackType, X0) )
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
	Pt t ;
	StackPt s = XTestExtra(stackType,X0) ;
	Ensure( StackTop(s, &t) )
	t = ZPushTerm(t) ; /* stacks may grow */
	if( !StackPop(s) )
		InternalError("PStackPop") ;
	MustBe( Unify(X1, t) )

}

static void PStackTop()
{
	Pt t ;
	Ensure( StackTop(XTestExtra(stackType,X0), &t) )
	t = ZPushTerm(t) ; /* stacks may grow */
	MustBe( Unify(X1, t) )
}

static void PStackAsList()
{
	StackPt s = XTestExtra(stackType,X0) ;
	Hdl h ;
	Z = tNilAtom ;
	for( h = StackFirst(s) ; h < StackLast(s) ; h++ ) {
		Pt t = ZPushTerm(*h) ; /* stacks may grow */
		Z = MakeList(t, Z) ;
	}
	MustBe( Unify(Z, X1) )
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
	PNDCurrentExtra(stackType, nil, 1) ;
	JumpNext()
}

static Size StacksAux(VoidPt x)
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
	return 0 ;
}
static void PStacks()
{
	ShowVersion() ;
	Write("Stacks:\n") ;
	ForEachExtra(stackType, StacksAux) ;
	JumpNext()
}


/* TEST, EXTRACT & INIT */

Bool IsStack(Pt t)
{
	return IsThisExtra(stackType, t) ;
}

StackPt XTestStack(Pt t)
{
	return XTestExtra(stackType, t) ;
}

void StacksInit()
{
	stackType = ExtraTypeNew("STACK", WordsOf(Stack)) ;
	InstallAtomGCHandler(StacksAtomGCMark) ;
	/* add "stacks." to CxProlog.c/PShow */

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
	InstallCBuiltinPred("stacks", 0, PStacks) ;
}
