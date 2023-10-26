/*
 *   This file is part of the CxProlog system

 *   Extra.c
 *   by A.Miguel Dias - 1999/12/30
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


/* EXTRA PRIMITIVE TYPES AND CORRESPONDING C SYSTEM PREDICATES */

#if 0
int GetExtraSize(Pt term)
{
	Pt t ;

	switch( XExtraSubTag(t) ) {
		case queueSubTag: {
			return QueueSize(cQueuePt(XExtra(term))) ;
		}
		default:
			Default("GetExtraSize") ;
	}
}
#endif

Pt MakeExtraPermanent(Pt t)
{
	switch( XExtraSubTag(t) ) {
		case queueSubTag: {
			return t ;
		}
		default:
			Default("MakeExtraPermanent") ;
	}
}


Bool EqExtra(Pt t1, Pt t2) 
{
	if( XExtraSubTag(t1) != XExtraSubTag(t2) )
		return false ;

	switch( XExtraSubTag(t1) ) {
		case queueSubTag: {
			return false ;
		}
		default:
			Default("EqExtra") ;
	}
}

CharPt XExtraAsStr(Pt t) 
{
	switch( XExtraSubTag(t) ) {
		case queueSubTag: {
			sprintf(strBuffer, "88'queue(%lx)", XPt(t)) ;
			break ;
		}
		default:
			Default("ExtraAsStr") ;
	}
	return strBuffer ;
}


/* TEXT EXTRA TYPE: SPECIFIC C SYSTEM PREDICATES */

#if 0

#define textSubTag 111

static CharPt XTestExtra(register Pt t)
{
	VarValue(t) ;
	if( IsExtra(t) && XExtraSubTag(t) == textSubTag ) return XExtra(t) ;
	TypeError("extra", t) ;
}

static void PText()
{
	Pt t0 = Drf(X0) ;
	if( IsExtra(t0) && XExtraSubTag(t0) == textSubTag ) JumpNext()
	DoFail()
}

static void PGetText()
{
	if( UnifyWithAtomic(X0, MakeExtra(GetLine(), textSubTag)) ) JumpNext()
	DoFail()
}

static void PLenText()
{
	Int i = strlen(XTestExtra(X0) + 1) ;
	if( Unify(X1, MakeInt(i)) ) JumpNext()
	DoFail()
}

static void PSliceText()
{
	CharPt s = XTestExtra(X0) + 1 ;
	Int i = strlen(s) ;
	Int i0 = XTestInt(X1) ;
	Int i1 = XTestInt(X2) ;
	if( i0 < 1 ) i0 = 1 ;
	if( i1 > i ) i1 = i ;
	if( i1 < i0 ) DoFail()
	strncpy(strBuffer, s + i0 - 1, i1 - i0 + 1) ;
	if( Unify(X3, MakeExtra(strBuffer, textSubTag)) ) JumpNext()
	DoFail()
}

static void PNameText()
{
	Pt t0, t1 ;

	VarValue2(t0, X0) ;
	VarValue2(t1, X1) ;
	if( IsAtom(t0) ) {
		if( Unify(t1, MakeExtra(XAtomOrTextName(t0), textSubTag)) ) JumpNext()
		DoFail()
	}
	elif( IsExtra(t1) && XExtraSubTag(t1) == textSubTag ) {
		if( UnifyWithAtomic(t0, MakeAtomOrText(XExtra(t1) + 1)) ) JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PConcatText()
{
	if( Unify(X1, MakeExtra(PConcatString(X0), textSubTag)) ) JumpNext()
	DoFail()
}

static void PQuote()
{
	Pt t ;

	if( Unify(X1, MakeExtra(Quote(XTestAtomName(X0)), textSubTag)) )
		JumpNext()
	DoFail()
}

#endif

/* */

void InstallExtraCBuiltinPreds()
{
#if 0
	InstallCBuiltinPred("text", 1, PText) ;
	InstallCBuiltinPred("gett", 1, PGetText) ;
	InstallCBuiltinPred("slicet", 4, PSliceText) ;
	InstallCBuiltinPred("namet", 2, PNameText) ;
	InstallCBuiltinPred("concatt", 2, PConcatText) ;
	InstallCBuiltinPred("quote", 2, PQuote) ;
#endif
}