/*
 *   This file is part of the CxProlog system

 *   TermExtra.c
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

/* C PREDs cannot have local variables */

static Bool b, b0, b1 ;
static Int i, i0, i1 ;
static Real r, r0, r1 ;
static Pt t, t0, t1 ;
static Hdl h0, h1 ;
static CharPt s, s0, s1 ;

/* EXTRA PRIMITIVE TYPES AND CORRESPONDING C SYSTEM PREDICATES */

Pt MakeExtra(CharPt str, int subcode)
{
	switch( subcode ) {
		case textSubTag: {
			h0 = H ;
			GrowGlobal(Words(strlen(str)+2)) ;
			cCharPt(h0)[0] = textSubTag ;
			strcpy(cCharPt(h0)+1, str) ;
			return TagExtra(h0) ;
		}
		default:
			Default("MakeExtra") ;
	}
}

int GetExtraSize(Pt term)
{
	switch( XExtraSubTag(t) ) {
		case textSubTag: {
			return Words(strlen(XExtra(t) + 1)+2) ;
		}
		default:
			Default("GetExtraSize") ;
	}
}

Pt MakeExtraPermanent(Pt t)
{
	switch( XExtraSubTag(t) ) {
		case textSubTag: {
			return TagExtra(AtomName(LookupAtom(XExtra(t)))) ;
		}
		default:
			Default("MakeExtraPermanent") ;
	}
}

CharPt XExtraName(Pt t)
{
	switch( XExtraSubTag(t) ) {
		case textSubTag: {
			return XExtra(t) + 1 ;
		}
		default:
			Default("XExtraName") ;
	}
}

Bool EqExtra(Pt t1, Pt t2) 
{
	if( XExtraSubTag(t1) != XExtraSubTag(t2) )
		return false ;

	switch( XExtraSubTag(t1) ) {
		case textSubTag: {
			return EqualStr(XExtraName(t1), XExtraName(t2)) ;
		}
		default:
			Default("EqExtra") ;
	}
}

void WriteExtra(Pt t) 
{
	switch( XExtraSubTag(t) ) {
		case textSubTag: {
			Put('`') ;
			PutString(XExtra(t) + 1) ;
			Put('`') ;
			break ;
		}
		default:
			Default("WriteExtra") ;
	}
}


/* TEXT EXTRA TYPE: SPECIFIC C SYSTEM PREDICATES */


static CharPt XTestExtra(register Pt t)
{
	VarValue(t) ;
	if( IsExtra(t) && XExtraSubTag(t) == textSubTag ) return XExtra(t) ;
	TypeError("extra", t) ;
}

static void PText()
{
	t0 = Drf(X0) ;
	if( IsExtra(t0) && XExtraSubTag(t0) == textSubTag ) JumpNext()
	DoFail()
}

static void PGetText()
{
	if( UnifyWithAtomic(X0, MakeExtra(GetLine(), textSubTag)) ) JumpNext()
	DoFail()
}

static void PPutText()
{
	PutString(XTestExtra(X0) + 1) ;
	JumpNext()
}

static void PLenText()
{
	i = strlen(XTestExtra(X0) + 1) ;
	if( Unify(X1, MakeInt(i)) ) JumpNext()
	DoFail()
}

static void PSliceText()
{
	s = XTestExtra(X0) + 1 ;
	i = strlen(s) ;
	i0 = XTestInt(X1) ;
	i1 = XTestInt(X2) ;
	if( i0 < 1 ) i0 = 1 ;
	if( i1 > i ) i1 = i ;
	if( i1 < i0 ) DoFail()
	strncpy(strBuffer, s + i0 - 1, i1 - i0 + 1) ;
	if( Unify(X3, MakeExtra(strBuffer, textSubTag)) ) JumpNext()
	DoFail()
}

static void PNameText()
{
	VarValue2(t0, X0) ;
	VarValue2(t1, X1) ;
	if( IsAtom(t0) ) {
		if( Unify(t1, MakeExtra(XAtomName(t0), textSubTag)) ) JumpNext()
		DoFail()
	}
	elif( IsExtra(t1) && XExtraSubTag(t1) == textSubTag ) {
		if( UnifyWithAtomic(t0, MakeAtom(XExtra(t1) + 1)) ) JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PConcatText()
{
	if( Unify(X1, MakeExtra(PConcatString(X0), textSubTag)) ) JumpNext()
	DoFail()
}


/* */

void InstallExtraCBuiltinPreds()
{
	InstallCBuiltinPred("text", 1, PText) ;
	InstallCBuiltinPred("gett", 1, PGetText) ;
	InstallCBuiltinPred("putt", 1, PPutText) ;
	InstallCBuiltinPred("slicet", 4, PSliceText) ;
	InstallCBuiltinPred("namet", 2, PNameText) ;
	InstallCBuiltinPred("concatt", 2, PConcatText) ;
}
