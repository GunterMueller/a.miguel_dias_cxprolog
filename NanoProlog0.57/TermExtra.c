/*
 *   This file is part of the NanoProlog system

 *   TermExtra.c
 *   by A.Miguel Dias - 99/12/30
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990,...,2000 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 000324: first version with release 0.57

*/

#include "NanoProlog.h"

/* */

#define X0		Xc(0)
#define X1		Xc(1)
#define X2		Xc(2)
#define X3		Xc(3)
#define X4		Xc(4)

#define R0		Drf(X0)
#define R1		Drf(X1)
#define R2		Drf(X2)
#define R3		Drf(X3)
#define R4		Drf(X4)

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
	switch( subcode )
	{
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

Pt GroundExtra(Pt t)
{
	switch( XExtraSubTag(t) )
	{
		case textSubTag: {
			return TagExtra(AtomName(LookupAtom(XExtra(t)))) ;
		}
		default:
			Default("GroundExtra") ;
	}
}

CharPt XExtraName(Pt t)
{
	switch( XExtraSubTag(t) )
	{
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

	switch( XExtraSubTag(t1) )
	{
		case textSubTag: {
			return strcmp(XExtraName(t1), XExtraName(t2)) == 0 ;
		}
		default:
			Default("EqExtra") ;
	}
}

void WriteExtra(Pt t) 
{
	switch( XExtraSubTag(t) )
	{
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

static void PText()
{
	if( IsExtra(R0) && XExtraSubTag(R0) == textSubTag ) JumpNext()
	DoFail()
}

static void PGetText()
{
	if( UnifyWithAtomic(X0, MakeExtra(GetLine(), textSubTag)) ) JumpNext()
	DoFail()
}

static void PPutText()
{
	TypeCheck("T") ;
	PutString(XExtra(R0) + 1) ;
	JumpNext()
}

static void PLenText()
{
	TypeCheck("T") ;
	i = strlen(XExtra(R0) + 1) ;
	if( Unify(X1, MakeInt(i)) ) JumpNext()
	DoFail()
}

static void PSliceText()
{
	TypeCheck("Cii") ;
	s = XExtra(R0) + 1 ;
	i = strlen(s) ;
	i0 = XInt(R1) ;
	i1 = XInt(R2) ;
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
	if( IsAtom(t0) )
	{
		if( Unify(t1, MakeExtra(XAtomName(t0), textSubTag)) ) JumpNext()
		DoFail()
	}
	elif( IsExtra(t1) && XExtraSubTag(t1) == textSubTag )
	{
		if( UnifyWithAtomic(t0, MakeAtom(XExtra(t1) + 1)) ) JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PConcatText()
{
	if( Unify(X1, MakeExtra(PConcatString(R0), textSubTag)) ) JumpNext()
	DoFail()
}


/* */

void InstallExtraCSysPreds()
{
	InstallCSysPred("text", 1, PText) ;
	InstallCSysPred("gett", 1, PGetText) ;
	InstallCSysPred("putt", 1, PPutText) ;
	InstallCSysPred("slicet", 4, PSliceText) ;
	InstallCSysPred("namet", 2, PNameText) ;
	InstallCSysPred("concatt", 2, PConcatText) ;
}
