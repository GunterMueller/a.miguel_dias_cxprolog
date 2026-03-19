/*
 *   This file is part of the NanoProlog system

 *   MoreSystem.c
 *   by A.Miguel Dias - 97/05/28
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990,...,1997 A.Miguel Dias, GLOC, DI/FCT/UNL

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

 931117: release of version 0.5

*/

/* INSTALL HERE YOUR OWN SYSTEM PREDICATES */

#include "NanoProlog.h"
#include "PlStrings.h"
#include "XAppl.h"
#include "YServer.h"


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

static Bool b, b0, b1 ;
static Int i, i0, i1 ;
static int n, n0, n1 ;
static Real r, r0, r1 ;
static Pt t0, t1, t2, t3 ;
static Hdl hh, h0, h1 ;
static char *str ;
static Clause *cl ;
static Predicate *pr ;
static Atom *at ;

static void TypeError(char *s)
{
	Error("Type Error: %s expected", s) ;
}

static void TypeCheck(char *types)
{
	Hdl a ;

	for( a = X ; *types != '\0' ; a++ )
	{
		switch(	*types++ )
		{
			case 'a':
					if( not IsAtom(Drf(*a)) ) TypeError("atom") ;
					break ;
			case 'i':
					if( not IsInt(Drf(*a)) ) TypeError("integer") ;
					break ;
			case 'p':
					if( not IsPos(Drf(*a)) ) TypeError("positive integer") ;
					break ;
			case 'n':
					if( not IsNumber(Drf(*a)) ) TypeError("number") ;
					break ;
			case 't':
					if( not IsAtomic(Drf(*a)) ) TypeError("atomic term") ;
					break ;
 			case 'v':
					if( not IsVar(Drf(*a)) ) TypeError("variable") ;
					break ;
			case 's':
					if( not IsStruct(Drf(*a)) ) TypeError("structure") ;
					break ;
			case 'l':
					if( not IsList(Drf(*a)) || Drf(*a) != TagAtom(nilAtom) ) TypeError("list") ;
					break ;
			case '?':
					break ;
			default: Default("TypeCheck") ;
		}
	}
}


static void PNet()	/* flag stating that the NET predicates are installed */
{
	JumpNext()
}


/* XAPPLICATION */

static void PXLaunch()
{
	TypeCheck("a") ;
	if( XLaunch(XAtomName(R0)) ) JumpNext()
	DoFail()
}

static void PXClose()
{
	XClose() ;
	JumpNext()
}

static void PXSend()
{
	XSend(StringToBuffer(R0, cCharPt(codeBuff), maxCodePerClause Wd)) ;
	JumpNext()
}

static void PXReceive()
{
	XReceive(cCharPt(codeBuff), maxCodePerClause Wd) ;	
	if( Unify(X0, BufferToString(cCharPt(codeBuff))) ) JumpNext()
	DoFail()
}

static void PXReceiveP()
{
	TypeCheck("?ai") ;
	XReceiveP(cCharPt(codeBuff), maxCodePerClause Wd,
					XAtomName(R1), XInt(R0)) ;	
	if( Unify(X0, BufferToString(cCharPt(codeBuff))) ) JumpNext()
	DoFail()
}

static void PXSetPrompt()
{
	TypeCheck("a") ;
	XSetPrompt(XAtomName(R0)) ;
	JumpNext()
}

static void PXSetTimeOut()
{
	TypeCheck("i") ;
	XSetTimeOut(XInt(R0)) ;
	JumpNext()
}


/* YSERVER */

static void PYInstallSocket()
{
	TypeCheck("i") ;
	YInstallSocket(XInt(R0)) ;
	JumpNext()
}

static void PYGetRequest()
{
	YGetRequest(cCharPt(codeBuff), maxCodePerClause Wd) ;	
	if( Unify(X0, BufferToString(cCharPt(codeBuff))) ) JumpNext()
	DoFail()
}

static void PYReplyToRequest()
{
	char *s = StringToBuffer(R0, cCharPt(codeBuff), maxCodePerClause Wd) ;
	
	if( *s != '\0' ) YReplyToRequest(s) ;
	YDoneRequest() ;
	JumpNext()
}


/* STRINGS */

static void PStrRead()
{
	if( Unify(X0, StrRead()) ) JumpNext()
	DoFail()
}

static void PStrWrite()
{
	StrWrite(R0) ;
	JumpNext()
}


/* PREDICATES */

static Pt StackAddr()
{
	int x ;
	
	return( cPt(&x) ) ;
}

static void PSys() /* Just a test */
{
	printf("STACK = %lx\n", StackAddr()) ;
	printf("&trailBegin = %lx\n", &trailBegin) ;
	printf("&h = %lx\n", &hh) ;
	printf("&CP = %lx\n", &CP) ;
	JumpNext()
}


/* */

void InstallMoreCPredicates()
{
	InstallCPredicate("net", 0, PNet) ;

	InstallCPredicate("xappl_launch", 1, PXLaunch) ;
	InstallCPredicate("xappl_close", 0, PXClose) ;
	InstallCPredicate("xappl_send", 1, PXSend) ;
	InstallCPredicate("xappl_receive", 1, PXReceive) ;
	InstallCPredicate("xappl_receive", 3, PXReceiveP) ;
	InstallCPredicate("xappl_set_prompt", 1, PXSetPrompt) ;
	InstallCPredicate("xappl_set_timeout", 1, PXSetTimeOut) ;

	InstallCPredicate("ysocket_install", 1, PYInstallSocket) ;
	InstallCPredicate("ysocket_get", 1, PYGetRequest) ;
	InstallCPredicate("ysocket_reply", 1, PYReplyToRequest) ;

	InstallCPredicate("str_read", 1, PStrRead) ;
	InstallCPredicate("str_write", 1, PStrWrite) ;

	InstallCPredicate("sys", 0, PSys) ;
}
