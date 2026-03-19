/*
 *   This file is part of the CxProlog system

 *   BuiltinsMore.c
 *   by A.Miguel Dias - 1997/05/28
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

/* The functions that define the C PREDs should not have local variables */

static Bool b, b0, b1 ;
static Int i, i0, i1 ;
static int n, n0, n1 ;
static Real r, r0, r1 ;
static Pt t0, t1, t2, t3 ;
static Hdl hh, h0, h1 ;
static CharPt str ;
static ClausePt cl ;
static PredicatePt pr ;
static AtomPt at ;


/* INSTALL HERE YOUR OWN C-SYSTEM PREDICATES */

#undef unix
#if unix

#include "XAppl.h"
#include "YServer.h"

static void PNet()	/* flag stating that the NET predicates are installed */
{
	JumpNext()
}


/* XAPPLICATION */

static void PXLaunch()
{
	if( XLaunch(XTestAtomName(X0)) ) JumpNext()
	DoFail()
}

static void PXClose()
{
	XClose() ;
	JumpNext()
}

static void PXSend()
{
	XSend(PStringToString(X0, cCharPt(codeBuff), maxCodePerClause Wd)) ;
	JumpNext()
}

static void PXReceive()
{
	XReceive(cCharPt(codeBuff), maxCodePerClause Wd) ;	
	if( Unify(X0, StringToPString(cCharPt(codeBuff))) ) JumpNext()
	DoFail()
}

static void PXReceiveP()
{
	XReceiveP(cCharPt(codeBuff), maxCodePerClause Wd,
					XTestAtomName(X1), XTestInt(X2)) ;	
	if( Unify(X0, StringToPString(cCharPt(codeBuff))) ) JumpNext()
	DoFail()
}

static void PXSetPrompt()
{
	XSetPrompt(XTestAtomName(X0)) ;
	JumpNext()
}

static void PXSetTimeOut()
{
	XSetTimeOut(XTestInt(X0)) ;
	JumpNext()
}


/* YSERVER */

static void PYInstallSocket()
{
	YInstallSocket(XTestInt(X0)) ;
	JumpNext()
}

static void PYGetRequest()
{
	YGetRequest(cCharPt(codeBuff), maxCodePerClause Wd) ;	
	if( Unify(X0, StringToPString(cCharPt(codeBuff))) ) JumpNext()
	DoFail()
}

static void PYReplyToRequest()
{
	CharPt s = PStringToString(X0, cCharPt(codeBuff), maxCodePerClause Wd) ;
	
	if( *s != '\0' ) YReplyToRequest(s) ;
	YDoneRequest() ;
	JumpNext()
}

#endif

/* */

void InstallMoreCBuiltinPreds()
{
#if unix
	InstallCBuiltinPred("net", 0, PNet) ;

	InstallCBuiltinPred("xappl_launch", 1, PXLaunch) ;
	InstallCBuiltinPred("xappl_close", 0, PXClose) ;
	InstallCBuiltinPred("xappl_send", 1, PXSend) ;
	InstallCBuiltinPred("xappl_receive", 1, PXReceive) ;
	InstallCBuiltinPred("xappl_receive", 3, PXReceiveP) ;
	InstallCBuiltinPred("xappl_set_prompt", 1, PXSetPrompt) ;
	InstallCBuiltinPred("xappl_set_timeout", 1, PXSetTimeOut) ;

	InstallCBuiltinPred("ysocket_install", 1, PYInstallSocket) ;
	InstallCBuiltinPred("ysocket_get", 1, PYGetRequest) ;
	InstallCBuiltinPred("ysocket_reply", 1, PYReplyToRequest) ;
#endif
}
