/*
 *   This file is part of the NanoProlog system

 *   SystemMore.c
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

#include "NanoProlog.h"

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
static CharPt str ;
static ClausePt cl ;
static PredicatePt pr ;
static AtomPt at ;



/* INSTALL HERE YOUR OWN C-SYSTEM PREDICATES */

#if 0

#include "XAppl.h"
#include "YServer.h"

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
	XSend(PStringToString(R0, cCharPt(codeBuff), maxCodePerClause Wd)) ;
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
	TypeCheck("?ai") ;
	XReceiveP(cCharPt(codeBuff), maxCodePerClause Wd,
					XAtomName(R1), XInt(R0)) ;	
	if( Unify(X0, StringToPString(cCharPt(codeBuff))) ) JumpNext()
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
	if( Unify(X0, StringToPString(cCharPt(codeBuff))) ) JumpNext()
	DoFail()
}

static void PYReplyToRequest()
{
	CharPt s = PStringToString(R0, cCharPt(codeBuff), maxCodePerClause Wd) ;
	
	if( *s != '\0' ) YReplyToRequest(s) ;
	YDoneRequest() ;
	JumpNext()
}

#endif

/* */

void InstallMoreCSysPreds()
{
#if 0
	InstallCSysPred("net", 0, PNet) ;

	InstallCSysPred("xappl_launch", 1, PXLaunch) ;
	InstallCSysPred("xappl_close", 0, PXClose) ;
	InstallCSysPred("xappl_send", 1, PXSend) ;
	InstallCSysPred("xappl_receive", 1, PXReceive) ;
	InstallCSysPred("xappl_receive", 3, PXReceiveP) ;
	InstallCSysPred("xappl_set_prompt", 1, PXSetPrompt) ;
	InstallCSysPred("xappl_set_timeout", 1, PXSetTimeOut) ;

	InstallCSysPred("ysocket_install", 1, PYInstallSocket) ;
	InstallCSysPred("ysocket_get", 1, PYGetRequest) ;
	InstallCSysPred("ysocket_reply", 1, PYReplyToRequest) ;
#endif
}
