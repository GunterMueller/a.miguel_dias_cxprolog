/*
 *   This file is part of the CxProlog system

 *   Net.c
 *   by A.Miguel Dias - 2002/04/11
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define NotInUse	-2
#define InUse(x)	( (x) != NotInUse )

static int server = NotInUse ;


/* CXPROLOG C'BUILTINS */


static void PNetInstall()
{
	if( InUse(server) )
		Error("The CxProlog server is already active") ;
	server = OSInstallServer(XTestInt(X0), 5) ;
	JumpNext()
}

static void PNetUninstall()
{
	if( !InUse(server) )
		Error("The CxProlog server is not active") ;
	OSUninstallServer(server) ;
	server = NotInUse ;
	JumpNext()
}

static void PNetAccept()
{
	FILE *r, *w ;
	if( !InUse(server) )
		Error("The CxProlog server is not active") ;
	OSAccept(server, &r, &w) ;
	BindVarWithExtra(X0, FILEToStream(r, mRead, "_%ssocket")) ;
	BindVarWithExtra(X1, FILEToStream(w, mWrite, "_%ssocket")) ;
	JumpNext()
}

static void PNetConnect()
{
	FILE *r, *w ;
	OSConnect(XTestAtomName(X0), XTestInt(X1), &r, &w) ;
	BindVarWithExtra(X2, FILEToStream(r, mRead, "_%csocket")) ;
	BindVarWithExtra(X3, FILEToStream(w, mWrite, "_%csocket")) ;
	JumpNext()
}

static void PNetPutInt()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	PInt i = OSEncodeInt(XTestInt(X1)) ;
	StreamWriteN(srm, &i, sizeof(i)) ;
	JumpNext()
}

static void PNetGetInt()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	PInt i ;
	StreamReadN(srm, &i, sizeof(i)) ;
	if( Unify(X1, MakeInt(OSDecodeInt(i))) )
		JumpNext()
	DoFail()
}

void NetInit()
{
	InstallCBuiltinPred("net_install", 1, PNetInstall) ;
	InstallCBuiltinPred("net_uninstall", 0, PNetUninstall) ;
	InstallCBuiltinPred("net_accept", 2, PNetAccept) ;
	InstallCBuiltinPred("net_connect", 4, PNetConnect) ;
	InstallCBuiltinPred("net_put_int", 2, PNetPutInt) ;
	InstallCBuiltinPred("net_get_int", 2, PNetGetInt) ;
}
