/*
 *   This file is part of the CxProlog system

 *   Socket.c
 *   by A.Miguel Dias - 2002/04/11
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


/* CXPROLOG C'BUILTINS */

static void PServerInstall()
{
	OSServerInstall(XTestInt(X0)) ;
	JumpNext()
}

static void PServerWaitConnection()
{
	FILE *r, *w ;
	OSServerWaitConnection(&r, &w) ;
	BindVarWithExtra(X0, FILEToStream(r, mRead)) ;
	BindVarWithExtra(X1, FILEToStream(w, mWrite)) ;
	JumpNext()
}

static void PServerUninstall()
{
	OSServerUninstall() ;
	JumpNext()
}

void SocketInit()
{
	InstallCBuiltinPred("server_install", 1, PServerInstall) ;
	InstallCBuiltinPred("server_wait_connection", 2, PServerWaitConnection) ;
	InstallCBuiltinPred("server_uninstall", 0, PServerUninstall) ;
}
