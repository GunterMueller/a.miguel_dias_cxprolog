/*
 *   This file is part of the CxProlog system

 *   OSUnknown.c
 *   by A.Miguel Dias - 2001/06/04
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

#ifdef unknownOS

/* INTERRUPT */

#include <signal.h>

static void IHandler(int a)
{
	signal(SIGINT, SIG_IGN) ;
	Interrupted() = true ;
	Attention() = true ;
	signal(SIGINT, IHandler) ;
}

void InterruptOff()
{
	signal(SIGINT, SIG_IGN) ;
}

void InterruptOn()
{
	signal(SIGINT, IHandler) ;
}


/* FILESYS */

/* OSExists: Also requires that the file is readable, which is
    too big a requirement. However, this is the best that we can
    do using "stdio.h" alone */ 
Bool OSExists(CharPt fname)
{
	FILE *f;
	if( (f = fopen(fname, "r")) != nil ) {
		fclose(f) ;
		return true ;
	}
	return false ;
}

Bool OSRen(CharPt oname, CharPt nname)
{
	return rename(oname, nname) == 0 ;
}

Bool OSDel(CharPt fname)
{
	return remove(fname) == 0 ;
}

Pt OSPropType(CharPt fname)
{
	return OSExists(fname) ? tNilAtom : nil ;
}

Pt OSPropReadable(CharPt fname)
{
	return OSExists(fname) ? tTrueAtom : tFalseAtom ;
}

Pt OSGetCurrDir()
{
	return tNilAtom ;
}

Bool OSSetCurrDir(Pt t)
{
	return true ;
}

void OSGoHome()
{
}

Pt OSFiles()
{
	return tNilAtom ;
}

void OSFileSys()
{
}


/* SOCKETS */

int OSInstallServer(int port, int queueLen)
{
	Error("Sockets not supported on this OS.") ;
}
	
void OSAccept(int server, FILE **r, FILE **w)
{
	Error("Sockets not supported on this OS.") ;
}

void OSUninstallServer(int server)
{
	Error("Sockets not supported on this OS.") ;
}

void OSConnect(CharPt host, int port, FILE **r, FILE **w)
{
	Error("Sockets not supported on this OS.") ;
}

PInt OSEncodeInt(PInt i)
{
	return i ;
}

PInt OSDecodeInt(PInt i)
{
	return i ;
}


/* PROCESSES */

CharPt OSName()
{
	return "unknown" ;
}

Bool OSRun(CharPt fname)
{
	return false ;
}


#endif
