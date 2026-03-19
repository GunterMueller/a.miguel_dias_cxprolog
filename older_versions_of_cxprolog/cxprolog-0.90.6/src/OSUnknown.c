/*
 *   This file is part of the CxProlog system

 *   OSUnknown.c
 *   by A.Miguel Dias - 2001/06/04
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

#if OS_UNKNOWN

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

void AllowInterruptibleSysCalls()
{
}

void DisallowInterruptibleSysCalls()
{
}


/* FILESYS */

Pt OSPathNameFromStr(CharPt str)
{
	return tNilAtom ;
}

CharPt OSPathNameToStr(Pt list)
{
	return "" ;
}

/* OSExists: Also requires that the file is readable which is
    too big a requirement. However this is the best we can
    do using the stdio lib alone */ 
Bool OSExists(CharPt fname)
{
	FILE *f;
	if( (f = fopen(StrExternalize(fname), "r")) != nil ) {
		fclose(f) ;
		return true ;
	}
	return false ;
}

Bool OSRen(CharPt oname, CharPt nname)
{
	return rename(StrExternalize(oname), StrExternalize(nname)) == 0 ;
}

Bool OSDel(CharPt fname)
{
	return remove(StrExternalize(fname)) == 0 ;
}

Pt OSPropType(CharPt fname)
{
	return OSExists(fname) ? tNilAtom : nil ;
}

Pt OSPropSize(CharPt fname)
{
	return MakeInt(-1) ;
}

Pt OSPropReadable(CharPt fname)
{
	return OSExists(fname) ? tTrueAtom : tFalseAtom ;
}

Pt OSPropTime(CharPt fname)
{
	Pt t[2] ;
	t[0] = MakeInt(0) ;
	t[1] = MakeInt(0) ;
	return ArrayToList(t, 2) ;
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

void OSFileSysInit()
{
}

/* RAW INPUT */

Bool SetRawInput(StreamPt srm)
{
	return false ;
}

void UnsetRawInput()
{
}

/* SOCKETS */

int OSInstallServer(int port, int queueLen)
{
	Error("Sockets not supported on this OS.") ;
	return 0 ;
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

int OSFork()
{
	Error("Processes not supported on this OS.") ;
	return 0 ;
}

void OSWait()
{
	Error("Processes not supported on this OS.") ;
}

void OSKill(int pid)
{
	Error("Processes not supported on this OS.") ;
}

int OSGetPid()
{
	Error("Processes not supported on this OS.") ;
	return 0 ;
}

void OSSleep(Size secs)
{
	Error("Processes not supported on this OS.") ;
}

Bool OSRun(CharPt command)
{
	StreamFlushAll() ;
	return system(StrExternalize(command)) == 0 ;
}

void OSPipe(int *fd)
{
	Error("Processes not supported on this OS.") ;
}

int OSPipeBufferSize()
{
	Error("Processes not supported on this OS.") ;
	return 0 ;
}

void OSWrite(int fd, VoidPt buf, Size size)
{
	Error("Processes not supported on this OS.") ;
}

Bool OSRead(int fd, VoidPt buf, Size size, Bool blocking)
{
	Error("Processes not supported on this OS.") ;
	return false ;
}

CharPt OSGetEnv(CharPt envVarName)
{
	return StrInternalize(getenv(StrExternalize(envVarName))) ;
}


/* MISC */

CharPt OSName()
{
	return "unknown" ;
}

#endif /* OS_UNKNOWN */
