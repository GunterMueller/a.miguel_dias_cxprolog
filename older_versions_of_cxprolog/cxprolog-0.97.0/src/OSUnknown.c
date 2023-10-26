/*
 *   This file is part of the CxProlog system

 *   OSUnknown.c
 *   by A.Miguel Dias - 2001/06/04
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL

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

void AllowInterruptibleSysCalls()
{
}

void DisallowInterruptibleSysCalls()
{
}


/* FILESYS */

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

Pt OSPropType(CharPt fname)
{
	return OSExists(fname) ? tNilAtom : nil ;
}

Pt OSPropSize(CharPt fname)
{
	return minusOneIntPt ;
}

Pt OSPropReadable(CharPt fname)
{
	return OSExists(fname) ? tTrueAtom : tFalseAtom ;
}

Pt OSPropTime(CharPt fname)
{
	Pt t[2] ;
	t[0] = zeroIntPt ;
	t[1] = zeroIntPt ;
	return ArrayToList(t, 2) ;
}

CharPt OSGetCurrDir()
{
	return "" ;
}

Bool OSSetCurrDir(CharPt s)
{
	return true ;
}

Pt OSFiles()
{
	return tNilAtom ;
}

CharPt OSApplDir()
{
	return nil ;
}

void OSFileSysInit()
{
}


/* SPECIAL I/O INPUT */

Bool SetRawInput(FILE *file)
{
	return false ;
}

void UnsetRawInput()
{
}

Bool OSIsATty(int fd)
{
	return fd <= 2 ; /* STDERR_FILENO */
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


/* PROCESSES/THREADS */

long OSGetPid()
{
	Error("Processes not supported on this OS.") ;
	return 0 ;
}

long OSGetPPid()
{
	Error("Processes not supported on this OS.") ;
	return 0 ;
}

long OSGetTid()
{
	Error("Threads not supported on this OS.") ;
	return 0 ;
}

long OSFork()
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

long OSPipeBufferSize()
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

CharPt OSGetEnv(CharPt envVarName, Bool err)
{
	CharPt res = StrInternalize(getenv(StrExternalize(envVarName))) ;
	if( res == nil && err )
		Error("Environment variable '%s' does not exist", envVarName) ;
	return res ;
}

void OSSetEnv(CharPt envVarName, CharPt newValue, Bool err)
{
	CharPt newEnv = GStrFormat("%s=%s", envVarName, newValue) ;
	int res = putenv(StrAllocate(StrExternalize(newEnv))) ;
	if( res != 0 && err )
		Error("Could not change environment variable '%s'", envVarName) ;
}

CharPt OSPathSeparator()
{
	return ":" ;
}

CharPt OSGetUserHome(CharPt username, Bool err)
{
	Error("Finding an user home directory is not supported on this OS.") ;
	return nil ;
}


/* MISC */

Bool CreateConsole()
{
	return true ;
}

void DeleteConsole()
{
	/* Nothing */
}

CharPt OSName()
{
	return "unknown" ;
}

#endif /* OS_UNKNOWN */
