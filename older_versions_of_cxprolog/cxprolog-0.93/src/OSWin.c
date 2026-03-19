/*
 *   This file is part of the CxProlog system

 *   OSWin.c
 *   by A.Miguel Dias - 2005/07/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

/*
 * As now this file is identical to the file "OSUnknown.c",
 * but this will change soon.
 */

#if OS_WIN

#include <windows.h>
#include <direct.h>
#include <io.h>


/* INTERRUPT */

void AllowInterruptibleSysCalls()
{
}

void DisallowInterruptibleSysCalls()
{
}


/* FILESYS */

Bool OSExists(CharPt fname)
{
	return access(StrExternalize(fname), 0) == 0 ;
}

Pt OSPropType(CharPt fname)
{
	DWORD a = GetFileAttributesA(StrExternalize(fname)) ;
	if( a == INVALID_FILE_ATTRIBUTES ) return nil ;
	return (a & FILE_ATTRIBUTE_DIRECTORY)
				? MakeAtom("dir")
				: MakeAtom("file") ;
}

Pt OSPropSize(CharPt fname)
{
	ULARGE_INTEGER i ;
	WIN32_FILE_ATTRIBUTE_DATA fileData ;
	if( !GetFileAttributesExA(StrExternalize(fname),
					GetFileExInfoStandard, &fileData) )
		return nil ;
	if( fileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
		return nil ;			/* The size of a directory would be 0 */
	i.HighPart = fileData.nFileSizeHigh ;
	i.LowPart = fileData.nFileSizeLow ;
	return MakeLLInt(i.QuadPart) ;
}

Pt OSPropReadable(CharPt fname)
{
	return access(StrExternalize(fname), 0) == 0
				? tTrueAtom
				: tFalseAtom ;
}

Pt OSPropTime(CharPt fname)
{
/* Windows Unit = 100-nanosecond intervals since 00:00:00 UTC, January 1, 1601 */
/* Unix Unit =          1-second intervals since 00:00:00 UTC, January 1, 1970 */
/* All times are converted to the Unix system */

	const ULONGLONG winTimeToUnixTimeOffset = 116444736000000000UL ;
	ULARGE_INTEGER creation, lastAccess, lastWrite ;
	WIN32_FILE_ATTRIBUTE_DATA fileData ;
	Pt t[3] ;
	if( !GetFileAttributesExA(StrExternalize(fname),
					GetFileExInfoStandard, &fileData) )
		return nil ;
	creation.HighPart = fileData.ftCreationTime.dwHighDateTime ;
	creation.LowPart = fileData.ftCreationTime.dwLowDateTime ;
	creation.QuadPart =
		(creation.QuadPart - winTimeToUnixTimeOffset) / 10000000 ;
	if( creation.QuadPart < 0 )
		Error("Creation time to old to encode") ;

	lastAccess.HighPart = fileData.ftLastAccessTime.dwHighDateTime ;
	lastAccess.LowPart = fileData.ftLastAccessTime.dwLowDateTime ;
	lastAccess.QuadPart =
		(lastAccess.QuadPart - winTimeToUnixTimeOffset) / 10000000 ;
	if( lastAccess.QuadPart < 0 )
		Error("Last access time to old to encode") ;

	lastWrite.HighPart = fileData.ftLastWriteTime.dwHighDateTime ;
	lastWrite.LowPart = fileData.ftLastWriteTime.dwLowDateTime ;
	lastWrite.QuadPart =
		(lastWrite.QuadPart - winTimeToUnixTimeOffset) / 10000000 ;
	if( lastWrite.QuadPart < 0 )
		Error("Last write time to old to encode") ;

	t[0] = MakeLLInt(creation.QuadPart) ;
	t[1] = MakeLLInt(lastAccess.QuadPart) ;
	t[2] = MakeLLInt(lastWrite.QuadPart) ;
	return ArrayToList(t + 1, 2) ;
}

CharPt OSGetCurrDir()
{
	Str2K str ;
	if( getcwd(str, 2000) == nil )
		Error("Too long a directory pathname") ;
	return GStrMake(StrInternalize(str)) ;
}

Bool OSSetCurrDir(CharPt str)
{
	return chdir(StrExternalize(str)) == 0 ;
}


Pt OSFiles()
{
	Pt list = tNilAtom ;
	Hdl h = &list + 1 ;
	HANDLE hFind ;
	WIN32_FIND_DATAA fileData ;
	if( (hFind = FindFirstFileA("*", &fileData)) != INVALID_HANDLE_VALUE )
		do {
			Pt t = MakeTempAtom(StrInternalize(fileData.cFileName)) ;
			h[-1] = MakeList(t, tNilAtom) ;
			h = H ;
		} while( FindNextFileA(hFind, &fileData) ) ;
	FindClose(hFind) ;
	return list ;
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

static Bool hasConsole = false ;

Bool CreateConsole() /* based on code from Sergio Lopes */
{
	if( !hasConsole && (hasConsole = AllocConsole()) ) {
		freopen("CONIN$", "r", stdin) ;
		freopen("CONOUT$", "w", stdout) ;
		freopen("CONOUT$", "w", stderr) ;
	}
	return hasConsole ;
}

void DeleteConsole()
{
	if( hasConsole ) {
		fclose(stdin) ;
		fclose(stdout) ;
		fclose(stderr) ;
		FreeConsole() ;
	}
}

CharPt OSName()
{
	return "windows" ;
}

#endif /*  OS_WIN */
