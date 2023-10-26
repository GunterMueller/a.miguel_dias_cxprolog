/*
 *   This file is part of the CxProlog system

 *   OSWin.c
 *   by A.Miguel Dias - 2005/07/25
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

/*
 * As now this file is identical to the file "OSUnknown.c",
 * but this will change soon.
 */

#if OS_WIN

#include <windows.h>
#include <direct.h>
#include <io.h>
#include <process.h>


/* INTERRUPT */

void AllowInterruptibleSysCalls()
{
}

void DisallowInterruptibleSysCalls()
{
}


/* FILESYS */

#define R_OK	4	/* Test for read permission */
#define W_OK	2	/* Test for write permission */
#define X_OK	1	/* Test for execute permission */
#define F_OK	0	/* Test for existence */

Bool OSExists(CharPt fname)
{
	return _access(StrExternalize(fname), 0) == 0 ;
}

Bool OSMkdir(CharPt fname)
{
	return CreateDirectory(StrExternalize(fname), nil) != 0 ;
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
	return _access(StrExternalize(fname), R_OK) == 0
				? tTrueAtom
				: tFalseAtom ;
}


Pt OSPropWritable(CharPt fname)
{
	return _access(StrExternalize(fname), W_OK) == 0
				? tTrueAtom
				: tFalseAtom ;
}

Pt OSPropTime(CharPt fname)
{
/* Windows Unit = 100-nanosecond intervals since 00:00:00 UTC, January 1, 1601 */
/* Unix Unit = 1-second intervals since 00:00:00 UTC, January 1, 1970 */
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
	return FileNameInternalize(str) ;
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

CharPt OSApplDir()
{
	static CharPt applDir = nil ;
	if( applDir == nil ) {
		Str2K str ;
		CharPt s ;
		switch( GetModuleFileName(nil, str, 2000) ) {
			case 2000: return Error("Too long a application directory pathname") ;
			case 0: return Error("Failed to get the application directory") ;
		}
		for( s = str + strlen(str) ; *s != '\\' && *s != '/' ; s-- ) ;
		*s = '\0' ;
		applDir = StrAllocate(FileNameInternalize(str)) ;
	}
	return applDir ;
}

CharPt OSPrefixDir()
{
	return OSApplDir() ;
}

void OSFileSysInit()
{
	int saveCP = GetConsoleCP() ;
	int saveOutputCP = GetConsoleOutputCP() ;
//	int cp = 1252 ;
	int cp = 28591 ;
//	int cp = 65001 ;
	if( !SetConsoleCP(cp) )
		Error("Couldn't set console code page") ;
	if( !SetConsoleOutputCP(cp) )
		Error("Couldn't set console output code page") ;
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
	return _isatty(fd) != 0 ;
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
	return _getpid() ;
}


long OSGetPPid()
{
	return -1 ;

/*
Adapted from "http://www.codeguru.com/cpp/w-p/win32/article.php/c1437/"

#define DYNLOADED_FPTR( ptrname, procname, dllname)\
FPTR_##procname ptrname = \
( FPTR_##procname ) GetProcAddress ( GetModuleHandle (  _TEXT( #dllname)), #procname);

#define CREATE_DYNFUNC_5( ptrname, procname, dllname, rettype, callconv, a1, a2, a3, a4, a5)\
typedef  rettype (callconv *FPTR_##procname) ( a1, a2, a3, a4, a5);\
DYNLOADED_FPTR( ptrname, procname, dllname);


	DWORD son = _getpid(), res ;
	NTSTATUS ntStatus ;
	HANDLE hProcess ;
	PROCESS_BASIC_INFORMATION pbi ;
	ULONG ulRetLen ;

	CREATE_DYNFUNC_5(NtQueryInformationProcess,
					NtQueryInformationProcess,
					ntdll, NTSTATUS, __stdcall,
					HANDLE, PROCESSINFOCLASS,
					PVOID, ULONG, PULONG
	);
	if( (hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, son)) == -1 )
		return -1 ;
	ntStatus = NtQueryInformationProcess(hProcess,
						ProcessBasicInformation, (void*)&pbi,
						sizeof(PROCESS_BASIC_INFORMATION), &ulRetLen) ;
	res = ntStatus ? pbi.InheritedFromUniqueProcessId : -1 ;
	CloseHandle(hProcess) ;
	return res ;
*/
}

long OSGetTid()
{
	return GetCurrentThreadId() ;
}

void OSBlockSignals()
{
/*	sigset_t sigs, oldSigSet ;
	sigfillset(&sigs) ;
	pthread_sigmask(SIG_BLOCK, &sigs, &oldSigSet) ;	*/
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
	return ";" ;
}

CharPt OSGetUserPath(CharPt username, Bool err)
{
	CharPt path ;
	if( username == nil ) {
		CharPt envVarName = "USERPROFILE" ;
		if( (path = getenv(envVarName)) == nil && err )
			Error("Environment variable '%s' does not exist", envVarName) ;
	}
	else {
		path = nil ;
	}
	return FileNameInternalize(path) ;
}

CharPt OSGetCachePath(Bool err)
{
	CharPt home = OSGetUserPath(nil, err) ;
	CharPt temp = "Temp/CxProlog/Cache" ;
	CharPt cachePath = GStrFormat("%s%s%s", home,
						home[strlen(home)-1] == '/' ? "" : "/",
						temp) ;
	if( !OSExists(cachePath) )
		OSRun(GStrFormat("mkdir \"%s\"", cachePath)) ;
	return cachePath ;
}

CharPt OSGetTmpPath(Bool err)
{	
	CharPt home = OSGetUserPath(nil, err) ;
	CharPt temp = "Temp/CxProlog" ;
	CharPt tmpPath = GStrFormat("%s%s%s", home,
						home[strlen(home)-1] == '/' ? "" : "/",
						temp) ;
	if( !OSExists(tmpPath) )
		OSRun(GStrFormat("mkdir \"%s\"", tmpPath)) ;
	return tmpPath ;
}


/* MISC */

#include <conio.h>

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
		printf("Press any key to continue") ;
		_getch() ;
		fclose(stdin) ;
		fclose(stdout) ;
		fclose(stderr) ;
		FreeConsole() ;
	}
}

#if !USE_WXWIDGETS
int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR lpszCmd, int nCmd)
{
	if( CreateConsole() ) {
		int res = main(1, &lpszCmd) ;
		DeleteConsole() ;
		return res ;
	}
	return -1 ;
}
#endif

CharPt OSName()
{
	return "windows" ;
}

#endif /*  OS_WIN */
