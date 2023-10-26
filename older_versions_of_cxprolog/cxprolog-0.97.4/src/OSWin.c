/*
 *   This file is part of the CxProlog system

 *   OSWin.c
 *   by A.Miguel Dias - 2009/03/09
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2009 A.Miguel Dias, CITI, DI/FCT/UNL

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

#if OS_WIN

/* Support __MINGW32__ */


#include <windows.h>

/* INTERRUPT */

#include <signal.h>
#include <setjmp.h>

void OSAllowInterruptibleSysCalls()
{
}

void OSDisallowInterruptibleSysCalls()
{
}

void OSHandleRunInterruptibleFun(void)
{
	/* As now, not actually used in the case of Windows */
}

void OSRunInterruptibleFun(Fun fun, FunI InterrHandler, BFun PostInterrHandler)
{
	/* As now, not actually used in the case of Windows */
}


/* FILESYS */

#include <dirent.h>
#include <fcntl.h>
#include <time.h>
#include <sys/stat.h>

Bool OSExists(Str fname)
{
	return access(FileNameExternalize(fname, false), F_OK) == 0 ;
}

Bool OSMkdir(Str fname)
{
	return mkdir(FileNameExternalize(fname, false)) == 0 ;
}

Pt OSPropType(Str fname)
{
	struct stat statbuf ;
	if( stat(FileNameExternalize(fname, true), &statbuf) != 0 ) return nil ;
	return statbuf.st_mode & S_IFREG ? tFileAtom
		 : statbuf.st_mode & S_IFDIR ? tDirAtom
		 : MakeAtom("other") ;
}

Pt OSPropSize(Str fname)
{
	struct stat statbuf ;
	if( stat(FileNameExternalize(fname, true), &statbuf) != 0 ) return nil ;
	if( statbuf.st_mode & S_IFDIR )
		return nil ;			/* The size of a directory doens not matter */
	return MakeLLInt(statbuf.st_size) ;
}

Bool OSPropReadable(Str fname)
{
	return access(FileNameExternalize(fname, false), R_OK) == 0 ;
}

Bool OSPropWritable(Str fname)
{
	return access(FileNameExternalize(fname, false), W_OK) == 0 ;
}

Pt OSPropTime(Str fname)
{
	struct stat statbuf ;
	Pt t[2] ;
	if( stat(FileNameExternalize(fname, true), &statbuf) != 0 ) return nil ;
	t[0] = MakeLLInt(statbuf.st_atime) ;
	t[1] = MakeLLInt(statbuf.st_mtime) ;
	return ArrayToList(t, 2) ;
}

/* @@@@@
static CharPt OSGetCurrDir()
{
	static Size size = 0 ;
	if( size == 0 ) {  // init
		size = 10 ;
		currPath = Allocate(size, true) ;
	}
	while( getcwd(currPath, WordsAsBytes(size)) == nil ) {
		if( errno == ERANGE ) { // needs expansion
			Release(currPath, ?) ;
			size *= 2 ;
			currPath = Allocate(size, true) ;
		}
		else FileError("Couldn't get current directory") ;
	}
	return currPath ;
}
*/

CharPt OSGetCurrDir()
{
	Str2K str ;
	if( getcwd(str, 2000) == nil ) {
		if( errno == ERANGE )
			Error("Too long a directory pathname") ;
		else
			Error("Current directory not defined because it has been deleted") ;
	}
	return FileNameInternalize(str) ;
}

Bool OSSetCurrDir(Str str)
{
	return chdir(FileNameExternalize(str, false)) == 0 ;
}

Pt OSFiles()
{
	DIR *dir ;
	struct dirent *dp ;
	Pt list = tNilAtom ;
	Hdl h = &list + 1 ;
	if( (dir = opendir(".")) == nil ) return nil ;
	while( (dp = readdir(dir)) != nil ) {
		h[-1] = MakeList(MakeTempAtom(StrInternalize(dp->d_name)), tNilAtom) ;
		h = H ;
	}
	closedir(dir) ;
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
	if( OSIsATty(stdout) ) {
	//	int cp = 1252 ;
		int cp = 28591 ;
	//	int cp = 65001 ;
	//	Mesg("%d %d", GetConsoleCP(), GetConsoleOutputCP()) ;
		if( !SetConsoleCP(cp) )
			Error("Couldn't set console code page") ;
		if( !SetConsoleOutputCP(cp) )
			Error("Couldn't set console output code page") ;	
	//	Mesg("%d %d", GetConsoleCP(), GetConsoleOutputCP()) ;
	}
}


/* SPECIAL I/O INPUT */

Bool SetRawInput(FILE *file)
{
	return false ;
}

void UnsetRawInput()
{
}

Bool OSIsATty(FILE *file)
{
	return _isatty(_fileno(file)) != 0 ;
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

void OSConnect(Str host, int port, FILE **r, FILE **w)
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
}

long OSGetTid()   /* This header is OS neutral so pid_t not used. */
{
	return GetCurrentThreadId() ;
}

void OSBlockSignals()
{
	/*
#if USE_WXWIDGETS
	sigset_t sigs, oldSigSet ;
	sigfillset(&sigs) ;
	pthread_sigmask(SIG_BLOCK, &sigs, &oldSigSet) ;
#endif
	*/
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
	Sleep(1000 * secs) ;  /* Windows sleep */
}

Bool OSRun(Str command)
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

CharPt OSGetEnv(Str envVarName, Bool err)
{
	CharPt res = StrInternalize(getenv(StrExternalize(envVarName))) ;
	if( res == nil && err )
		Error("Environment variable '%s' does not exist", envVarName) ;
	return res ;
}

void OSSetEnv(Str envVarName, Str newValue, Bool err)
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

CharPt OSGetUserPath(Str username, Bool err)
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

static CharPt OSGetUserSubdirPath(CharPt subdir, Bool err)
{
	CharPt home = OSGetUserPath(nil, err) ;
	CharPt subdirPath = GStrFormat("%s%s%s", home,
						home[strlen(home)-1] == '/' ? "" : "/",
						subdir) ;
	if( !OSExists(subdirPath) )
		OSRun(GStrFormat("mkdir -p \"%s\"", subdirPath)) ;
	return subdirPath ;
}

CharPt OSGetPreferencesPath(Bool err)
{
	return OSGetUserSubdirPath("Temp/CxProlog", err) ;
}

CharPt OSGetCachePath(Bool err)
{
	return OSGetUserSubdirPath("Temp/CxProlog/Cache", err) ;
}

CharPt OSGetTmpPath(Bool err)
{
	return OSGetUserSubdirPath("Temp/CxProlog/Temp", err) ;
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
#if 0
		WriteStd("Press any key to continue") ;
		_getch() ;
#endif
		fclose(stdin) ;
		fclose(stdout) ;
		fclose(stderr) ;
		FreeConsole() ;
	}
	else {
#if 1
		if( NormalFinish() == true3 ) {
			WriteStd("Press any key to continue") ;
			_getch() ;
		}
		else {
			WriteStd("\nClosing...") ;
			Sleep(400) ;
		}
#endif
	}

}

CharPt OSName()
{
	return "win32" ;
}

#endif /*  OS_WIN */

/* cpp -dM OS.h */
