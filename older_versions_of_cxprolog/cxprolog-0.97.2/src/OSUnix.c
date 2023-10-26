/*
 *   This file is part of the CxProlog system

 *   OSUnix.c
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

#if OS_UNIX


/* INTERRUPT */

#include <signal.h>

void AllowInterruptibleSysCalls()
{
#ifndef __MSDOS__
	siginterrupt(SIGINT, 1) ;
#endif
}

void DisallowInterruptibleSysCalls()
{
#ifndef __MSDOS__
	siginterrupt(SIGINT, 0) ;
#endif
}


/* FILESYS */

#include <dirent.h>
#include <fcntl.h>
#include <time.h>
#include <sys/stat.h>

Bool OSExists(CharPt fname)
{
	return access(StrExternalize(fname), F_OK) == 0 ;
}

Bool OSMkdir(CharPt fname)
{
	return mkdir(StrExternalize(fname), 0777) == 0 ;
}

Pt OSPropType(CharPt fname)
{
	struct stat statbuf ;
	if( stat(StrExternalize(fname), &statbuf) != 0 ) return nil ;
	return statbuf.st_mode & S_IFREG ? tFileAtom
		 : statbuf.st_mode & S_IFDIR ? tDirAtom
		 : MakeAtom("other") ;
}

Pt OSPropSize(CharPt fname)
{
	struct stat statbuf ;
	if( stat(StrExternalize(fname), &statbuf) != 0 ) return nil ;
	if( statbuf.st_mode & S_IFDIR )
		return nil ;			/* The size of a directory doens not matter */
	return MakeLLInt(statbuf.st_size) ;
}

Bool OSPropReadable(CharPt fname)
{
	return access(StrExternalize(fname), R_OK) == 0 ;
}

Bool OSPropWritable(CharPt fname)
{
	return access(StrExternalize(fname), W_OK) == 0 ;
}

Pt OSPropTime(CharPt fname)
{
	struct stat statbuf ;
	Pt t[2] ;
	if( stat(StrExternalize(fname), &statbuf) != 0 ) return nil ;
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

Bool OSSetCurrDir(CharPt str)
{
	return chdir(StrExternalize(str)) == 0 ;
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
#ifdef FULL_PREFIX
	return FULL_PREFIX ;
#else
#ifdef PREFIX
	return PREFIX "/share/cxprolog" ;
#else
	return "/usr/local" "/share/cxprolog" ;
#endif
#endif
}

CharPt OSPrefixDir()
{
#ifdef FULL_PREFIX
	return FULL_PREFIX ;
#else
#ifdef PREFIX
	return PREFIX ;
#else
	return "/usr/local" ;
#endif
#endif
}

void OSFileSysInit()
{
}


/* SPECIAL I/O INPUT */

#include <termios.h>

static int fd = -2 ;
static struct termios oldt, newt ;

Bool SetRawInput(FILE *file)
{
	if( fd == -2 ) { /* not initialized yet */
		if( (fd = fileno(file)) < 0
			|| !isatty(fd) || tcgetattr(fd, &oldt) < 0 )
				{ fd = -1 ; return false ; }
		newt = oldt ;
		cfmakeraw(&newt) ;
		newt.c_iflag |= ICRNL ;
		newt.c_oflag = oldt.c_oflag ;
		newt.c_lflag |= ISIG ;
	}
	return fd != -1 && tcsetattr(fd, TCSANOW, &newt) == 0 ;
}

void UnsetRawInput(void)
{
	if( fd != -1 )
		tcsetattr(fd, TCSANOW, &oldt) ;
}

Bool OSIsATty(FILE *file)
{
	return isatty(fileno(file)) == 1 ;
}


/* SOCKETS */

#ifndef __MSDOS__
 #include <sys/types.h>
 #include <sys/socket.h>
 #include <netinet/in.h>
 #include <netdb.h>
#endif

int OSInstallServer(int port, int queueLen)
{
#if __MSDOS__
	Error("Sockets not supported on this OS.") ;
	return 0 ;
#else
	struct hostent *h ;
	struct sockaddr_in sin ;
	int sock ;

	if( (h = gethostbyname("localhost")) == nil )
		Error("Can't get my own host info") ;

	if( (sock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
		Error("Can't create server socket") ;
	memset(&sin, 0, sizeof(sin)) ;
	sin.sin_family = h->h_addrtype ;
	sin.sin_port = htons(port) ;
	if( bind(sock, (struct sockaddr *)&sin, sizeof(sin)) < 0 ) {
		close(sock) ;
		Error("Can't bind to port") ;
	}
	if( listen(sock, queueLen) < 0 ) {
		close(sock) ;
		Error("Can't listen to port") ;
	}
	return sock ;
#endif
}

void OSAccept(int server, FILE **r, FILE **w)
{
#if __MSDOS__
	Error("Sockets not supported on this OS.") ;
#else
	int sock ;
	if( (sock = accept(server, nil, nil)) < 0 )
		Error("Accept failed") ;
	*r = fdopen(sock, "r") ;
	*w = fdopen(sock, "w") ;
	setbuf(*w, nil) ;	/* output not buffered */
#endif
}

void OSUninstallServer(int server)
{
#if __MSDOS__
	Error("Sockets not supported on this OS.") ;
#else
	close(server) ;
#endif
}

void OSConnect(CharPt host, int port, FILE **r, FILE **w)
{
#if __MSDOS__
	Error("Sockets not supported on this OS.") ;
#else
	struct hostent *h ;
	struct sockaddr_in sin ;
	int sock ;

	if( (h = gethostbyname(host)) == nil )
		Error("Unknown host") ;

	if( (sock = socket(h->h_addrtype, SOCK_STREAM, 0)) < 0 )
		Error("Can't create client socket") ;
	memset(&sin, 0, sizeof(sin)) ;
	sin.sin_family = h->h_addrtype ;
	sin.sin_port = htons(port) ;
	if( connect(sock, (struct sockaddr *)&sin, sizeof(sin)) < 0 ) {
		close(sock) ;
		Error("Cannot connect") ;
	}
	*r = fdopen(sock, "r") ;
	*w = fdopen(sock, "w") ;
	setbuf(*w, nil) ;	/* output not buffered */
#endif
}

PInt OSEncodeInt(PInt i)
{
#if __MSDOS__
	return i ;
#else
	return htonl(i) ;
#endif
}

PInt OSDecodeInt(PInt i)
{
#if __MSDOS__
	return i ;
#else
	return ntohl(i) ;
#endif
}


/* PROCESSES/THREADS */

#include <sys/wait.h>
#include <sys/syscall.h>

long OSGetPid()
{
	return getpid() ;
}

long OSGetPPid()
{
	return getppid() ;
}

long OSGetTid()   /* This header is OS neutral so pid_t not used. */
{
#ifdef SYS_gettid
/* CxProlog is not thread-safe and this Linux specific call
   is used only to detect improbable dangerous behaviour
   from external components written, for example, in Java
   ou wxWidgets. */
	return syscall(SYS_gettid) ;
#else
/* It's OK to return -1 if SYS_gettid not available,
   as in FreeBSD, for example. */
	return -1 ;
#endif
}

void OSBlockSignals()
{
#if USE_WXWIDGETS
	sigset_t sigs, oldSigSet ;
	sigfillset(&sigs) ;
	pthread_sigmask(SIG_BLOCK, &sigs, &oldSigSet) ;
#endif
}

long OSFork()
{
	int pid ;
	if( (pid = fork()) == -1 )
		Error("fork") ;
	return pid ;
}

void OSWait()
{
	wait(nil) ;
}

void OSKill(int pid)
{
	kill(pid, SIGKILL) ;
}

void OSSleep(Size secs)
{
	sleep(secs) ;
}

Bool OSRun(CharPt command)
{
	StreamFlushAll() ;
	return system(StrExternalize(command)) == 0 ;
}

void OSPipe(int *fd)
{
	if( pipe(fd) == -1 )
		Error("pipe") ;
}

long OSPipeBufferSize()
{
	return PIPE_BUF ;
}

void OSWrite(int fd, VoidPt buf, Size size)
{
	if( write(fd, buf, size) != size )
		Error("Inter-process communication") ;
}

Bool OSRead(int fd, VoidPt buf, Size size, Bool blocking)
{
	int n ;
	fcntl(fd, F_SETFL, blocking
						? fcntl(fd, F_GETFL) & ~O_NONBLOCK
						: fcntl(fd, F_GETFL) | O_NONBLOCK) ;
	if( (n = read(fd, buf, size)) == -1 && errno == EAGAIN )
		return false ;
	elif( n != size )
		Error("Inter-process communication") ;
	return true ;
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

#include <pwd.h>

CharPt OSGetUserPath(CharPt username, Bool err)
{
	CharPt path = nil ;
	if( username == nil || username[0] == '\0' ) {
		CharPt envVarName = "HOME" ;
		if( (path = getenv(envVarName)) == nil && err )
			Error("Environment variable '%s' does not exist", envVarName) ;
	}
	else {
		struct passwd *puser ;
		if( (puser = getpwnam(StrExternalize(username))) != nil )
			path = puser->pw_dir ;
		elif( err )
			Error("User '%s' does not exist", username) ;
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
	return OSGetUserSubdirPath(".cxprolog", err) ;
}

CharPt OSGetCachePath(Bool err)
{
	return OSGetUserSubdirPath(".cxprolog/cache", err) ;
}

CharPt OSGetTmpPath(Bool err)
{
	return "/tmp" ;
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
	return "unix" ;
}

#endif /*  OS_UNIX */

/* cpp -dM OS.h */
