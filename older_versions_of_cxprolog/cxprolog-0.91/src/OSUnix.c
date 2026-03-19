/*
 *   This file is part of the CxProlog system

 *   OSUnix.c
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

#if OS_UNIX

#include <unistd.h>


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

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <time.h>

#define SEP     '/'

static CharPt homePath ;

/* @@@@@
static CharPt GetCurrDir()
{
	static Size size = 0 ;
	if( size == 0 ) {  // init
		size = 10 ;
		currPath = TempAllocate(size) ;	
	}
	while( getcwd(currPath, WordsAsBytes(size)) == nil ) {
		if( errno == ERANGE ) { // needs expansion
			Release(currPath) ; 
			size *= 2 ;
			currPath = TempAllocate(size) ;
		}
		else FileError("Couldn't get current directory") ;
	}
	return currPath ;
}
*/

static CharPt GetCurrDir()
{
	Str2K str ;
	if( getcwd(str, 2000) == nil )
		Error("Too long a directory pathname") ;
	return GStrMake(str) ;
}

static CharPt GetElem(CharPt str)
{
	if( *str == '\0' ) return nil ;
	for( ; *str != SEP && *str != '\0' ; str++ ) ;
	if( *str == SEP ) *str++ = '\0' ;
	return str ;
}

Pt OSPathNameFromStr(CharPt str)
{
	CharPt b, a ;
	Pt list = tNilAtom ;
#if __MSDOS__
    if( str[1] != ':' ) return nil ;
#else
    if( *str++ != SEP ) return nil ;
#endif
	for( a = b = str ; (b = GetElem(a)) != nil ; a = b ) {
		if( *a == '\0' ) return nil ;
		list = MakeList(MakeTempAtom(a), list) ;
	}
	return list ;
}

CharPt OSPathNameToStr(Pt list)
{
	Size len ;
	Hdl h = ListToArray(list, &len) ;
    if( len == 0 ) {
#if __MSDOS__
        return nil ;
#else
		static Str4 sepStr = { SEP, '\0' } ;
        return sepStr ;
#endif
    }
	GStrOpen() ;
#if __MSDOS__
    GStrAddStr(XTestAtomName(h[--len])) ;
#endif	
	while( len-- ) {
		GStrAddChar(SEP) ;
		GStrAddStr(XTestAtomName(h[len])) ;
	}
	return GStrClose() ;
}

Bool OSExists(CharPt fname)
{
	return access(StrExternalize(fname), 0) == 0 ;
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
	struct stat statbuf ;
	if( stat(StrExternalize(fname), &statbuf) != 0 ) return nil ;
	return statbuf.st_mode & S_IFREG ? MakeAtom("file")
		 : statbuf.st_mode & S_IFDIR ? MakeAtom("dir")
		 : MakeAtom("other") ;
}

Pt OSPropSize(CharPt fname)
{
	struct stat statbuf ;
	if( stat(StrExternalize(fname), &statbuf) != 0 ) return nil ;
	return MakeInt(statbuf.st_size) ;
}

Pt OSPropReadable(CharPt fname)
{
	struct stat statbuf ;
	if( stat(StrExternalize(fname), &statbuf) != 0 ) return nil ;
	if( statbuf.st_mode & S_IFDIR ) return tFalseAtom ;
	return access(fname, 0) == 0
				? tTrueAtom
				: tFalseAtom ;
}

Pt OSPropTime(CharPt fname)
{
	struct stat statbuf ;
	Pt t[2] ;
	if( stat(StrExternalize(fname), &statbuf) != 0 ) return nil ;
	t[0] = MakeInt(statbuf.st_atime) ;
	t[1] = MakeInt(statbuf.st_mtime) ;
	return ArrayToList(t, 2) ;
}

Pt OSGetCurrDir()
{
	return OSPathNameFromStr(StrInternalize(GetCurrDir())) ;
}

Bool OSSetCurrDir(Pt t)
{
	return chdir(StrExternalize(OSPathNameToStr(t))) == 0 ;
}

void OSGoHome()
{
	if( chdir(StrExternalize(homePath)) != 0 )
        InternalError("OSGoHome") ;
}

Pt OSFiles()
{
	DIR *dir ;
	struct direct *dp ;
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

void OSFileSysInit()
{
	homePath = StrPerm(GetCurrDir()) ;
}


/* RAW INPUT */

#include <termios.h>

static int fd = -2 ;
static struct termios oldt, newt ;

Bool SetRawInput(StreamPt srm)
{
	if( fd == -2 ) { /* not initialized yet */
		if( (fd = fileno(StreamChannel(srm))) < 0
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
	return 0 ;
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
	return 0 ;
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


/* PROCESSES */

#include <sys/wait.h>

int OSFork()
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

int OSGetPid()
{
	return getpid() ;
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

int OSPipeBufferSize()
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

CharPt OSGetEnv(CharPt envVarName)
{
	return StrInternalize(getenv(StrExternalize(envVarName))) ;
}


/* MISC */

CharPt OSName()
{
	return "unix" ;
}

#endif /*  OS_UNIX */

/* cpp -dM OSUnix.h */
