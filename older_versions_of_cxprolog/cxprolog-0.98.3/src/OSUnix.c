/*
 *   This file is part of the CxProlog system 
 *   OSUnix.c
 *   by A.Miguel Dias - 2001/06/04
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL

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

/* Support __linux__ || __APPLE__ || __FreeBSD__ */


/* INTERRUPT */

void OSAllowInterruptibleSysCalls(void)
{
	siginterrupt(SIGINT, 1) ;
}

void OSDisallowInterruptibleSysCalls(void)
{
	siginterrupt(SIGINT, 0) ;
}

static sigjmp_buf *attentionJB = nil ;

void OSHandleRunInterruptibleFun(void)
{
	if( attentionJB != nil ) {
		sigjmp_buf *b = attentionJB ;	/* Handle eagerly */
		attentionJB = nil ;
		siglongjmp(*b, 1) ;		/* Restore signals automatically */
	}
}

void OSRunInterruptibleFun(Fun fun, FunI InterrHandler, BFun PostInterrHandler)
{
	sigjmp_buf jb ;
	do {
		attentionJB = &jb ;
		if( sigsetjmp(jb, 1) == 0 ) {
			signal(SIGINT, InterrHandler) ;
			fun() ;
			attentionJB = nil ;
		}
	} while( PostInterrHandler() ) ;
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
	return mkdir(FileNameExternalize(fname, false), 0777) == 0 ;
}

Pt OSPropType(Str fname)
{
	struct stat statbuf ;
	if( stat(FileNameExternalize(fname, false), &statbuf) != 0 ) return nil ;
	return statbuf.st_mode & S_IFREG ? tFileAtom
		 : statbuf.st_mode & S_IFDIR ? tDirAtom
		 : MakeAtom("other") ;
}

Pt OSPropSize(Str fname)
{
	struct stat statbuf ;
	if( stat(FileNameExternalize(fname, false), &statbuf) != 0 ) return nil ;
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
	if( stat(FileNameExternalize(fname, false), &statbuf) != 0 ) return nil ;
	t[0] = MakeLLInt(statbuf.st_atime) ;
	t[1] = MakeLLInt(statbuf.st_mtime) ;
	return ArrayToList(t, 2) ;
}

/* @@@@@
static Str OSGetCurrDir(void)
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

Str OSGetCurrDir(void)
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

Pt OSFiles(void)
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

Str OSPrefixDir(void)
{
#ifdef PREFIX
	return PREFIX ;
#else
	return "/usr/local" ;
#endif
}

Str OSApplSubdir(void)
{
	return "share/cxprolog" ;
}

void OSDeleteTree(Str dirPath)
{
	Str comm = GStrFormat("rm -rf %s", dirPath) ;
	OSRun(comm) ;
}

void OSFileSysInit(void)
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

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int OSInstallServer(int port, int queueLen)
{
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
}

void OSAccept(int server, FILE **r, FILE **w)
{
	int sock ;
	if( (sock = accept(server, nil, nil)) < 0 )
		Error("Accept failed") ;
	*r = fdopen(sock, "r") ;
	*w = fdopen(sock, "w") ;
	setbuf(*w, nil) ;	/* output not buffered */
}

void OSUninstallServer(int server)
{
	close(server) ;
}

void OSConnect(Str host, int port, FILE **r, FILE **w)
{
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
}

PInt OSEncodeInt(PInt i)
{
	return htonl(i) ;
}

PInt OSDecodeInt(PInt i)
{
	return ntohl(i) ;
}


/* PROCESSES/THREADS */

#include <sys/wait.h>
#include <sys/syscall.h>

long OSGetPid(void)
{
	return getpid() ;
}

long OSGetPPid(void)
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

void OSBlockSignals(void)
{
#if USE_WXWIDGETS
	sigset_t sigs, oldSigSet ;
	sigfillset(&sigs) ;
	pthread_sigmask(SIG_BLOCK, &sigs, &oldSigSet) ;
#endif
}

long OSFork(void)
{
	int pid ;
	if( (pid = fork()) == -1 )
		Error("fork") ;
	return pid ;
}

void OSWait(void)
{
	wait(nil) ;
}

void OSKill(int pid)
{
	kill(pid, SIGKILL) ;
}

void OSSleep(double seconds)
{
	struct timespec t ;
	t.tv_sec = seconds;
	t.tv_nsec = (seconds - t.tv_sec) * 1000000000L;
	if( nanosleep(&t , nil) < 0 && errno != EINTR ) {
		switch( errno ) {
			case EFAULT: Mesg("EFAULT"); break ;
			case EINTR: Mesg("EINTR"); break ;
			case EINVAL: Mesg("EINVAL"); break ;
			default: Mesg("OTHER"); break ;
		}
		Error("sleep");
	}
}

Bool OSRun(Str command)
{
	StreamFlushAll() ;
	return system(StrExternalize(command)) == 0 ;
}

void OSPipe(int *fd)
{
	if( pipe(fd) == -1 )
		Error("pipe") ;
}

long OSPipeBufferSize(void)
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

Str OSGetEnv(Str envVarName, Bool err)
{
	Str res = StrInternalize(getenv(StrExternalize(envVarName))) ;
	if( res == nil && err )
		Error("Environment variable '%s' does not exist", envVarName) ;
	return res ;
}

void OSSetEnv(Str envVarName, Str newValue, Bool err)
{
	Str newEnv = GStrFormat("%s=%s", envVarName, newValue) ;
	int res = putenv(StrAllocate(StrExternalize(newEnv))) ;
	if( res != 0 && err )
		Error("Could not change environment variable '%s'", envVarName) ;
}

Str OSPathSeparator(void)
{
	return ":" ;
}

#include <pwd.h>

Str OSGetUserPath(Str username, Bool err)
{
	CharPt path = nil ;
	if( username == nil || username[0] == '\0' ) {
		Str envVarName = "HOME" ;
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

static Str OSGetUserSubdirPath(Str subdir, Bool err)
{
	Str home = OSGetUserPath(nil, err) ;
	Str subdirPath = GStrFormat("%s%s%s", home,
						home[strlen(home)-1] == '/' ? "" : "/",
						subdir) ;
	if( !OSExists(subdirPath) )
		OSRun(GStrFormat("mkdir -p \"%s\"", subdirPath)) ;
	return subdirPath ;
}

Str OSGetPreferencesPath(Bool err)
{
	return OSGetUserSubdirPath(".cxprolog", err) ;
}

Str OSGetCachePath(Bool err)
{
	return OSGetUserSubdirPath(".cxprolog/cache", err) ;
}

Str OSGetTmpPath(Bool err)
{
	Unused(err) ;
	return "/tmp" ;
}


/* TIMER */

#include <sys/time.h>

static void dflTimerAction(int signum)
{
	Mesg("TIMER TICK %d", signum);
}

static void ActivateTimer(double period, Fun action, Bool repeat)
{
	if( period <= 0 )
		Error("Timer period must be positive");

	struct sigaction sa ;
	memset(&sa, 0, sizeof(sa));
	sa.sa_handler = action == nil ? dflTimerAction : action;
	sigaction(SIGALRM, &sa, nil);

	struct itimerval timer ;
	timer.it_value.tv_sec = period;
	timer.it_value.tv_usec = (period - timer.it_value.tv_sec) * 1000000;
	timer.it_interval.tv_sec = repeat ? timer.it_value.tv_sec : 0 ;
	timer.it_interval.tv_usec = repeat ? timer.it_value.tv_usec : 0 ;
	setitimer(ITIMER_REAL, &timer, nil);
}


void OSStartTimer(double period, Fun action)
{
	ActivateTimer(period, action, true) ;
}

void OSStopTimer(void)
{
	struct itimerval timer ;
	memset(&timer, 0, sizeof(timer));
	setitimer(ITIMER_REAL, &timer, nil);
}

void OSAlarm(double seconds, Fun action)
{
	ActivateTimer(seconds, action, false) ;
}



/* MISC */

/* WIN
 * #if defined(WIN32)
  MEMORYSTATUSEX status = {0};
  status.dwLength = sizeof(status);
  if (GlobalMemoryStatusEx(&status)) {
    memory_ = status.ullTotalPhys;
  } else {
    LOG_GLE(LS_WARNING) << "GlobalMemoryStatusEx failed.";
    memory_ = -1;
  }
*/

Size OSTotalSystemMemory(void)
{
#ifdef _SC_PHYS_PAGES
/* POSIX */
 	Size pages = sysconf(_SC_PHYS_PAGES);
	Size page_size = sysconf(_SC_PAGE_SIZE);
	return pages * page_size;

#else
/* MACOSX */
	uint64_t mem;
	size_t len = sizeof(mem);
	sysctlbyname("hw.memsize", &mem, &len, NULL, 0);
	return mem;
#endif
}

Str OSName(void)
{
	return "unix" ;
}

Bool CreateConsole(void)
{
	return true ;
}

void DeleteConsole(void)
{
	/* Nothing */
}

void FileInfo(FILE *f) {
	struct stat sb;
	if( fstat(fileno(f), &sb) == -1 ) {
		perror("stat");
		return;
	}
	printf("File type:                ");
	switch (sb.st_mode & S_IFMT) {
		case S_IFBLK:  printf("block device\n"); break;
		case S_IFCHR:  printf("character device\n"); break;
		case S_IFDIR:  printf("directory\n"); break;
		case S_IFIFO:  printf("FIFO/pipe\n"); break;
		case S_IFLNK:  printf("symlink\n"); break;
		case S_IFREG:  printf("regular file\n"); break;
		case S_IFSOCK: printf("socket\n"); break;
		default:       printf("unknown?\n"); break;
	}
	printf("I-node number:            %ld\n", (long)sb.st_ino);
	printf("Mode:                     %lo (octal)\n", (unsigned long)sb.st_mode);
	printf("Link count:               %ld\n", (long)sb.st_nlink);
	printf("Ownership:                UID=%ld   GID=%ld\n", (long) sb.st_uid, (long)sb.st_gid);
	printf("Preferred I/O block size: %ld bytes\n", (long)sb.st_blksize);
	printf("File size:                %lld bytes\n", (long long)sb.st_size);
	printf("Blocks allocated:         %lld\n", (long long)sb.st_blocks);
	printf("Last status change:       %s", ctime(&sb.st_ctime));
	printf("Last file access:         %s", ctime(&sb.st_atime));
	printf("Last file modification:   %s", ctime(&sb.st_mtime));
}

#endif /*  OS_UNIX */

/* cpp -dM OS.h */
