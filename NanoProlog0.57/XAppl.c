/*
 *   This file is part of the NanoProlog system

 *   XAppl.c
 *   by A.Miguel Dias - 97/05/10
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990,...,1997 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:
 970510: first version
*/

#include "NanoProlog.h"
#include "XAppl.h"
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

#define maxPromptsize	256

static char xPrompt[maxPromptsize+1] ;
static int xPromptSize ;
static int in[2], out[2];
static Bool inUse = false ;
static jmp_buf timeOut, childControl ;
static int waitForTimeOut ;

static void Close(int fd)
{
	if( close(fd) == -1 )
		SysErrorN("Closing file descriptor") ;
}

static void DupIn(int fd, int entry)
{
	Close(entry) ;
	if( dup(fd) == -1 )
		SysErrorN("Duplicating file descriptor") ;
}

static void EnterInUse(void)
{
	if( inUse )
		Error("There is already one Xapplication active") ;
	inUse = true ;
	XSetPrompt("\n") ;
	XSetTimeOut(3) ;
}

static void CheckInUse()
{
	if( not inUse )
		Error("There is no xApplication activated") ;
}

void XSetTimeOut(int to)
{
	if( to < 2 ) to = 1 ;
	if( to > 60 ) to = 60 ;
	waitForTimeOut = to ;
}

void XSetPrompt(CharPt pr)
{
	int pSize = strlen(pr) ;
	
	if( pSize > maxPromptsize )
		Error("xPrompt too long") ;
	strcpy(xPrompt, pr) ;
	xPromptSize = pSize ;
}

void XSend(CharPt str)
{
	CheckInUse() ;
	if( write(out[1], str, strlen(str)) == -1 /*??*/ )
		SysErrorN("Unable to send data to xApplication") ;
}

void HandleSIGALRM()
{
	longjmp(timeOut, 1) ;
}

void XReceive(CharPt buf, int bufSize)
{
	int s, size = 0 ;

	CheckInUse() ;
	bufSize -= 3 ;

	alarm(0) ; signal(SIGALRM, HandleSIGALRM) ;
	switch( setjmp(timeOut) )
	{
		case 0:
		{
			do {
				alarm(waitForTimeOut) ;
				if( (s = read(in[0], buf + size, bufSize - size)) <= 0 )
					SysErrorN("Unable to receive data from xApplication") ;
				buf[size += s] = '\0' ;
				if( size == bufSize )
					Error("xbuffer overflow") ;
			}
			while( strcmp(xPrompt, buf + size - xPromptSize) != 0 ) ;
			alarm(0) ; signal(SIGALRM, SIG_IGN) ;
			break ;
		}
		case 1:
		{
			fprintf(stderr, "<TIMEOUT>\n") ;
			buf[0] = '\0' ;
			break ;
		}
		default: InternalError("XReceive") ;
	}
}

void XReceiveP(CharPt buf, int bufSize, char *tempPrompt, int timeout)
{
	char savePrompt[maxPromptsize] ;
	int saveTimeout ;

	CheckInUse() ;
	strcpy(savePrompt, xPrompt) ;
	XSetPrompt(tempPrompt) ;
	saveTimeout = waitForTimeOut ;
	waitForTimeOut = timeout ;

	XReceive(buf, bufSize) ;

	waitForTimeOut = saveTimeout ;
	XSetPrompt(savePrompt) ;
}

void HandleSIGTERM()
{
	longjmp(childControl, 1) ;
}

Bool XLaunch(CharPt xappl_path)
{
	int father = getpid() ;

	EnterInUse() ;

	if( pipe(in) == -1 || pipe(out) == -1 )
		Error("Cannot open pipes") ;

	signal(SIGTERM, HandleSIGTERM) ;
	switch( setjmp(childControl) )
	{
		case 0:
		{
			if( fork() == 0 )
			{
				signal(SIGTERM, SIG_IGN) ;
				DupIn(out[0], 0) ;
				DupIn(in[1], 1) ;

				Close(out[0]) ;
				Close(out[1]) ;
				Close(in[0]) ;
				Close(in[1]) ;
					
				execl(xappl_path, xappl_path, nil) ;
				kill(father, SIGTERM) ;
				exit(0) ;
			}
			sleep(2) ;
			signal(SIGTERM, SIG_IGN) ;
			Close(in[1]) ;
			Close(out[0]) ;
			return( true ) ;
			break ;
		}
		case 1:
		{
			inUse = false ;
			Close(out[0]) ;
			Close(out[1]) ;
			Close(in[0]) ;
			Close(in[1]) ;
			fprintf(stderr,
			 "Cannot launch '%s' (maybe the pathname is wrong)\n",
			 xappl_path) ;
			return( false ) ;
			break ;
		}
		default: InternalError("XLaunch") ;
	}
}

void XClose()
{
	CheckInUse() ;
	Close(in[0]) ;
	Close(out[1]) ;
	wait(nil) ;
	inUse = false ;
}
