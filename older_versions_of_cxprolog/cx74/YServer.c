/*
 *   This file is part of the CxProlog system

 *   YServer.c
 *   by A.Miguel Dias - 1997/05/10
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#define NotInUse	-2
#define InUse(x)	( (x) != NotInUse )

static int msock = NotInUse, ssock = NotInUse ;

/* allocate and bind a server socket */
static int passiveTCP(int port, int qlen)
{
	struct protoent *env ;
	struct sockaddr_in sin ;
	int s ;

	bzero((char *)&sin, sizeof(sin)) ;
	sin.sin_family = AF_INET ;
	sin.sin_addr.s_addr = INADDR_ANY ;
	sin.sin_port = port ;

	if( (env = getprotobyname("tcp")) == 0)
		SysErrorN("can't get TCP protocol entry") ;

	if( (s = socket(PF_INET, SOCK_STREAM, env->p_proto)) < 0 )
		SysErrorN("can't create socket") ;
	if( bind(s, (struct sockaddr *)&sin, sizeof(sin)) < 0 )
		SysErrorN("can't bind to port") ;
	if( listen(s, qlen) < 0 )
		SysErrorN("can't listen to port") ;

	return s ;
}

void YInstallSocket(int port)
{
	if( InUse(msock) )
		Error("The server is already active") ;
	msock = passiveTCP(port, 5) ;
}

static void YReadLn(CharPt buf, int bufSize)
{
	int s, size ;

	size = 0 ;
	do {
		if( (s = read(ssock, buf + size, bufSize - size)) <= 0 )
			SysErrorN("Unable to read from socket") ;
			buf[size += s] = '\0' ;
			if( size == bufSize )
				Error("ybuffer overflow") ;
	}
	while( buf[size-1] != '\n' ) ;
}

void YGetRequest(CharPt buf, int bufSize)
{
	struct sockaddr_in fsin ;
	int alen = sizeof(struct sockaddr_in) ;

	if( not InUse(msock) )
		Error("There is no server active") ;
	if( InUse(ssock) )
		Error("There is a request already being processed") ;
	if( (ssock = accept(msock, (struct sockaddr *)&fsin, &alen)) < 0 )
		SysErrorN("accept failed") ;
	YReadLn(buf, bufSize) ;
}

void YReplyToRequest(CharPt str)
{
	if( not InUse(msock) )
		Error("There is no server active") ;
	if( not InUse(ssock) )
		Error("There is no request active") ;
	if( write(ssock, str, strlen(str)) < 0 )
		SysErrorN("Unable to write to socket") ;
}

void YDoneRequest(void)
{
	if( not InUse(msock) )
		Error("There is no server active") ;
	if( not InUse(ssock) )
		Error("There is no request active") ;
	close(ssock) ;
	ssock = NotInUse ;
}

static void YCloseSocket(void)
{
	if( InUse(ssock) )
		Error("Cannot close because there is a request active") ;
	if( not InUse(msock) )
		Error("There is no server active") ;
	close(msock) ;
	msock = NotInUse ;
}
