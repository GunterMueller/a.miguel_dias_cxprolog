/*
 *   This file is part of the NanoProlog system

 *   NanoCGI.c
 *   by A.Miguel Dias - 97/06/04
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
 970604: first version
*/

/* NanoCGI: this is a CGI script written in C that communicates
  with NanoProlog using a TCP/IP socket.
   
 When a HTML FORM is used, the information collected is placed in the
environment variable QUERY_STRING. Then the CGI script gets the information
from that variable. 
   
What does this CGI script (NanoCGI) do?
1- NanoCGI gets the request from the environment variable QUERY_STRING.
2- NanoCGI sends the request (1 line) to NanoProlog which gets it using socket_get/1.
3- NanoProlog handles the request and then replies to NanoCGI using the socket_reply/1.
4- NanoCGI receives the reply (multiple lines) and writes it on the standard output.
5- This is an HTML encoded page that is immediately displayed by the browser.

Example of a form that activates NanoCGI:

*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#ifndef INADDR_NODE
#define INADDR_NODE	0xffffffff
#endif

u_long inet_addr() ;

void Error(char *s)
{
	fprintf(stderr, s) ;
	exit( 1 ) ;
}

/* allocate and connect a client socket */
int connectTCP(char *hostname, int port)
{
	struct hostent *host ;
	struct protoent *env ;
	struct sockaddr_in sin ;
	int s ;

	bzero((char *)&sin, sizeof(sin)) ;
	sin.sin_family = AF_INET ;
	sin.sin_port = port ;
	if( host = gethostbyname(hostname) ) /* map host name to IP address */
		bcopy(host->h_addr, (char *)&sin.sin_addr, host->h_length) ;
	else if( (sin.sin_addr.s_addr = inet_addr(hostname)) == INADDR_NODE )
		Error("can't get host entry\n") ;

	if( (env = getprotobyname("tcp")) == 0)
		Error("can't get TCP protocol entry\n") ;
	if( (s = socket(PF_INET, SOCK_STREAM, env->p_proto)) < 0 )
		Error("can't create socket\n") ;
	if( connect(s, (struct sockaddr *)&sin, sizeof(sin)) < 0)
		Error("can't connect to socket\n") ;

	return s ;
}

void convert(char *buf) /* Convert %xxx characters */
{
	char *get, *put ;
	char z[3] ;
	int i ;

	for( get = put = buf ; *get != '\0' ; )
	{
		if( *get == '%' )
		{
			z[0] = get[1] ;
			z[1] = get[2] ;
			z[3] = '\0' ;
			sscanf(z, "%x", &i) ;
			get += 3 ;
			*put++ = i ;
		}
		else *put++ = *get++ ;
	}
	*put = '\0' ;
}

main(int argc, char *argv[])
{
	char *host ;
	int port ;
	char buf[2000] ;
	char *query ;
	int s, n ;

/*
	if( argc != 2 )
		Error("usage: NanoCGI port_number\n") ;
	if( sscanf(argv[1], "%d", &port) != 1 )
		Error("Invalid port number\n") ;
*/

	host = "localhost" ;
	port = 3007 ;

/* READ REQUEST LINE */
	if( (query = getenv("QUERY_STRING")) == NULL || strlen(query) == 0 )
		strcpy(buf, "n=999");
	else
		strncpy(buf, query, 1999); /* SAFE COPY */
	strcat(buf, "\n");
	convert(buf) ;

/* CONNECT WITH THE SOCKET */
	s = connectTCP(host, port) ;
	
/* SEND REQUEST LINE TO NANOPROLOG VIA SOCKET */
	if( write(s, buf, strlen(buf)) <= 0 )
		Error("Cannot write in socket\n") ;

/* RECEIVE NANOPROLOG'S REPLY VIA SOCKET  */
	while( (n = read(s, buf, 1999)) > 0 )
	{
		buf[n] = '\0' ;
/* WRITE NANOPROLOG'S REPLY (this should be an entire HTML page) */
		fputs(buf, stdout) ;
	}

/* PROGRAM TERMINATES NORMALLY */
}
