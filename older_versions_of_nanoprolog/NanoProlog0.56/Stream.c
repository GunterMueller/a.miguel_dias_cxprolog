/*
 *   This file is part of the NanoProlog system

 *   Stream.c
 *   by A.Miguel Dias - 89/12/2
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

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

 990527: new function "GetString(char *buff, char *sw)"
 		 PeekStream replaced by UngetStream
 931117: release of version 0.5

*/

#include "NanoProlog.h"
#include <limits.h>

#define maxStream 16
#define maxStreamName 255

#define CtrlZ	26

typedef enum { notInUse, fileStream, windowStream } StreamType ;
typedef enum { none, input, output } StreamMode ;

static struct
{
	StreamType type ;
	StreamMode mode ;
	FILE *file ;
	char name[maxStreamName] ;
/*	char last ; */
} stream[maxStream] ; 

static int inputStream, outputStream, saveOutputStream ;

#define InUse(i)	( stream[i].type != notInUse )

/* */

static void ResetStream(int i, FILE *f, char *n, StreamMode m, StreamType t)
{
	stream[i].file = f ;
	strcpy(stream[i].name, n) ;
	stream[i].mode = m ;
	stream[i].type = t ;
/*	stream[i].last = '\0' ; */
}

static int FindStream(char *name)
{
	register int i ;
	
	for( i = 2 ; i < maxStream ; i++ )	
		if( InUse(i) && EqualStr(stream[i].name, name) ) return( i ) ;
	return( -1 ) ;
}

static int FindAvailableStream(char *name)
{
	register int i ;
	
	for( i = 2 ; i < maxStream ; i++ )	
		if( not InUse(i) ) return( i ) ;
	return( -1 ) ;
}

static int OpenStream(char *name, StreamMode mode, StreamType type)
{
	register int i ;
	FILE *file ;

	if( (i = FindAvailableStream(name)) == -1 )
			Error("Too many files open, openning '%s'", name) ;
	switch( type )
	{
		case fileStream:
		{
			if( (file = fopen(name, mode == output ? "w" : "r")) == nil )
					Error("Cannot open file '%s'", name) ;
			break ;
		}		
		default: Default("OpenStream") ;
	}
	ResetStream(i, file, name, mode, type) ;
	return( i ) ;
}

static void CloseStream(int i)
{
	if( i == dflInputStream || i == dflOutputStream ) return ;
	switch( stream[i].type )
	{
		case fileStream:
		{
			fclose(stream[i].file) ;
			ResetStream(i, nil, "", none, notInUse) ;
			break ;
		}		
		default: Default("CloseStream") ;
	}
	if( i == inputStream ) inputStream  = dflInputStream ;
	elif( i == outputStream ) outputStream = dflOutputStream ;
}

static void FlushStream(int i)
{
	switch( stream[i].type )
	{
		case fileStream:
		{
			fflush(stream[i].file) ;
			break ;
		}		
		default: Default("FlushStream") ;
	}
}

static Bool EndOfStream(int i)
{
	switch( stream[i].type )
	{
		case fileStream:
		{
			return( feof(stream[i].file) ) ;
			break ;
		}		
		default: Default("EndOfStream") ;
	}
}

static void PutStream(int i, int c)
{
	switch( stream[i].type )
	{
		case fileStream:
		{
			putc(c, stream[i].file) ;
			break ;
		}
		default: Default("PutStream") ;
	}
}

static int GetStream(int i)
{
	register int c ;

	switch( stream[i].type )
	{
		case fileStream:
		{
            c = getc(stream[i].file) ;
            if( c == EOF ) c = CtrlZ ;
			break ;
		}
		default: Default("GetStream") ;
	}
	return( c ) ;
}

static void UngetStream(int i, int c)
{
	switch( stream[i].type )
	{
		case fileStream:
		{
			ungetc(c, stream[i].file) ;
			break ;
		}
		default: Default("UngetStream") ;
	}
}


/* PUBLIC */

void InitStreams()
{
	int i;

	dotimes(i, maxStream)
		ResetStream(i, nil, "", none, notInUse) ;
	ResetStream(dflInputStream, stdin, "user", input, fileStream) ;
	ResetStream(dflOutputStream, stdout, "user", output, fileStream) ;
	inputStream = dflInputStream ;
	outputStream = dflOutputStream ;
}

void RecoverStreams()
{
	inputStream = dflInputStream ;
	outputStream = dflOutputStream ;
}

void CloseAllStreams()
{
	int i ;

	dotimes(i, maxStream)
		if( InUse(i) ) CloseStream(i) ;
}

void Flush(char *name)
{
	int i;

	if( (i = FindStream(name)) != -1 ) FlushStream(i) ;
}

void Seen()
{
	CloseStream(inputStream) ;
}

void Told()
{
	CloseStream(outputStream) ;
}

char *Seeing()
{
	return( stream[inputStream].name ) ;
}


char *Telling()
{
	return( stream[outputStream].name ) ;
}

void See(char *name)
{
	int i ;
	
	if( EqualStr(name, "user") ) i = dflInputStream ;
	else
	{
		if( (i = FindStream(name)) == -1 )
				i = OpenStream(name, input, fileStream) ;
		elif( stream[i].mode == output )
		{
			CloseStream(i) ;
		    i = OpenStream(name, input, fileStream) ;
		}
	}
	inputStream = i ;
}

void Tell(char *name)
{
	int i;
    
	if( EqualStr(name, "user") ) i = dflOutputStream ;
	else
	{
		if( (i = FindStream(name)) == -1 )
				i = OpenStream(name, output, fileStream) ;
		elif( stream[i].mode == input )
		{
			CloseStream(i) ;
		    i = OpenStream("name", output, fileStream) ;
		}
	}
	outputStream = i ;
}

void Put(int c)
{
	PutStream(outputStream, c) ;
}

void PutString(char *s)
{
	while( *s ) Put(*s++) ;
}

void Nl()
{
	Put('\n') ;
}

void Tab(int n)
{
	while( n-- ) Put(' ') ;
}

void Prompt(char *s)
{
	if( inputStream == dflInputStream && outputStream == dflOutputStream )
	{
		PutString(s) ;
		FlushStream(outputStream) ;
    }
}

int Get0()
{
	return( /*stream[inputStream].last =*/ GetStream(inputStream) ) ;
}

int Get()
{
	int c ;
	
	while( ( c = Get0() ) <= ' ' && c != CtrlZ ) ;
	return( c ) ;
}

static void NotGetString(char *buff, char *stop)
{
	register char *pt ;
	register int c ;
	
	for(;;)
	{
		c = GetStream(inputStream) ;
		if( c == stop[0] || c == '\n' || c == CtrlZ ) goto end ;
		for( pt = stop+1 ; *pt != '\0' ; pt++ )
			if( c == *pt ) goto end ;
		*buff++ = c ;
	}
end:
	UngetStream(inputStream, c) ;
	*buff = '\0' ;
}

static void YesGetString(char *buff, char *want)
{
	register char *pt ;
	register int c ;
	
	for(;;)
	{
		c = GetStream(inputStream) ;
		if( c == '\n' || c == CtrlZ ) goto end ;
		for( pt = want ; *pt != '\0' ; pt++ )
			if( c == *pt ) goto found ;
		goto end ;
	found:
		*buff++ = c ;
	}
end:
	UngetStream(inputStream, c) ;
	*buff = '\0' ;
}

void GetString(char *buff, char *sw)
{
	register char *pt ;
	register int c ;
	
	if( sw[0] == '\0' ) ;
	elif( sw[0] == '~' && sw[1] == '\0' ) {
			for(;;) {
				c = GetStream(inputStream) ;
				if( c == '\n' || c == CtrlZ ) break ;
				*buff++ = c ;
			}
			UngetStream(inputStream, c) ;
			*buff = '\0' ;
	}
	elif( sw[0] == '~' ) NotGetString(buff, sw + 1) ;
	else YesGetString(buff, sw) ;
}

void Skip(c)
int c ;
{
	while( Get0() != c ) ;
}


int Peek0()
{
	int c = GetStream(inputStream) ;
	UngetStream(inputStream, c) ;
	return( c ) ;
}

void SetTempOutput(int id)
{
	if( stream[id].mode != output )
		Error("Not an output stream") ;
	saveOutputStream = outputStream ;
	outputStream = id ;
}

void RestoreOutput()
{
	if( saveOutputStream != -1 )
	{
		outputStream = saveOutputStream ;
		saveOutputStream = -1 ;
	}
}

