/*
 *   This file is part of the CxProlog system

 *   Stream.c
 *   by A.Miguel Dias - 1989/12/02
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

#define maxStream 16
#define maxStreamName 255

#define eofCode	26

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

static void ResetStream(int i, FILE *f, CharPt n, StreamMode m, StreamType t)
{
	stream[i].file = f ;
	strcpy(stream[i].name, n) ;
	stream[i].mode = m ;
	stream[i].type = t ;
/*	stream[i].last = '\0' ; */
}

static int FindStream(CharPt name)
{
	register int i ;
	
	for( i = 2 ; i < maxStream ; i++ )	
		if( InUse(i) && EqualStr(stream[i].name, name) ) return i ;
	return -1 ;
}

static int FindAvailableStream(CharPt name)
{
	register int i ;
	
	for( i = 2 ; i < maxStream ; i++ )	
		if( not InUse(i) ) return i ;
	return -1 ;
}

static int OpenStream(CharPt name, StreamMode mode, StreamType type)
{
	register int i ;
	FILE *file ;

	if( (i = FindAvailableStream(name)) == -1 )
			Error("Too many files open, openning '%s'", name) ;
	switch( type ) {
		case fileStream: {
			if( (file = fopen(name, mode == output ? "w" : "r")) == nil )
					Error("Cannot open file '%s'", name) ;
			break ;
		}		
		default: Default("OpenStream") ;
	}
	ResetStream(i, file, name, mode, type) ;
	return i ;
}

static void CloseStream(int i)
{
	if( i == dflInputStream || i == dflOutputStream ) return ;
	switch( stream[i].type ) {
		case fileStream: {
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
	switch( stream[i].type ) {
		case fileStream: {
			fflush(stream[i].file) ;
			break ;
		}		
		default: Default("FlushStream") ;
	}
}

static Bool EndOfStream(int i)
{
	switch( stream[i].type ) {
		case fileStream: {
			return feof(stream[i].file) ;
			break ;
		}		
		default: Default("EndOfStream") ;
	}
}

static void PutStream(int i, int c)
{
	switch( stream[i].type ) {
		case fileStream: {
			putc(c, stream[i].file) ;
			break ;
		}
		default: Default("PutStream") ;
	}
}

static int GetStream(int i)
{
	register int c ;

	switch( stream[i].type ) {
		case fileStream: {
	redo:	interr = false ;
			c = getc(stream[i].file) ;
			if( c == EOF )
				if( interr ) goto redo ;
				else c = eofCode ;
			break ;
		}
		default: Default("GetStream") ;
	}
	return c ;
}

static void UngetStream(int i, int c)
{
	switch( stream[i].type ) {
		case fileStream: {
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

void Flush(CharPt name)
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

CharPt Seeing()
{
	return stream[inputStream].name ;
}


CharPt Telling()
{
	return stream[outputStream].name ;
}

void See(CharPt name)
{
	int i ;
	
	if( EqualStr(name, "user") ) i = dflInputStream ;
	else {
		if( (i = FindStream(name)) == -1 )
				i = OpenStream(name, input, fileStream) ;
		elif( stream[i].mode == output ) {
			CloseStream(i) ;
			i = OpenStream(name, input, fileStream) ;
		}
	}
	inputStream = i ;
}

void Tell(CharPt name)
{
	int i;

	if( EqualStr(name, "user") ) i = dflOutputStream ;
	else {
		if( (i = FindStream(name)) == -1 )
				i = OpenStream(name, output, fileStream) ;
		elif( stream[i].mode == input ) {
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

void PutString(CharPt s)
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

void Write(CharPt fmt, ...)
{
	char s[1 Kb] ;
	va_list p ;

	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	PutString(s) ;
}

void WriteStd(CharPt fmt, ...)
{
	char s[1 Kb] ;
	va_list p ;
	int saveOut ;

	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	saveOut = outputStream ;
	outputStream = dflOutputStream ;
	PutString(s) ;
	outputStream = saveOut ;
}

void Prompt(CharPt s)
{
	if( inputStream == dflInputStream && outputStream == dflOutputStream ) {
		PutString(s) ;
		FlushStream(outputStream) ;
	}
}

int Get0()
{
	return /*stream[inputStream].last =*/ GetStream(inputStream) ;
}

int Get()
{
	int c ;
	
	while( ( c = Get0() ) <= ' ' && c != eofCode ) ;
	return c ;
}

CharPt GetLine()
{
	register CharPt b, lim ;
	register int c ;
	
	for( b = strBuffer, lim = b + strBufferSize - 1 ; b < lim ; b++ ) {
		c = GetStream(inputStream) ;
		if( c == '\n' || c == eofCode ) break ;
		*b = c ;
	}
	*b = '\0' ;
	if( b == lim )
		Error("String to long: '%s'", b) ;
	return strBuffer ;
}

void Skip(int c)
{
	while( Get0() != c ) ;
}

int Peek0()
{
	int c = GetStream(inputStream) ;
	UngetStream(inputStream, c) ;
	return c ;
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
	if( saveOutputStream != -1 ) {
		outputStream = saveOutputStream ;
		saveOutputStream = -1 ;
	}
}

void GetCharCommand(int *comm, int *arg)
{
	int saveInputStream, c, n ;

	FlushStream(dflInputStream) ;
	saveInputStream = inputStream ;
	inputStream = dflInputStream ;
	while( ( c = Get0() ) <= ' ' && c != '\n' && c != eofCode ) ;
	*comm = InRange(c,'A','Z') ? (c - 'A' + 'a') : c ;
	n = 0 ;
	while( c != '\n' && c != eofCode ) {
		if( InRange(c, '0', '9') )
			n = n * 10 + c - '0' ;
		c = Get0() ;
	}
	*arg = n ;
	while( c != '\n' && c != eofCode )
		c = Get0() ;
	inputStream = saveInputStream ;
}
