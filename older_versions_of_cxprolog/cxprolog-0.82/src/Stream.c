/*
 *   This file is part of the CxProlog system

 *   Stream.c
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define cStreamPt(s)		((StreamPt)(s))
#define StreamType(s)		((s)->type)
#define StreamMode(s)		((s)->mode)
#define StreamFile(s)		((s)->file)
#define StreamName(s)		((s)->name)

extern ExtraTypePt streamType ;
extern StreamPt userIn, userOut, userErr ;
extern StreamPt origIn, origOut, origErr ;
extern StreamPt currIn, currOut ;


/* PRIVATE */

static StreamPt StreamNew(FILE *f, CharPt n, StreamMode m, StreamType t)
{
	StreamPt srm = ExtraNew(streamType) ;
	StreamType(srm) = t ;
	StreamMode(srm) = m ;
	StreamFile(srm) = f ;
	strncpy(StreamName(srm), n, maxStreamName-1) ;
	StreamName(srm)[maxStreamName-1] = '\0' ;
	return srm ;
}

static Bool FindNamedStreamAux(VoidPt x, VoidPt name)
{
	return EqualStr(StreamName(cStreamPt(x)), cCharPt(name)) ;
}
static StreamPt FindNamedStream(CharPt name, StreamMode mode)
{
	StreamPt res ;
	if( name[0] == '\0' )
		FileError("'' is an invalid file name") ;
	if( EqualStr(name, "user") )
		return mode == mRead  ? userIn : userOut ;
	res = ExtraFindFirst(streamType, FindNamedStreamAux, name) ;
	if( res != nil )
		return UsingStream(res, mode) ;
	return nil ;
}


/* PUBLIC */

void StreamsInit()
{
	streamType = ExtraTypeNew("stream", WordsOf(Stream)) ;
	currIn = userIn = origIn =
		StreamNew(stdin, "user_input", mRead, fileStream) ;
	currOut = userOut = origOut =
		StreamNew(stdout, "user_output", mWrite, fileStream) ;
	userErr = origErr =
		StreamNew(stderr, "user_error", mWrite, fileStream) ;
}

StreamPt UsingStream(StreamPt srm, StreamMode mode)
{
	if( not XExtraCheck(streamType, TagExtra(srm)) )
		FileError("Invalid operation over closed stream %s",
								XExtraAsStr(TagExtra(srm))) ;
	if( StreamMode(srm) == mNone )
		return srm ;
	if( (StreamMode(srm)==mRead) == (mode==mRead) )
		return srm ;
	elif( StreamMode(srm) == mRead )
		FileError("'%s' is a read stream, so it cannot be used as write stream", StreamName(srm)) ;
	else
		FileError("'%s' is a write stream, so it cannot be used as read stream", StreamName(srm)) ;
	return nil ;
}

CharPt StreamTypeAsStr(StreamType st)
{
	switch( st ) {
		case fileStream: return "File" ;
		case nullStream: return "Null" ;
		default: Default("StreamTypeAtStr") ;
	}
}

CharPt StreamModeAsStr(StreamMode sm)
{
	switch( sm ) {
		case mRead: return "Read" ;
		case mWrite: return "Write" ;
		case mAppend: return "Append" ;
		default: Default("StreamModeAtStr") ;
	}
}


/* OPEN & CLOSE */

StreamPt FileStreamOpen(CharPt name, StreamMode mode)
{
	StreamPt srm ;
	FILE *file ;
	if( (srm = FindNamedStream(name, mode)) != nil )
		return srm ;
	if( (file = fopen(name, mode==mWrite?"w": mode==mRead?"r": "a")) == nil )
		FileError("Cannot open file '%s'", name) ;
	return StreamNew(file, name, mode, fileStream) ;
}

StreamPt FILEToStream(FILE *file, StreamMode mode)
{
	Str256 s ;
	sprintf(s, "FILE_%lx", file) ;
	return StreamNew(file, s, mode, fileStream) ;
}

void StreamClose(StreamPt srm)
{
	if( srm == userIn || srm == userOut || srm == userErr ) return ;
	switch( StreamType(srm) ) {
		case fileStream: {
			fclose(StreamFile(srm)) ;
			ExtraDelete(streamType, srm) ;
			break ;
		}		
		case nullStream: {
			break ;
		}
		default: Default("StreamClose") ;
	}
	if( srm == currIn ) currIn = userIn ;
	elif( srm == currOut ) currOut = userOut ;
}


/* INPUT STREAMS */

int StreamGet(StreamPt srm)
{
	register int c ;
	switch( StreamType(srm) ) {
		case fileStream: {
	redo:	c = getc(StreamFile(srm)) ;
			if( c == EOF )
				if( InterruptHandle() )
					goto redo ;
				else c = eofCode ;
			break ;
		}
		case nullStream: {
			c = eofCode ;
			break ;
		}
		default: Default("StreamGet") ;
	}
	return c ;
}

int StreamGetNonBlank(StreamPt srm)
{
	register int c ;
	switch( StreamType(srm) ) {
		case fileStream: {
	redo:	while( (c = getc(StreamFile(srm))) <= ' ' && c != EOF ) ;
			if( c == EOF )
				if( InterruptHandle() )
					goto redo ;
				else c = eofCode ;
			break ;
		}
		case nullStream: {
			c = eofCode ;
			break ;
		}
		default: Default("StreamGetNonBlank") ;
	}
	return c ;
}

void StreamUnget(StreamPt srm, int c)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			ungetc(c, StreamFile(srm)) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		default: Default("StreamUnget") ;
	}
}

Bool StreamAtEnd(StreamPt srm)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			return feof(StreamFile(srm)) ;
		}		
		case nullStream: {
			return true ;
		}
		default: Default("StreamAtEnd") ;
	}
	return false ;
}


/* OUTPUT STREAMS */

void StreamPut(StreamPt srm, int c)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			putc(c, StreamFile(srm)) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		default: Default("StreamPut") ;
	}
}

void StreamFlush(StreamPt srm)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			fflush(StreamFile(srm)) ;
			break ;
		}		
		case nullStream: {
			break ;
		}
		default: Default("StreamFlush") ;
	}
}
