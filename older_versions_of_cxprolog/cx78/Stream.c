/*
 *   This file is part of the CxProlog system

 *   Stream.c
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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
static StreamPt stream ;

StreamPt userIn, userOut, userErr, currIn, currOut ;

/* PRIVATE */

static void ResetStream(StreamPt srm, FILE *f, CharPt n, StreamMode m, StreamType t)
{
	StreamTagHolder(srm) = streamSubTag ;
	StreamType(srm) = t ;
	StreamMode(srm) = m ;
	StreamFile(srm) = f ;
	strncpy(StreamName(srm), n, maxStreamName-1) ;
	StreamName(srm)[maxStreamName-1] = '\0' ;
}

static StreamPt FindAvailableStream(CharPt name)
{
	register StreamPt srm ;
	dotable(srm, stream, maxStream)
		if( StreamType(srm) == notInUse )
			return srm ;
	Error("Too many files open, when opening file '%s'", name) ;
	return nil ;
}

static StreamPt UsingStream(StreamPt srm, StreamMode mode)
{
	if( StreamType(srm) == notInUse )
		Error("Invalid operation over closed stream %s", XExtraAsStr(TagExtra(srm))) ;
	if( mode == mNone )
		return srm ;
	if( (StreamMode(srm)==mRead) == (mode==mRead) )
		return srm ;
	elif( StreamMode(srm) == mRead )
		Error("'%s' is a read stream, so it cannot be used as write stream", StreamName(srm)) ;
	else
		Error("'%s' is a write stream, so it cannot be used as read stream", StreamName(srm)) ;
	return nil ;
}

static StreamPt FindNamedStream(CharPt name, StreamMode mode)
{
	register StreamPt srm ;
	if( name[0] == '\0' )
		Error("'' is an invalid file name") ;
	if( EqualStr(name, "user") )
		return mode == mRead  ? userIn : userOut ;
	dotable(srm, stream, maxStream)
		if( EqualStr(StreamName(srm), name) )
			return UsingStream(srm, mode) ;
	return nil ;
}

static StreamPt OpenStream(CharPt name, StreamMode mode, StreamType type)
{
	register StreamPt srm ;
	if( (srm = FindNamedStream(name, mode)) != nil )
		return srm ;
	else srm = FindAvailableStream(name) ;
	if( mode == mNone )
		InternalError("OpenStream") ;
	switch( type ) {
		case fileStream: {
			FILE *file =
				fopen(name, mode == mWrite ? "w" : mode == mRead ? "r" : "a") ;
			if( file == nil )
				Error("Cannot open file '%s'", name) ;
			ResetStream(srm, file, name, mode, type) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		default: Default("OpenStream") ;
	}
	return srm ;
}


/* PUBLIC */

void StreamsInit()
{
	register StreamPt srm ;
	stream = PrimitiveAllocate(WordsOf(Stream) * maxStream) ;
	dotable(srm, stream, maxStream)
		ResetStream(srm, nil, "", mNone, notInUse) ;
	currIn = userIn = stream ;
	ResetStream(userIn, stdin, "user_input", mRead, fileStream) ;
	currOut = userOut = stream + 1 ;
	ResetStream(userOut, stdout, "user_output", mWrite, fileStream) ;
	userErr = stream + 2 ;
	ResetStream(userErr, stderr, "user_error", mWrite, fileStream) ;
}

Bool StreamCheck(VoidPt ref)
{
	register StreamPt srm = ref ;
	int i = srm - stream ;
	return InRange(i, 0, maxStream - 1)
		&& stream + i == srm
		&& StreamType(srm) != notInUse ;
}

StreamPt XTestStream(register Pt t, StreamMode mode)
{
	VarValue(t) ;
	if( IsAtom(t) ) {
		if( t == tUserAtom )
			return mode == mRead  ? userIn : userOut ;
		t = IVarGet(XAtom(t)) ;
	}
	if( IsExtra(t) && XExtraSubTag(t) == streamSubTag )
		return UsingStream(cStreamPt(XPt(t)), mode) ;
	TypeError("stream", t) ;
	return nil ;
}

void SaneStreams()
{
	currIn = userIn ;
	currOut = userOut ;
}

CharPt NameOfStream(StreamPt srm)
{
	return StreamName(srm) ;
}

StreamPt OpenFileStream(CharPt name, StreamMode mode)
{
	return OpenStream(name, mode, fileStream) ;
}

void CloseStream(StreamPt srm)
{
	if( srm == userIn || srm == userOut || srm == userErr ) return ;
	switch( StreamType(srm) ) {
		case fileStream: {
			fclose(StreamFile(srm)) ;
			ResetStream(srm, nil, "", mNone, notInUse) ;
			break ;
		}		
		case nullStream: {
			break ;
		}
		default: Default("CloseStream") ;
	}
	if( srm == currIn ) currIn = userIn ;
	elif( srm == currOut ) currOut = userOut ;
}

void CloseAllStreams()
{
	register StreamPt srm ;
	dotable(srm, stream, maxStream)
		CloseStream(srm) ;
}

void StreamsShow()
{
	register StreamPt srm ;
	CharPt type, mode ;
	AtomPt at ;
	VersionShow() ;
	Write("Streams:\n") ;
	dotable(srm, stream, maxStream)
		if( StreamType(srm) != notInUse ) {
			switch( StreamType(srm) ) {
				case fileStream: type = "File" ; break ;
				case nullStream: type = "Null" ; break ;
				default: InternalError("StreamsShow (1)") ;
			}
			switch( StreamMode(srm) ) {
				case mRead: mode = "Read" ; break ;
				case mWrite: mode = "Write" ; break ;
				case mAppend: mode = "Append" ; break ;
				default: InternalError("StreamsShow (2)") ;
			}
			Write(" %s %14.14s -> %5s, %6s, %s%s",
					(srm == currIn || srm == currOut) ? "CURR" : "    ",
					StreamName(srm),
					type,
					mode,
					XExtraAsStr(TagExtra(srm)),
					EndOfStream(srm) ? ", at end of file" : ""
			) ;
			at = IVarWith(TagExtra(srm)) ;
			if( at != nil )
				Write(" (in ivar '%s')", AtomName(at)) ;
			Write("\n") ;
		}
}


/* INPUT STREAMS */

void See(CharPt name)
{
	currIn = OpenStream(name, mRead, fileStream) ;
}

Bool EndOfStream(StreamPt srm)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			return feof(StreamFile(srm)) ;
		}		
		case nullStream: {
			return true ;
		}
		default: Default("EndOfStream") ;
	}
	return false ;
}

int GetStream(StreamPt srm)
{
	register int c ;
	switch( StreamType(srm) ) {
		case fileStream: {
			c = getc(StreamFile(srm)) ;
			if( c == EOF )
				if( feof(StreamFile(srm)) ) c = eofCode ;
				else { PutStream(userOut,'\n') ; EventRestart() ; }
			break ;
		}
		case nullStream: {
			c = eofCode ;
			break ;
		}
		default: Default("GetStream") ;
	}
	return c ;
}

int GetStreamNonBlank(StreamPt srm)
{
	register int c ;
	switch( StreamType(srm) ) {
		case fileStream: {
			while( (c = getc(StreamFile(srm))) <= ' ' && c != EOF ) ;
			if( c == EOF )
				if( feof(StreamFile(srm)) ) c = eofCode ;
				else { PutStream(userOut,'\n') ; EventRestart() ; }
			break ;
		}
		case nullStream: {
			c = eofCode ;
			break ;
		}
		default: Default("GetStreamNonBlank") ;
	}
	return c ;
}

void UngetStream(StreamPt srm, int c)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			ungetc(c, StreamFile(srm)) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		default: Default("UngetStream") ;
	}
}

CharPt GetLineStream(StreamPt srm)
{
	if( EndOfStream(srm) ) return nil ;
	UseBuffer() ;
	for(;;) {
		register int c = GetStream(srm) ;
		if( c == '\n' || c == eofCode ) break ;
		BufferAddCh(c) ;
	}
	BufferAddCh('\0') ;
	return FreeBuffer() ;
}


/* OUTPUT STREAMS */

void Tell(CharPt name)
{
	currOut = OpenStream(name, mWrite, fileStream) ;
}

void FlushStream(StreamPt srm)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			fflush(StreamFile(srm)) ;
			break ;
		}		
		case nullStream: {
			break ;
		}
		default: Default("FlushStream") ;
	}
}

void PutStream(StreamPt srm, int c)
{
	switch( StreamType(srm) ) {
		case fileStream: {
			putc(c, StreamFile(srm)) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		default: Default("PutStream") ;
	}
}

void WriteStream(StreamPt srm, CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	vfprintf(StreamFile(srm), fmt, p) ;
}

void Write(CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	vfprintf(StreamFile(currOut), fmt, p) ;
}

void WriteStd(CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	vfprintf(StreamFile(userOut), fmt, p) ;
}
