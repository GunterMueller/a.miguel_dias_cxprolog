/*
 *   This file is part of the CxProlog system

 *   Stream.c
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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
#include <locale.h>
#define __USE_ISOC99	1
#include <wchar.h>

#define classicStreams	0

#define cStreamPt(s)		((StreamPt)(s))

#define StreamChannel(s)	((s)->channel)
#define StreamLocale(s)		((s)->locale)
#define StreamMode(s)		((s)->mode)
#define StreamKind(s)		((s)->kind)
#define StreamIsBinary(s)	((s)->isBinary)
#define StreamName(s)		((s)->name)

static ExtraTypePt streamType ;
static CharPt defaultLocale, currentLocale ;
static StreamPt origIn, origOut, origErr ;
StreamPt userIn, userOut, userErr ;
StreamPt currIn, currOut ;

static Pt tReadAtom, tWriteAtom, tAppendAtom, tTextAtom, tBinaryAtom ;
static CharPt stringStreamData ;
static Pt listStreamData ;

/* AUXILIARY PRIVATE FUNCTIONS */

static CharPt SetLocaleSafe(CharPt l)
{
	CharPt loc ;
	if( (loc = AllocStr(setlocale(LC_CTYPE, l))) == nil )
		Error("Cannot set locale %s", l) ;
	else currentLocale = loc ;
	return currentLocale ;
}

static void SetLocaleUnsafe(CharPt l)
{	/* pre: l is an already validated locale */
	if( setlocale(LC_CTYPE, l) == nil ) FatalError("Cannot set locale %s", l) ;
	currentLocale = l ;
}

static void SetLocaleInit(void) /* called when no resources available */
{
	CharPt loc ;
	/* setlocale returns "C" if LC_ALL, LC_CTYPE and LANG are undefined.
       setlocale returns nil only if LC_ALL, LC_CTYPE and LANG contains an
       invalid locale specification. */
	if( (loc = setlocale(LC_CTYPE, "")) == nil ) {
		fprintf(stderr, "Could not initialize locale LC_TYPE\n") ;
		exit(1) ;
	}
	currentLocale = defaultLocale = strdup(loc) ; /* temporary mem */
}

static void SetLocaleInit2(void) /* called when all resorces are available */
{	/* fix defaultLocale and currentLocale */
	CharPt freeLocale = defaultLocale ;
	defaultLocale = SetLocaleSafe(defaultLocale) ;
	StreamLocale(userIn) = defaultLocale ;
	StreamLocale(userOut) = defaultLocale ;
	StreamLocale(userErr) = defaultLocale ;
	free(freeLocale) ; /* free temporary mem */
}

static StreamPt StreamNew(VoidPt chn, CharPt n, StreamMode m, StreamKind k)
{
	StreamPt srm = ExtraNew(streamType) ;
#if classicStreams
	if( k == localeTextFileStream ) k = textFileStream ;
#endif
	StreamChannel(srm) = chn ;
	StreamLocale(srm) = defaultLocale ;
	StreamMode(srm) = m ;
	StreamKind(srm) = k ;
    StreamIsBinary(srm) = false ;
	strncpy(StreamName(srm), n, maxStreamName-1) ;
	StreamName(srm)[maxStreamName-1] = '\0' ;
	return srm ;
}

static StreamPt UsingStream(StreamPt srm, StreamMode mode)
{
	if( !XExtraCheck(streamType, TagExtra(srm)) )
		FileError("Invalid operation over closed stream %s",
								XExtraAsStr(TagExtra(srm))) ;
	if( mode == mNone )
		return srm ;
	if( (StreamMode(srm)==mRead) == (mode==mRead) )
		return srm ;
	elif( StreamMode(srm) == mRead )
		return FileError("'%s' is a read stream; cannot be used as write stream",
							StreamName(srm)) ;
	else
		return FileError("'%s' is a write stream; cannot be used as read stream",
							StreamName(srm)) ;
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
	if( EqualStrN(name, "user", 4) ) {
		if( EqualStr(name, "user") )
			return mode == mRead ? userIn : userOut ;
		if( EqualStr(name, "user_input") )
			return userIn ;
		if( EqualStr(name, "user_output") )
			return userOut ;
		if( EqualStr(name, "user_error") )
			return userErr ;
	}
	if( (res = ExtraFindFirst(streamType, FindNamedStreamAux, name)) != nil )
		return UsingStream(res, mode) ;
	return nil ;
}

static void SetStdStreamsIVars()
{
	IVarForceSet(LookupAtom("user_input"), TagExtra(userIn), true) ;
	IVarForceSet(LookupAtom("user_output"), TagExtra(userOut), true) ;
	IVarForceSet(LookupAtom("user_error"), TagExtra(userErr), true) ;
}

static CharPt StreamModeStr(StreamPt srm)
{
	switch( StreamMode(srm) ) {
		case mRead: return "read" ;
		case mWrite: return "write" ;
		case mAppend: return "append" ;
		default: return InternalError("StreamModeAtStr") ;
	}
}

static StreamMode XTestStreamMode(Pt t)
{
	t = Drf(t) ;
	if( t == tReadAtom ) return mRead ;
	elif( t == tWriteAtom ) return mWrite ;
	elif( t == tAppendAtom ) return mAppend ;
	else { FileError("Invalid stream mode") ; return 0 ; }
}


/* CONSTRUCTORS */

StreamPt FileStreamOpen(CharPt name, StreamMode mode, CharPt locale)
{
	StreamPt srm ;
	FILE *file ;
	Str32 m ;
	Bool isBinary ;

/* check existence */
	if( (srm = FindNamedStream(name, mode)) != nil )
		return srm ;

/* open file */
	isBinary = locale != nil && EqualStr(locale, "binary") ;
	strcpy(m, mode==mWrite ? "w" : mode==mRead ? "r" : "a") ;
	if( isBinary ) strcat(m, "b") ;
	if( (file = fopen(name, m)) == nil )
		FileError("Cannot open file '%s'", name) ;

/* setup descriptor */
	srm = StreamNew(file, name, mode,
					isBinary ? binaryFileStream : localeTextFileStream) ;
	if( isBinary )
		StreamIsBinary(srm) = true ;
	elif( locale != nil && !EqualStr(locale, "text") )
		StreamLocale(srm) = SetLocaleSafe(locale) ;
	return srm ;
}

StreamPt BufferStreamOpen(VoidPt buff /* @@ */, StreamMode mode)
{
	StreamPt srm ;
	Str256 name ;
	sprintf(name, "_%%buffer_%lx", buff) ;
	if( mode == mWrite )
		BufferRewrite(buff) ;
	elif( mode == mRead )
		BufferRewrite(buff) ;
	elif( mode == mAppend ) ;
		BufferAppend(buff) ;
	srm = StreamNew(buff, name, mode, bufferStream) ;
	StreamIsBinary(srm) = true ;
	return srm ;
}

StreamPt NullStreamOpen()
{
	static int n = 0 ;
	Str32 name ;
	sprintf(name, "_%%null_%d", n++) ;
	return StreamNew(nil, name, mWrite, nullStream) ;
}

StreamPt StringStreamOpen(CharPt string)
{
	static StreamPt ss = nil ;
	if( ss == nil )
		ss = StreamNew(nil, "_%string", mRead, stringStream) ;	
	stringStreamData = string ;
	return ss ;
}

StreamPt ListStreamOpen(Pt list)
{
	static StreamPt ss = nil ;
	if( ss == nil )
		ss = StreamNew(nil, "_%list", mRead, listStream) ;	
	listStreamData = Drf(list) ;
	return ss ;
}

StreamPt ScratchStreamOpen()
{
	static StreamPt bs = nil ;
	if( bs == nil )
		bs = StreamNew(nil, "_%scratch", mWrite, scratchStream) ;
	UseScratch() ;
	return bs ;
}

StreamPt FILEToStream(FILE *file, StreamMode mode, CharPt prefName)
{
	Str256 n ;
	sprintf(n, "%s_%lx", prefName, file) ;
	return StreamNew(file, n, mode, textFileStream) ;
}


/* PRIMITIVE OPERATIONS */

int StreamGetChar(StreamPt srm)
{
	register int c ;
	switch( StreamKind(srm) ) {
		case textFileStream: {
			for(;;) { /* this works even with raw input or sockets */
				c = fgetc(StreamChannel(srm)) ;
				if( c >= ' ' || c == '\n' ) return c ;
				if( c == 0 || c == 10 || c == 13 ) continue ;
				if( c == EOF || c == 4 || c == 26 ) /* CNTL-D, CNTL-Z */
					if( InterruptHandle() ) continue ;
					else return eofMark ;
				return c ;
			}
		}
		case localeTextFileStream: {
			if( StreamLocale(srm) != currentLocale )
				SetLocaleUnsafe(StreamLocale(srm)) ;	
			for(;;) { /* this works even with raw input or sockets */
				c = fgetwc(StreamChannel(srm)) ;
				if( c > 255 ) return '?' ;
				if( c >= ' ' || c == '\n' ) return c ;
				if( c == 0 || c == 10 || c == 13 ) continue ;
				if( c == 4 || c == 26 ) { /* CNTL-D, CNTL-Z */
					ungetwc(c, StreamChannel(srm)) ;
					return eofMark ;
				}
				if( c == EOF )
					if( errno == EILSEQ ) Error("Wide character conversion error") ;
					elif( InterruptHandle() ) continue ;
					else return eofMark ;
				return c ;
			}
		}
		case binaryFileStream: {
			for(;;) {
				c = fgetc(StreamChannel(srm)) ;
				if( c == EOF )
					if( InterruptHandle() ) continue ;
					else return -1 ;
				return c ;
			}
		}
		case bufferStream: {
			return BufferAtEnd(StreamChannel(srm))
					? -1
					: BufferGetChar(StreamChannel(srm)) ;
		}
		case nullStream: {
			FileError("This operation is not available for null streams") ;
			return 0 ;
		}
		case stringStream: {
			if( *stringStreamData == '\0' )
				return  eofMark ;
			else return *stringStreamData++ ;
		}
		case listStream: {
			if( listStreamData == tNilAtom )
				return eofMark ;
			elif( IsList(listStreamData) ) {
				Pt t = Drf(XListHead(listStreamData)) ;
				listStreamData = Drf(XListTail(listStreamData)) ;
				return XTestCode(t) ;
			}
			else TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
		}
		case scratchStream: {
			FileError("This operation is not available for scratch streams") ;
			return 0 ;
		}		
		default: return IInternalError("StreamGetChar") ;
	}
}

int StreamPeekChar(StreamPt srm)
{
	register int c ;
	switch( StreamKind(srm) ) {
		case textFileStream: {
			for(;;) { /* this works even with raw input or sockets */
				c = fgetc(StreamChannel(srm)) ;
				if( c >= ' ' || c == '\n' ) return ungetc(c,StreamChannel(srm));
				if( c == 0 || c == 10 || c == 13 ) continue ;
				if( c == EOF || c == 4 || c == 26 ) /* CNTL-D, CNTL-Z */
					if( InterruptHandle() ) continue ;
					else return eofMark ;
				return ungetc(c, StreamChannel(srm)) ;
			}
		}
		case localeTextFileStream: {
			if( StreamLocale(srm) != currentLocale )
				SetLocaleUnsafe(StreamLocale(srm)) ;	
			for(;;) { /* this works even with raw input or sockets */
				c = fgetwc(StreamChannel(srm)) ;
				if( c > 255 ) return ungetwc('?',StreamChannel(srm));
				if( c >= ' ' || c == '\n' ) return ungetwc(c,StreamChannel(srm));
				if( c == 0 || c == 10 || c == 13 ) continue ;
				if( c == 4 || c == 26 ) {/* CNTL-D, CNTL-Z */
					ungetwc(c, StreamChannel(srm)) ;
					return eofMark ;
				}
				if( c == EOF ) {
					if( errno == EILSEQ ) Error("Wide character conversion error.") ;
					elif( InterruptHandle() ) continue ;
					else return eofMark ;
				}
				return ungetwc(c, StreamChannel(srm)) ;
			}
		}
		case binaryFileStream: {
			for(;;) {
				c = fgetc(StreamChannel(srm)) ;
				if( c == EOF )
					if( InterruptHandle() ) continue ;
					else return -1 ;
				return ungetc(c, StreamChannel(srm)) ;
			}
		}
		case bufferStream: {
			return BufferAtEnd(StreamChannel(srm))
					? -1
					: BufferPeekChar(StreamChannel(srm)) ;
		}
		case nullStream: {
			FileError("This operation is not available for null streams") ;
			return 0 ;
		}
		case stringStream: {
			if( *stringStreamData == '\0' )
				return  eofMark ;
			else return *stringStreamData ;
		}
		case listStream: {
			if( listStreamData == tNilAtom )
				return eofMark ;
			elif( IsList(listStreamData) ) {
				Pt t = Drf(XListHead(listStreamData)) ;
				return XTestCode(t) ;
			}
			else TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
		}
		case scratchStream: {
			FileError("This operation is not available for scratch streams") ;
			return 0 ;
		}
		default: return IInternalError("StreamPeekChar") ;
	}
}

void StreamReadN(StreamPt srm, VoidPt v, Size n)
{
	switch( StreamKind(srm) ) {
		case textFileStream:
		case binaryFileStream: {
			fread(v, 1, n, StreamChannel(srm)) ;
			break ;
		}
		case nullStream: {
			FileError("This operation is not available for null streams") ;
			break ;
		}
		case stringStream: {
			FileError("This operation is not available for string streams") ;
			break ;
		}
		case listStream: {
			FileError("This operation is not available for list streams") ;
			break ;
		}
		case scratchStream: {
			FileError("This operation is not available for scratch streams") ;
			break ;
		}
		default: InternalError("StreamReadN") ;
	}
}

Bool StreamAtEnd(StreamPt srm)
{
	if( StreamMode(srm) != mRead ) return false ;
		
 	switch( StreamKind(srm) ) {
		case textFileStream:
		case localeTextFileStream:
		case binaryFileStream: {
			return feof(StreamChannel(srm)) ;
		}
		case bufferStream: {
			return BufferAtEnd(StreamChannel(srm)) ;
		}
		case nullStream: {
			return false ;
		}
		case stringStream: {
			return *stringStreamData == '\0' ;
		}
		case listStream: {
			return listStreamData == tNilAtom ;
		}
		case scratchStream: {
			FileError("This operation is not available for scratch streams") ;
			return false ;
		}
		default: return IInternalError("StreamAtEnd") ;
	} 
}

void StreamPutChar(StreamPt srm, int c)
{
	switch( StreamKind(srm) ) {
		case textFileStream:
		case binaryFileStream: {
			if( fputc(c, StreamChannel(srm)) == EOF )
				FileError("Could not  write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case localeTextFileStream: {
			if( StreamLocale(srm) != currentLocale )
				SetLocaleUnsafe(StreamLocale(srm)) ;
				if( c > 255 ) c = '?' ;
				if( fputwc(c, StreamChannel(srm)) == EOF )
				FileError("Could not  write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case bufferStream: {
			BufferPutChar(StreamChannel(srm), c) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		case stringStream: {
			FileError("This operation is not available for string streams") ;
			break ;
		}
		case listStream: {
			FileError("This operation is not available for list streams") ;
			break ;
		}
		case scratchStream: {
			ScratchAddCh(c) ;
			break ;
		}
		default: InternalError("StreamPutChar") ;
	}
}

void StreamPutStr(StreamPt srm, CharPt s)
{
	switch( StreamKind(srm) ) {
		case textFileStream:
		case binaryFileStream: {
			if( fputs(s, StreamChannel(srm)) == EOF )
				FileError("Could not  write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case localeTextFileStream: {
			while( *s )
				StreamPutChar(srm, *s++) ;
			break ;
		}
		case bufferStream: {
			BufferPutStr(StreamChannel(srm), s) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		case stringStream: {
			FileError("This operation is not available for string streams") ;
			break ;
		}
		case listStream: {
			FileError("This operation is not available for list streams") ;
			break ;
		}
		case scratchStream: {
			ScratchAddStr(s) ;
			break ;
		}
		default: InternalError("StreamPutStr") ;
	}
}

void StreamWriteN(StreamPt srm, VoidPt v, Size n)
{
	switch( StreamKind(srm) ) {
		case textFileStream:
		case localeTextFileStream:
		case binaryFileStream: {
			fwrite(v, 1, n, StreamChannel(srm)) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		case stringStream: {
			FileError("This operation is not available for string streams") ;
			break ;
		}
		case listStream: {
			FileError("This operation is not available for list streams") ;
			break ;
		}
		case scratchStream: {
			FileError("This operation is not available for scratch streams") ;
			break ;
		}
		default: InternalError("StreamWriteN") ;
	}
}

void StreamWriteV(StreamPt srm, CharPt fmt, VoidPt v)
{
	if( srm == userErr ) StreamFlush(userOut) ;
	switch( StreamKind(srm) ) {
		case textFileStream:
		case binaryFileStream: {
			if( vfprintf(StreamChannel(srm), fmt, v) < 0 )
				FileError("Could not  write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case localeTextFileStream: {
			wchar_t wfmt[1 K] ;
			if( StreamLocale(srm) != currentLocale )
				SetLocaleUnsafe(StreamLocale(srm)) ;	
			if( swprintf(wfmt, 1 K, L"%s", fmt) < 0 )
				FileError("Bad format string") ;
			if( vfwprintf(StreamChannel(srm), wfmt, v) < 0 )
				FileError("Could not write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case bufferStream: {
			BufferWriteV(StreamChannel(srm), fmt, v) ;
			break ;
		}
		case nullStream: {
			break ;
		}
		case stringStream: {
			FileError("This operation is not available for string streams") ;
			break ;
		}
		case listStream: {
			FileError("This operation is not available for list streams") ;
			break ;
		}
		case scratchStream: {
			ScratchWriteV(fmt, v) ;
			break ;
		}
		default: InternalError("StreamWriteV") ;
	}
}

void StreamFlush(StreamPt srm)
{
	switch( StreamKind(srm) ) {
		case textFileStream:
		case localeTextFileStream:
		case binaryFileStream: {
			fflush(StreamChannel(srm)) ;
			break ;
		}
		case bufferStream:
		case nullStream: {
			break ;
		}
		case stringStream: {
			FileError("This operation is not available for string streams") ;
			break ;
		}
		case listStream: {
			FileError("This operation is not available for list streams") ;
			break ;
		}
		case scratchStream: {
			break ;
		}
		default: InternalError("StreamFlush") ;
	}
}

CharPt StreamClose(StreamPt srm)
{
	CharPt res ;

	if( srm == userIn || srm == userOut || srm == userErr ) return nil ;
 
	switch( StreamKind(srm) ) {
		case textFileStream:
		case localeTextFileStream:
		case binaryFileStream: {
			fclose(StreamChannel(srm)) ;
			ExtraDelete(streamType, srm) ;
			res = nil ;
			break ;
		}
		case bufferStream:
		case nullStream: {
			ExtraDelete(streamType, srm) ;
			res = nil ;
			break ;
		}
		case stringStream: {
			/* Never close the only permanent string stream */
			res = nil ;
			break ;
		}
		case listStream: {
			/* Never close the only permanent list stream */
			res = nil ;
			break ;
		}
		case scratchStream: {
			/* Never close the only permanent scratch stream */
			ScratchAddCh('\0') ;
			res = FreeScratch() ;
			break ;
		}
		default: InternalError("StreamClose") ;
	}

	if( srm == currIn ) currIn = userIn ;
	elif( srm == currOut ) currOut = userOut ;
	return res ;
}


/* INPUT STREAMS */

void See(CharPt name)
{
	currIn = FileStreamOpen(name, mRead, nil) ;
}

void Seen()
{
	StreamClose(currIn) ;
}

int StreamGetNonBlank(StreamPt srm)
{
	int c ;
	while( (c = StreamGetChar(srm)) == ' ' ) ;
	return c ;
}

int StreamPeekNonBlank(StreamPt srm)
{
	int c ;
	while( (c = StreamPeekChar(srm)) == ' ' )
		StreamGetChar(srm) ;
	return c ;
}

CharPt StreamGetLine(StreamPt srm)
{
	register int c ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	c = StreamGetChar(srm) ;
	if( c == eofMark ) return nil ;
	UseScratch() ;
	do {
		ScratchAddCh(c) ;
		c = StreamGetChar(srm) ;
	} while( c != '\n' && c != eofMark ) ;
	ScratchAddCh('\0') ;
	return FreeScratch() ;
}

void GetCharCommand(int *comm, int *arg)
{
	int c, n ;
	Bool hasArg ;
	StreamFlush(userOut) ;
	while( (c = StreamGetChar(userIn)) <= ' ' && c != '\n' && c != eofMark ) ;
	*comm = InRange(c,'A','Z') ? (c - 'A' + 'a') : c ;
	hasArg = 0 ;
	n = 0 ;
	while( c != '\n' && c != eofMark ) {
		if( InRange(c, '0', '9') ) {
			hasArg = true ;
			n = n * 10 + c - '0' ;
		}
		c = StreamGetChar(userIn) ;
	}
	*arg = hasArg ? n : -1 ;
	while( c != '\n' && c != eofMark )
		c = StreamGetChar(userIn) ;
}


/* OUTPUT STREAMS */

void Tell(CharPt name)
{
	currOut = FileStreamOpen(name, mWrite, nil) ;
}

void StreamWrite(StreamPt srm, CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	StreamWriteV(srm, fmt, p) ;
}

void Write(CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	StreamWriteV(currOut, fmt, p) ;
}

void WriteStd(CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	StreamWriteV(userOut, fmt, p) ;
}

void WriteErr(CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	StreamWriteV(userErr, fmt, p) ;
}

void Dot(int c)
{
	StreamPutChar(origOut, c) ;
	StreamFlush(origOut) ;
}

void StreamsSane()
{
	currIn = userIn ;
	currOut = userOut ;
}

static Size FlushAllAux(VoidPt x)
{
	StreamPt srm = cStreamPt(x) ;
	if( StreamMode(srm) == mWrite || StreamMode(srm) == mAppend )
		StreamFlush(srm) ;
	return 0 ;
}
void StreamFlushAll()
{
	ForEachExtra(streamType, FlushAllAux) ;
}


/* CXPROLOG C'BUILTINS */

StreamPt XTestStream(register Pt t, StreamMode mode)
{
	VarValue(t) ;
	if( IsAtom(t) ) {
		if( t == tUserAtom )
			return mode == mRead ? userIn : userOut ;
		t = IVarGet(XAtom(t)) ;
	}
	if( IsThisExtra(streamType, t) )
		return UsingStream(cStreamPt(XPt(t)), mode) ;
	return TypeError2("STREAM", t) ;
}

static void PStreamCheck()
{
	if( XExtraCheck(streamType, X0) ) JumpNext()
	DoFail()
}

static void PSee()
{
	See(XTestAtomName(X0)) ;
	JumpNext()
}

static void PSeeing()
{
	if( UnifyWithAtomic(X0, MakeTempAtom(StreamName(currIn))) ) JumpNext()
	DoFail()
}

static void PSeen()
{
	Seen() ;
	JumpNext()
}

static void PTell()
{
	Tell(XTestAtomName(X0)) ;
	JumpNext()
}

static void PTelling()
{
	if( UnifyWithAtomic(X0, MakeTempAtom(StreamName(currOut))) ) JumpNext()
	DoFail()
}

static void PTold()
{
	StreamClose(currOut) ;
	JumpNext()
}

static void PFlush()
{
	StreamFlush(currOut) ;
	JumpNext()
}

static void PSFlush()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamFlush(srm) ;
	JumpNext()
}

static void PSFlushAll()
{
	StreamFlushAll() ;
	JumpNext()
}

static void POpenFileStream()
{
	BindVarWithExtra(X2, FileStreamOpen(XTestAtomName(X0), XTestStreamMode(X1), XTestAtomName(tTextAtom))) ;
	JumpNext()
}

static void POpenFileStream2()
{
	BindVarWithExtra(X3, FileStreamOpen(XTestAtomName(X0), XTestStreamMode(X1), XTestAtomName(X2))) ;
	JumpNext()
}

static void POpenBufferStream()
{
	BindVarWithExtra(X2, BufferStreamOpen(XTestBuffer(X0), XTestStreamMode(X1))) ;
	JumpNext()
}

static void POpenNull()
{
	BindVarWithExtra(X0, NullStreamOpen()) ;
	JumpNext()
}

static void PClose()
{
	StreamPt srm = XTestStream(X0, mNone) ;
	StreamClose(srm) ;
	JumpNext()
}

static void PSetInput()
{
	currIn = XTestStream(X0, mRead) ;
	JumpNext()
}

static void PCurrentInput()
{
	BindVarWithExtra(X0, currIn) ;
	JumpNext()
}

static void PSetOutput()
{
	currOut = XTestStream(X0, mWrite) ;
	JumpNext()
}

static void PCurrentOutput()
{
	BindVarWithExtra(X0, currOut) ;
	JumpNext()
}

static Bool CurrentStreamHandle(VoidPt x)
{
	register StreamPt srm = cStreamPt(x) ;
	if( StreamKind(srm) == stringStream
		|| StreamKind(srm) == listStream
		|| StreamKind(srm) == scratchStream )
		return false ;
	return UnifyWithAtomic(X0, MakeTempAtom(StreamName(srm)))
	    && UnifyWithAtomic(X1, MakeTempAtom(StreamModeStr(srm))) ;
}
static void PNDCurrentStream()
{
	PNDCurrentExtra(streamType, CurrentStreamHandle, 3) ;
	JumpNext()
}

static void PGet0()
{
 	int c = StreamGetChar(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X0, t) ) JumpNext()
    DoFail()
}

static void PSGet0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
 	int c = StreamGetChar(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X1, t) ) JumpNext()
    DoFail()
}

static void PGet()
{
 	int c = StreamGetNonBlank(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X0, t) ) JumpNext()
    DoFail()
}

static void PSGet()
{
	StreamPt srm = XTestStream(X0, mRead) ;
 	int c = StreamGetNonBlank(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X1, t) ) JumpNext()
    DoFail()
}

static void PPeek0()
{
 	int c = StreamPeekChar(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X0, t) ) JumpNext()
    DoFail()
}

static void PSPeek0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
 	int c = StreamPeekChar(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X1, t) ) JumpNext()
    DoFail()
}

static void PPeek()
{
 	int c = StreamPeekNonBlank(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X0, t) ) JumpNext()
    DoFail()
}

static void PSPeek()
{
	StreamPt srm = XTestStream(X0, mRead) ;
 	int c = StreamPeekNonBlank(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	if( UnifyWithNumber(X1, t) ) JumpNext()
    DoFail()
}

static void PSkip()
{
	int c = StreamIsBinary(currIn) ? XTestCode(X0) : XTestByte(X0) ;
	while( StreamGetChar(currIn) != c ) ;
	JumpNext()
}

static void PSSkip()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamIsBinary(srm) ? XTestCode(X1) : XTestByte(X1) ;
	while( StreamGetChar(srm) != c ) ;
	JumpNext()
}

static void PGetLine()
{
	CharPt str = StreamGetLine(currIn) ;
	if( str == nil ) {
		if( UnifyWithAtomic(X0, MakeCode(eofMark)) ) JumpNext()
		DoFail()
	}
	else {
		if( UnifyWithAtomic(X0, MakeTempAtom(str)) ) JumpNext()
		DoFail()
	}
}

static void PSGetLine()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	CharPt str = StreamGetLine(srm) ;
	if( str == nil ) {
		if( UnifyWithAtomic(X1, MakeCode(eofMark)) ) JumpNext()
		DoFail()
	}
	else {
		if( UnifyWithAtomic(X1, MakeTempAtom(str)) ) JumpNext()
		DoFail()
	}
}

static void PPut()
{
	int c = StreamIsBinary(currOut) ? XTestCode(X0) : XTestByte(X0) ;
    StreamPutChar(currOut, c) ;
	JumpNext()
}

static void PSPut()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	int c = StreamIsBinary(srm) ? XTestCode(X1) : XTestByte(X1) ;
    StreamPutChar(srm, c) ;
 	JumpNext()
}

static void PNl()
{
	StreamPutChar(currOut, '\n') ;
	JumpNext()
}

static void PSNl()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamPutChar(srm, '\n') ;
	JumpNext()
}

static void PTab()
{
	Size n = XTestInt(X0) ; ;
	while( n-- )
		StreamPutChar(currOut, ' ') ;
	JumpNext()
}

static void PSTab()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	Size n = XTestInt(X1) ; ;
	while( n-- )
		StreamPutChar(srm, ' ') ;
	JumpNext()
}

static void PSGetBlock()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	BufferPt b = XTestBuffer(X1) ;
	PInt n = XTestPosInt(X2) ;
	if( StreamKind(srm) != binaryFileStream )
		FileError("This operation is only available for binary file streams") ;
	BufferRewrite(b) ;
	BufferEnsureSpace(b, n) ;
	n = fread(BufferContents(b), 1, n, StreamChannel(srm)) ;
	BufferSetSizeUnsafe(b, n) ;
	JumpNext()
}

static void PSPutBlock()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	BufferPt b = XTestBuffer(X1) ;
	if( StreamKind(srm) != binaryFileStream )
		FileError("This operation is only available for binary file streams") ;
	fwrite(BufferContents(b), 1, BufferSize(b), StreamChannel(srm)) ;
	JumpNext()
}

static void PSetStdStreams()
{
	StreamPt s0 = XTestStream(X0, mRead) ;
	StreamPt s1 = XTestStream(X1, mWrite) ;
	StreamPt s2 = XTestStream(X2, mWrite) ;
	
	if( currIn == userIn ) currIn = s0 ;
	if( currOut == userOut ) currOut = s1 ;
	if( currOut == userErr ) currOut = s2 ;

	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	userIn = s0 ;
	userOut = s1 ;
	userErr = s2 ;
	SetStdStreamsIVars() ;
	JumpNext()
}

static void PRestoreStdStreams()
{
	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	userIn = origIn ;
	userOut = origOut ;
	userErr = origErr ;
	SetStdStreamsIVars() ;
	JumpNext()
}

static Size PStreamsAux(VoidPt x)
{
	register StreamPt srm = cStreamPt(x) ;
	AtomPt at ;
	if( StreamKind(srm) == stringStream
		|| StreamKind(srm) == listStream
		|| StreamKind(srm) == scratchStream )
		return 0 ;
	Write(" %s %16.16s -> %6s, %s%s",
			(srm == currIn || srm == currOut) ? "CURR" : "    ",
			StreamName(srm),
			StreamModeStr(srm),
			XExtraAsStr(TagExtra(srm)),
			StreamAtEnd(srm) ? " [at EOF]" : ""
	) ;
	at = IVarWith(TagExtra(srm)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
	return 0 ;
}
static void PStreams()
{
	VersionShow() ;
	Write("Streams:\n") ;
	ForEachExtra(streamType, PStreamsAux) ;
	JumpNext()
}


/* INIT */

void StreamsInit2()
{
	SetLocaleInit2() ;
	
	tReadAtom = MakeAtom("read") ;
	tWriteAtom = MakeAtom("write") ;
	tAppendAtom = MakeAtom("append") ;
	tTextAtom = MakeAtom("text") ;
	tBinaryAtom = MakeAtom("binary") ;

	SetStdStreamsIVars() ;

	InstallCBuiltinPred("stream", 1, PStreamCheck) ;

	InstallCBuiltinPred("see", 1, PSee) ;
	InstallCBuiltinPred("seeing", 1, PSeeing) ;
	InstallCBuiltinPred("seen", 0, PSeen) ;

	InstallCBuiltinPred("tell", 1, PTell) ;
	InstallCBuiltinPred("telling", 1, PTelling) ;
	InstallCBuiltinPred("told", 0, PTold) ;
	InstallCBuiltinPred("flush", 0, PFlush) ;
	InstallCBuiltinPred("flush", 1, PSFlush) ;
	InstallCBuiltinPred("flushall", 0, PSFlushAll) ;

	InstallCBuiltinPred("open", 3, POpenFileStream) ;
	InstallCBuiltinPred("open", 4, POpenFileStream2) ;
	InstallCBuiltinPred("open_buffer_stream", 3, POpenBufferStream) ;
	InstallCBuiltinPred("open_null_stream", 1, POpenNull) ;
	InstallCBuiltinPred("close", 1, PClose) ;
	InstallCBuiltinPred("set_input", 1, PSetInput) ;
	InstallCBuiltinPred("current_input", 1, PCurrentInput) ;
	InstallCBuiltinPred("set_output", 1, PSetOutput) ;
	InstallCBuiltinPred("current_output", 1, PCurrentOutput) ;

	InstallNDeterCBuiltinPred("current_stream", 3, PNDCurrentStream) ;

	InstallCBuiltinPred("get0", 1, PGet0) ;
	InstallCBuiltinPred("get0", 2, PSGet0) ;
	InstallCBuiltinPred("get", 1, PGet) ;
	InstallCBuiltinPred("get", 2, PSGet) ;
	InstallCBuiltinPred("peek0", 1, PPeek0) ;
	InstallCBuiltinPred("peek0", 2, PSPeek0) ;
	InstallCBuiltinPred("peek", 1, PPeek) ;
	InstallCBuiltinPred("peek", 2, PSPeek) ;
	InstallCBuiltinPred("skip", 1, PSkip) ;
	InstallCBuiltinPred("skip", 2, PSSkip) ;
	InstallCBuiltinPred("get_line", 1, PGetLine) ;
	InstallCBuiltinPred("get_line", 2, PSGetLine) ;

	InstallCBuiltinPred("put", 1, PPut) ;
	InstallCBuiltinPred("put", 2, PSPut) ;
	InstallCBuiltinPred("nl", 0, PNl) ;
	InstallCBuiltinPred("nl", 1, PSNl) ;
	InstallCBuiltinPred("tab", 1, PTab) ;
	InstallCBuiltinPred("tab", 2, PSTab) ;

	InstallCBuiltinPred("get_block", 3, PSGetBlock) ;
	InstallCBuiltinPred("put_block", 2, PSPutBlock) ;

	InstallCBuiltinPred("set_user_streams", 3, PSetStdStreams) ;
	InstallCBuiltinPred("restore_user_streams", 0, PRestoreStdStreams) ;

	InstallCBuiltinPred("streams", 0, PStreams) ;
}

void StreamsInit()
{
	SetLocaleInit() ;
	streamType = ExtraTypeNew("stream", WordsOf(Stream)) ;
	currIn = userIn = origIn =
		StreamNew(stdin, "_%stdin", mRead, localeTextFileStream) ;
	currOut = userOut = origOut =
		StreamNew(stdout, "_%stdout", mWrite, localeTextFileStream) ;
	userErr = origErr =
		StreamNew(stderr, "_%stderr", mWrite, localeTextFileStream) ;
}
