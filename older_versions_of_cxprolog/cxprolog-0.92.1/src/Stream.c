/*
 *   This file is part of the CxProlog system

 *   Stream.c
 *   by A.Miguel Dias - 1989/12/02
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

#if USE_WIDECHARS
#include <wchar.h>
#define MyEOF						WEOF
#define MyUseLocale(l)				UseLocale(l)
#define MyGetChar(f)				fgetwc(f)
#define MyUngetChar(c,f)			ungetwc(c,f)
#define MyGetByte(f)				fgetc(f)
#define MyUngetByte(c,f)			ungetc(c,f)
#define MyPutChar(c,f)				fputwc(c,f)
#define MyPutByte(c,f)				fputc(c,f)
#define MySetNoOrientation(srm,m)	SetNoOrientation(srm, m)
#else
#define MyEOF						EOF
#define MyUseLocale(l)				/* nothing */
#define MyGetChar(f)				fgetc(f)
#define MyUngetChar(c,f)			ungetc(c,f)
#define MyGetByte(f)				fgetc(f)
#define MyUngetByte(c,f)			ungetc(c,f)
#define MyPutChar(c,f)				fputc(c,f)
#define MyPutByte(c,f)				fputc(c,f)
#define MySetNoOrientation(srm,m)	/* nothing */
#endif

static ExtraTypePt streamType ;
static StreamPt origIn, origOut, origErr ;
StreamPt userIn, userOut, userErr ;
StreamPt currIn, currOut ;

static Pt tReadAtom, tWriteAtom, tAppendAtom, tTextAtom, tBinaryAtom ;
static CharPt stringStreamData ;
static Pt listStreamData ;
static va_list _p ;	


/* PRIVATE FUNCTIONS */

static StreamPt StreamNew(VoidPt chn, CharPt n, StreamMode m, StreamKind k)
{
	StreamPt srm = ExtraNew(streamType) ;
	StreamChannel(srm) = chn ;
	StreamLocale(srm) = DefaultLocale() ;
	StreamMode(srm) = m ;
	StreamKind(srm) = k ;
	StreamIsBinary(srm) = false ;
	StreamIsEdinburgh(srm) = false ;
	StreamAtom(srm) = LookupTempAtom(n) ;	
	StreamPath(srm) =
		origErr == nil || !IsFileStream(srm)
				? StreamAtom(srm)
				: AbsoluteFileName(StreamAtom(srm)) ;
	return srm ;
}

static void SetInteractive(StreamPt srm)
{
	if( StreamKind(srm) == textFileStream )
		StreamKind(srm) = textFileStreamInteractive ;
	else InternalError("SetInteractive") ;
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

static Bool FindFileStreamAux(VoidPt x, VoidPt name)
{
	return StreamIsEdinburgh(cStreamPt(x))
		&& StrEqual(StreamName(cStreamPt(x)), cCharPt(name)) ;
}
static StreamPt FindEdinburghStream(CharPt name, StreamMode mode)
{
	StreamPt res ;
	if( name[0] == '\0' )
		FileError("'' is an invalid file name") ;
	if( StrEqualN(name, "user", 4) ) {
		if( StrEqual(name, "user") )
			return mode == mRead ? userIn : userOut ;
		if( StrEqual(name, "user_input") )
			return userIn ;
		if( StrEqual(name, "user_output") )
			return userOut ;
		if( StrEqual(name, "user_error") )
			return userErr ;
	}
	if( (res = ExtraFindFirst(streamType, FindFileStreamAux, name)) != nil )
		return UsingStream(res, mode) ;
	return nil ;
}

static void SetStdStreamsIVars()
{
	IVarForceSet(LookupAtom("user_input"), TagExtra(userIn), true) ;
	IVarForceSet(LookupAtom("user_output"), TagExtra(userOut), true) ;
	IVarForceSet(LookupAtom("user_error"), TagExtra(userErr), true) ;
}

static Size StreamsBasicGCMarkAux(VoidPt x)
{
	StreamPt srm = cStreamPt(x) ;
	AtomGCMark(StreamAtom(srm)) ;
	AtomGCMark(StreamPath(srm)) ;
	return 1 ;
}
static void StreamsBasicGCMark()
{
	ExtraForEach(streamType, StreamsBasicGCMarkAux) ;
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


/* OPEN & CLOSE OPERATIONS */

StreamPt FileStreamOpen(CharPt name, StreamMode mode, CharPt locale)
/* values for 'locale':
	nil - edinburgh-style text stream using the default char encoding
	'text' - text stream using the default char encoding
	'binary' - binary stream
	other - text stream using the specified char encoding */
{
	StreamPt srm ;
	FILE *file ;
	Str32 m ;
	Bool isEdinburgh = locale == nil ;	
	Bool isBinary = locale != nil && StrEqual(locale, "binary") ;

/* check existence if edinburgh-style stream */
	if( isEdinburgh && (srm = FindEdinburghStream(name, mode)) != nil )
		return srm ;

/* open file */
	strcpy(m, mode==mWrite ? "w" : mode==mRead ? "r" : "a") ;
	if( isBinary ) strcat(m, "b") ;
	if( (file = fopen(StrExternalize(name), m)) == nil )
		FileError("Cannot open file '%s'", name) ;

/* setup descriptor */
	srm = StreamNew(file, name, mode,
					isBinary ? binaryFileStream : textFileStream) ;
	StreamIsEdinburgh(srm) = isEdinburgh ;
	if( isBinary )
		StreamIsBinary(srm) = true ;
 	elif( locale != nil && !StrEqual(locale, "text") )
		StreamLocale(srm) = MakeLocaleCType(locale) ;
	return srm ;
}

StreamPt BufferStreamOpen(BufferPt buff, StreamMode mode, CharPt locale)
{
	StreamPt srm ;
	CharPt name = GStrFormat("_%%buffer_%lx", cWord(buff)) ;
	Bool isBinary = locale != nil && StrEqual(locale, "binary") ;

/* open buffer */
	if( mode == mWrite ) BufferRewrite(buff) ;
	elif( mode == mRead ) BufferReset(buff) ;
	elif( mode == mAppend ) BufferAppend(buff) ;
	else InternalError("BufferStreamOpen") ;

/* setup descriptor */
	srm = StreamNew(buff, name, mode,
					isBinary ? binaryBufferStream : textBufferStream) ;
	StreamIsEdinburgh(srm) = false ;
	if( isBinary )
		StreamIsBinary(srm) = true ;
	elif( locale != nil && !StrEqual(locale, "text") )
		StreamLocale(srm) = MakeLocaleCType(locale) ;
	return srm ;
}

StreamPt NullStreamOpen()
{
	static int n = 0 ;
	CharPt name = GStrFormat("_%%null_%d", n++) ;
	StreamPt srm = StreamNew(nil, name, mWrite, nullStream) ;
	return srm ;
}

StreamPt StringStreamOpen(CharPt string)
{
	static StreamPt srm = nil ;
	if( srm == nil ) { /* There is only one string stream */
		srm = StreamNew(nil, "_%string", mRead, stringStream) ;	
		ExtraSetHidden(srm) ;
		ExtraSetPermanent(srm) ;
	}
	stringStreamData = string ;
	return srm ;
}

StreamPt ListStreamOpen(Pt list)
{
	static StreamPt srm = nil ;
	if( srm == nil ) { /* There is only one  stream */
		srm = StreamNew(nil, "_%list", mRead, listStream) ;
		ExtraSetHidden(srm) ;
		ExtraSetPermanent(srm) ;
	}
	listStreamData = Drf(list) ;
	return srm ;
}

StreamPt InnerStreamOpen()
{
	static StreamPt srm = nil ;
	if( srm == nil ) { /* There is only one inner stream */
		srm = StreamNew(nil, "_%inner", mWrite, innerStream) ;
		ExtraSetHidden(srm) ;
		ExtraSetPermanent(srm) ;
	}
	BigStrOpen() ;
	return srm ;
}

StreamPt FILEToStream(FILE *file, StreamMode mode, CharPt prefName)
{
	CharPt name = GStrFormat("%s_%lx", prefName, cWord(file)) ;
	StreamPt srm = StreamNew(file, name, mode, textFileStream) ;
	if( mode == mRead ) SetInteractive(srm) ;
	return srm ;
}

CharPt StreamClose(StreamPt srm)
{
	CharPt res = nil ; /* keep nil here */

	if( srm == userIn || srm == userOut || srm == userErr ) return nil ;
 
	switch( StreamKind(srm) ) {
		case textFileStream:
		case textFileStreamInteractive:
		case binaryFileStream: {
			fclose(StreamChannel(srm)) ;
			ExtraDelete(streamType, srm) ;
			break ;
		}
		case textBufferStream:
		case binaryBufferStream:
		case nullStream: {
			ExtraDelete(streamType, srm) ;
			break ;
		}
		case stringStream: {
			/* Never delete the only permanent string stream */
			break ;
		}
		case listStream: {
			/* Never delete the only permanent list stream */
			break ;
		}
		case innerStream: {
			/* Never delete the only permanent inner stream */
			res = BigStrClose() ;
			break ;
		}
		default: InternalError("StreamClose") ;
	}

	if( srm == currIn ) currIn = userIn ;
	elif( srm == currOut ) currOut = userOut ;
	return res ;
}


/* BASIC INPUT OPERATIONS */

int StreamAccess(StreamPt srm, Bool advance)
{
	register int c ;
	switch( StreamKind(srm) ) {
		case textFileStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			for(;;) {
				if( (c = MyGetChar(StreamChannel(srm))) == MyEOF ) {
#ifdef EILSEQ
					if( errno == EILSEQ )
						FileError("Wide character conversion error") ;
#endif
					return eofMark ;
				}
				if( c == 0 ) continue ;
				if( !CharValidCode(c) ) c = '?' ;
				return advance ? c : MyUngetChar(c, StreamChannel(srm)) ;
			}
		}
		case textFileStreamInteractive: {
			MyUseLocale(StreamLocale(srm)) ;	
			for(;;) { /* this works even with raw input or sockets */
				AllowInterruptibleSysCalls() ;
				c = MyGetChar(StreamChannel(srm)) ;
				DisallowInterruptibleSysCalls() ;
				if( c == MyEOF ) {
#ifdef EILSEQ
					if( errno == EILSEQ )
						FileError("Wide character conversion error") ;
#endif
					if( InterruptHandle() ) continue ;
					return eofMark ;
				}
				if( c < ' ' && c != '\n' ) {
					if( c == 0 ) continue ;
					elif( c == 10 || c == 13 ) {
						if( !advance ) MyUngetChar(c, StreamChannel(srm)) ;
						return '\n' ;
					}
					elif( c == 4 || c == 26 ) { /* CNTL-D, CNTL-Z */
						if( !advance ) MyUngetChar(c, StreamChannel(srm)) ;
						return eofMark ;
					}
				}
				if( !CharValidCode(c) ) c = '?' ;
				return advance ? c : MyUngetChar(c, StreamChannel(srm)) ;
			}
		}
		case binaryFileStream: {
			if( (c = MyGetByte(StreamChannel(srm))) == EOF )
				return -1 ;  /* eof is hardwired nn binary streams */
			return advance ? c : MyUngetByte(c, StreamChannel(srm)) ;
		}
		case textBufferStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			c = BufferAccessChar(StreamChannel(srm), advance) ;
			if( !CharValidCode(c) ) c = '?' ;
			return c ;
		}
		case binaryBufferStream: {
			return BufferAccessByte(StreamChannel(srm), advance) ;
		}
		case nullStream: {
			FileError("This operation is not available for null streams") ;
			return 0 ;
		}
		case stringStream: {
			if( *stringStreamData == '\0' ) return eofMark ;
			if( advance ) return *stringStreamData++ ;
			else return *stringStreamData ;;
		}
		case listStream: {
			if( listStreamData == tNilAtom ) return eofMark ;
			elif( IsList(listStreamData) ) {
				Pt t = Drf(XListHead(listStreamData)) ;
				if( advance )
					listStreamData = Drf(XListTail(listStreamData)) ;
				return XTestCode(t) ;
			}
			else TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
		}
		case innerStream: {
			FileError("This operation is not available for inner streams") ;
			return 0 ;
		}		
		default: return IInternalError("StreamAccess") ;
	}
}

int StreamGetSingleChar(void)
{
	if( SetRawInput(userIn) ) {
		int c = StreamGet(userIn) ;
 		UnsetRawInput() ;
		return c ;
	}
	else {
		CharPt s = StreamGetLine(userIn) ;
		while( *s == ' ' ) s++ ;
		return *s == '\0' ? 10 : *s ;
	}
}

#if USE_WIDECHARS
static void SetNoOrientation(StreamPt srm, CharPt mode)
{
	if( *mode == 'r' ) {
		long pos  ;
		if( (pos = ftell(StreamChannel(srm))) == -1 )
			FileError("Cannot ftell file '%s'", StreamName(srm)) ;
		if( freopen(nil, mode, StreamChannel(srm)) == nil )
			FileError("Cannot unset orientation of file '%s'", StreamName(srm)) ;
		if( fseek(StreamChannel(srm), pos, SEEK_SET) != 0 )
			FileError("Cannot fseek on file '%s'", StreamName(srm)) ;
	}
	else
		if( freopen(nil, mode, StreamChannel(srm)) == nil )
			FileError("Cannot unset orientation of file '%s'", StreamName(srm)) ;
}
#endif

Size StreamReadBytes(StreamPt srm, VoidPt v, Size n)
{
	switch( StreamKind(srm) ) {
		case textFileStream:
		case textFileStreamInteractive: {
			Size r ;
			MySetNoOrientation(srm, "rb") ;
			r = fread(v, 1, n, StreamChannel(srm)) ;
			MySetNoOrientation(srm, "r") ;
			return r ;
		}
		case binaryFileStream: {
			return fread(v, 1, n, StreamChannel(srm)) ;
		}
		case textBufferStream:
		case binaryBufferStream: {
			return BufferReadN(StreamChannel(srm), v, n) ;
		}
		case nullStream: {
			return 0 ;
		}
		default: {
			FileError("This operation is not available for this kind of stream") ;
			return -1 ;
		}
	}
}

Bool StreamAtEnd(StreamPt srm)
{
	if( StreamMode(srm) != mRead ) return false ;
		
 	switch( StreamKind(srm) ) {
		case textFileStream:
		case textFileStreamInteractive:
		case binaryFileStream: {
			return feof((FILE *)StreamChannel(srm)) ; /* VS requires (FILE *) */
		}
		case textBufferStream: {
		case binaryBufferStream:
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
		case innerStream: {
			FileError("This operation is not available for inner streams") ;
			return false ;
		}
		default: return IInternalError("StreamAtEnd") ;
	} 
}


/* BASIC OUTPUT OPERATIONS */

void StreamPut(StreamPt srm, int c)
{
	switch( StreamKind(srm) ) {
		case textFileStream: {
			MyUseLocale(StreamLocale(srm)) ;
			if( MyPutChar(c, StreamChannel(srm)) == MyEOF )
				FileError("Could not write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case binaryFileStream: {
			if( MyPutByte(c, StreamChannel(srm)) == EOF )
				FileError("Could not write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case textBufferStream: {
			MyUseLocale(StreamLocale(srm)) ;
			BufferPutChar(StreamChannel(srm), c) ;
			break ;
		}
		case binaryBufferStream: {
			BufferPutByte(StreamChannel(srm), c) ;
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
		case innerStream: {
			BigStrAddChar(c) ;
			break ;
		}
		default: InternalError("StreamPut") ;
	}
}

void StreamPutStr(StreamPt srm, CharPt s)
{
	switch( StreamKind(srm) ) {
		case textFileStream: {
			MyUseLocale(StreamLocale(srm)) ;
			while( *s )
				if( MyPutChar(CharDecode(s), StreamChannel(srm)) == MyEOF )
					FileError("Could not write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case binaryFileStream: {
			if( fputs(s, StreamChannel(srm)) == EOF )
				FileError("Could not write on stream '%s'", StreamName(srm)) ;
			break ;
		}
		case textBufferStream: {
			MyUseLocale(StreamLocale(srm)) ;
			while( *s )
				BufferPutChar(StreamChannel(srm), CharDecode(s)) ;
			break ;
		}
		case binaryBufferStream: {
			while( *s )
				BufferPutByte(StreamChannel(srm), *s++) ;
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
		case innerStream: {
			BigStrAddStr(s) ;
			break ;
		}
		default: InternalError("StreamPutStr") ;
	}
}

void StreamPutStrNl(StreamPt srm, CharPt s)
{
	StreamPutStr(srm, s) ;
	StreamPut(srm, '\n') ;
}

void StreamPutStrMulti(StreamPt srm, ...)
{
	va_list p ;
	CharPt s ;
	va_start(p, srm) ;
	while( (s = va_arg(p, CharPt)) != nil )
		StreamPutStr(srm, s) ;
    va_end(p);
}

void StreamPutStrSegm(StreamPt srm, CharPt s, CharPt end)
{
	Char save = *end ;
	*end = '\0' ;
	StreamPutStr(srm, s) ;
	*end = save ;
}

Size StreamWriteBytes(StreamPt srm, VoidPt v, Size n)
{
	switch( StreamKind(srm) ) {
		case textFileStream: {
			Size r ;
			MySetNoOrientation(srm, "ab") ;
			r = fwrite(v, 1, n, StreamChannel(srm)) ;
			MySetNoOrientation(srm, "a") ;
			return r ;
		}
		case binaryFileStream: {
			return fwrite(v, 1, n, StreamChannel(srm)) ;
		}
		case textBufferStream:
		case binaryBufferStream: {
			return BufferWriteN(StreamChannel(srm), v, n) ;
		}
		case nullStream: {
			return 0 ;
		}
		default:
			FileError("This operation is not available for this kind of stream") ;
			return -1 ;
	}
}

void StreamWriteV(StreamPt srm, CharPt fmt, va_list v)
{
	if( srm == nil ) {
		/* Emergency writting.
		     Streams have not initialized yet.
		     Also the memory manager may have not been initialized yet. */
		vfprintf(stderr, fmt, v) ;
		freopen(nil, "a", stderr) ;	/* Set no-orientation on stderr */
	}
	else {
		CharPt s = GStrFormatV(fmt, v) ;		
		if( srm == userErr ) {
			StreamFlush(userOut) ;
			StreamPutStr(srm, s) ;
			if( sysTrace_flag > 0 )
				SysTraceWrite(s) ;
			StreamFlush(userErr) ;
		}
		else StreamPutStr(srm, s) ;
	}
}

void StreamFlush(StreamPt srm)
{
	switch( StreamKind(srm) ) {
		case textFileStream:
		case binaryFileStream: {
			fflush(StreamChannel(srm)) ;
			break ;
		}
		case textBufferStream:
		case binaryBufferStream:
		case nullStream: {
			break ;
		}
		case textFileStreamInteractive: {
			FileError("This operation is not available for interactive streams") ;
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
		case innerStream: {
			break ;
		}
		default: InternalError("StreamFlush") ;
	}
}


/* MORE INPUT OPERATIONS */

int StreamGetNonBlank(StreamPt srm)
{
	int c ;
	while( (c = StreamGet(srm)) == ' ' ) ;
	return c ;
}

int StreamPeekNonBlank(StreamPt srm)
{
	int c ;
	while( (c = StreamPeek(srm)) == ' ' )
		StreamGet(srm) ;
	return c ;
}

CharPt StreamGetLine(StreamPt srm)
{
	register int c = StreamGet(srm) ;
	if( c == eofMark ) return nil ;
	GStrOpen() ;
	while( c != '\n' && c != eofMark ) {
		GStrAddChar(c) ;
		c = StreamGet(srm) ;
	}
	return GStrClose() ;
}

void GetCharCommand(int *comm, int *arg)
{
	int c, n ;
	Bool hasArg ;
	InterruptOff() ;
	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	while( (c = StreamGet(userIn)) <= ' ' && c != '\n' && c != eofMark ) ;
	*comm = InRange(c,'A','Z') ? (c - 'A' + 'a') : c ;
	hasArg = 0 ;
	n = 0 ;
	while( c != '\n' && c != eofMark ) {
		if( InRange(c, '0', '9') ) {
			hasArg = true ;
			n = n * 10 + c - '0' ;
		}
		c = StreamGet(userIn) ;
	}
	*arg = hasArg ? n : -1 ;
	while( c != '\n' && c != eofMark )
		c = StreamGet(userIn) ;
	InterruptOn() ;
}


/* MORE OUTPUT OPERATIONS */

void StreamWrite(StreamPt srm, CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	StreamWriteV(srm, fmt, _p) ;
}

void Write(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	StreamWriteV(currOut, fmt, _p) ;
}

void WriteStd(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	StreamWriteV(userOut, fmt, _p) ;
}

void WriteErr(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	StreamWriteV(userErr, fmt, _p) ;
}

void Dot(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	StreamWriteV(userErr, fmt, _p) ;
}

static Size FlushAllAux(VoidPt x)
{
	StreamPt srm = cStreamPt(x) ;
	if( StreamMode(srm) == mWrite || StreamMode(srm) == mAppend )
		StreamFlush(srm) ;
	return 1 ;
}
void StreamFlushAll()
{
	ExtraForEach(streamType, FlushAllAux) ;
}


/* CXPROLOG C'BUILTINS */

static void PStreamCheck()
{
	MustBe( XExtraCheck(streamType, X0) ) ;
}

static void POpenFileStream3()
{
	BindVarWithExtra(X2, FileStreamOpen(XTestFileName(X0), XTestStreamMode(X1), XTestAtomName(tTextAtom))) ;
	JumpNext() ;
}

static void POpenFileStream4()
{
	BindVarWithExtra(X3, FileStreamOpen(XTestFileName(X0), XTestStreamMode(X1), XTestAtomName(X2))) ;
	JumpNext() ;
}

static void POpenBufferStream3()
{
	BindVarWithExtra(X2, BufferStreamOpen(XTestBuffer(X0), XTestStreamMode(X1), XTestAtomName(tTextAtom))) ;
	JumpNext() ;
}

static void POpenBufferStream4()
{
	BindVarWithExtra(X3, BufferStreamOpen(XTestBuffer(X0), XTestStreamMode(X1), XTestAtomName(X2))) ;
	JumpNext() ;
}

static void POpenNull()
{
	BindVarWithExtra(X0, NullStreamOpen()) ;
	JumpNext() ;
}

static void PClose()
{
	StreamPt srm = XTestStream(X0, mNone) ;
	StreamClose(srm) ;
	JumpNext() ;
}

static Bool CurrentStreamHandle(VoidPt x)
{
	register StreamPt srm = cStreamPt(x) ;
	return UnifyWithAtomic(X0, MakeTempAtom(StreamName(srm)))
		&& UnifyWithAtomic(X1, MakeTempAtom(StreamModeStr(srm))) ;
}
static void PNDCurrentStream3()
{
	ExtraPNDCurrent(streamType, CurrentStreamHandle, 3, 2) ;
	JumpNext() ;
}

static Bool CurrentStreamHandle4(VoidPt x)
{
	StreamPt srm = cStreamPt(x) ;
	Pt t = IsAbsoluteFileName(StreamPath(srm))
				? ZPushTerm(PathNameListFromAtom(StreamPath(srm)))
				: TagAtom(StreamPath(srm)) ;
	return UnifyWithAtomic(X0, TagAtom(StreamAtom(srm)))
		&& UnifyWithAtomic(X1, MakeTempAtom(StreamModeStr(srm)))
		&& Unify(X2, t) ; 
}
static void PNDCurrentStream4()
{
	ExtraPNDCurrent(streamType, CurrentStreamHandle4, 4, 3) ;
	JumpNext() ;
}

static Size PStreamsAux(VoidPt x)
{
	register StreamPt srm = cStreamPt(x) ;
	AtomPt at = IVarWith(TagExtra(srm)) ;
	Write("%16.16s %s> %s, %s, <%s>",
			StreamName(srm),
			(srm == currIn || srm == currOut) ? "=" : "-",
			StreamModeStr(srm),
			XExtraAsStr(TagExtra(srm)),
			StreamLocale(srm)
	) ;
	if( at != nil )
		Write(", (%s)", AtomName(at)) ;
	if( StreamAtEnd(srm) )
		Write(", [at EOF]") ;
	Write(", %s", AtomName(StreamPath(srm))) ;
	Write("\n") ;	
	return 1 ;
}
static void PStreams()
{
	ExtraShow(streamType, PStreamsAux) ;
	JumpNext() ;
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
	JumpNext() ;
}

static void PRestoreStdStreams()
{
	if( currIn == userIn ) currIn = origIn ;
	if( currOut == userOut ) currOut = origOut ;
	if( currOut == userErr ) currOut = origErr ;

	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	userIn = origIn ;
	userOut = origOut ;
	userErr = origErr ;
	SetStdStreamsIVars() ;
	JumpNext() ;
}

static void PSetInput()
{
	currIn = XTestStream(X0, mRead) ;
	JumpNext() ;
}

static void PCurrentInput()
{
	BindVarWithExtra(X0, currIn) ;
	JumpNext() ;
}

static void PSetOutput()
{
	currOut = XTestStream(X0, mWrite) ;
	JumpNext() ;
}

static void PCurrentOutput()
{
	BindVarWithExtra(X0, currOut) ;
	JumpNext() ;
}

static void PSee()
{
	register Pt t ;
	VarValue2(t, X0) ;
	if( IsAtom(t) )
		currIn = FileStreamOpen(XTestFileName(t), mRead, nil) ;
	elif( IsThisExtra(streamType, t) )
		currIn = XTestStream(t, mRead) ;
	else ExtraTypeError(streamType, "FILENAME", t) ;
	JumpNext() ;
}

static void PSeeing()
{
	if( currIn == userIn )
		MustBe( UnifyWithAtomic(X0, tUserAtom ) ) ;
	elif( oldFashionedSeeing_flag )
 		MustBe( UnifyWithAtomic(X0, MakeTempAtom(StreamName(currIn))) ) ;
	else {
		BindVarWithExtra(X0, currIn) ;
		JumpNext() ;
	}
}

static void PSeen()
{
	StreamClose(currIn) ;
	JumpNext() ;
}

static void PTell()
{
	register Pt t ;
	VarValue2(t, X0) ;
	if( IsAtom(t) )
		currOut = FileStreamOpen(XTestFileName(t), mWrite, nil) ;
	elif( IsThisExtra(streamType, t) )
		currOut = XTestStream(t, mWrite) ;
	else ExtraTypeError(streamType, "FILENAME", t) ;
	JumpNext() ;
}

static void PTelling()
{
	if( currOut == userOut )
		MustBe( UnifyWithAtomic(X0, tUserAtom ) ) ;
	elif( oldFashionedSeeing_flag )
 		MustBe( UnifyWithAtomic(X0, MakeTempAtom(StreamName(currOut))) ) ;
	else {
		BindVarWithExtra(X0, currOut) ;
		JumpNext() ;
	}
}

static void PTold()
{
	StreamClose(currOut) ;
	JumpNext() ;
}

static void PGetChar()
{
	int c ;
	if( StreamIsBinary(currIn) )
		FileError("This operation is not available for binary streams") ;
	c = StreamGet(currIn) ;
	MustBe( UnifyWithAtomic(X0, c == eofMark ? tEofAtom : MakeChar(c)) ) ;
}

static void PSGetChar()
{
	int c ;
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	c = StreamGet(srm) ;
	MustBe( UnifyWithAtomic(X1, c == eofMark ? tEofAtom : MakeChar(c)) ) ;
}

static void PGetCode()
{
	if( StreamIsBinary(currIn) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithAtomic(X0, MakeCode(StreamGet(currIn))) ) ;
}

static void PSGetCode()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithAtomic(X1, MakeCode(StreamGet(srm))) ) ;
}

static void PGet()
{
	if( StreamIsBinary(currIn) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithNumber(X0, MakeCode(StreamGetNonBlank(currIn))) ) ;
}

static void PSGet()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithNumber(X1, MakeCode(StreamGetNonBlank(srm))) ) ;
}

static void PGetLine()
{
	CharPt str ;
	if( StreamIsBinary(currIn) )
		FileError("This operation is not available for binary streams") ;
	if( (str = StreamGetLine(currIn)) == nil )
		MustBe( UnifyWithAtomic(X0, MakeCode(eofMark)) ) ;
	else
		MustBe( UnifyWithAtomic(X0, MakeTempAtom(str)) ) ;
}

static void PSGetLine()
{
	CharPt str ;
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	if( (str = StreamGetLine(srm)) == nil )
		MustBe( UnifyWithAtomic(X1, MakeCode(eofMark)) ) ;
	else
		MustBe( UnifyWithAtomic(X1, MakeTempAtom(str)) ) ;
}

static void PGetSingleChar()
{
 	MustBe( UnifyWithNumber(X0, MakeCode((StreamGetSingleChar()))) ) ;
}

static void PPeekChar()
{
	int c ;
	if( StreamIsBinary(currIn) )
		FileError("This operation is not available for binary streams") ;
	c = StreamPeek(currIn) ;
	MustBe( UnifyWithAtomic(X0, c == eofMark ? tEofAtom : MakeChar(c)) ) ;
}

static void PSPeekChar()
{
	int c ;
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	c = StreamPeek(srm) ;
	MustBe( UnifyWithAtomic(X1, c == eofMark ? tEofAtom : MakeChar(c)) ) ;
}

static void PPeekCode()
{
	if( StreamIsBinary(currIn) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithAtomic(X0, MakeCode(StreamPeek(currIn))) ) ;
}

static void PSPeekCode()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithAtomic(X1, MakeCode(StreamPeek(srm))) ) ;
}


static void PPeek()
{
	if( StreamIsBinary(currIn) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithNumber(X0, MakeCode(StreamPeekNonBlank(currIn))) ) ;
}

static void PSPeek()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	MustBe( UnifyWithNumber(X1, MakeCode(StreamPeekNonBlank(srm))) ) ;
}

static void PPutChar()
{
	if( StreamIsBinary(currOut) )
		FileError("This operation is not available for binary streams") ;
	StreamPut(currOut, XTestChar(X0)) ;
	JumpNext() ;
}

static void PSPutChar()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	StreamPut(srm, XTestChar(X1)) ;
	JumpNext() ;
}

static void PPutCode()
{
	if( StreamIsBinary(currOut) )
		FileError("This operation is not available for binary streams") ;
	StreamPut(currOut, XTestCode(X0)) ;
	JumpNext() ;
}

static void PSPutCode()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	StreamPut(srm, XTestCode(X1)) ;
	JumpNext() ;
}

static void PNl()
{
	if( StreamIsBinary(currOut) )
		FileError("This operation is not available for binary streams") ;
	StreamPut(currOut, '\n') ;
	JumpNext() ;
}

static void PSNl()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PTab()
{
	Size n = XTestInt(X0) ; ;
	if( StreamIsBinary(currOut) )
		FileError("This operation is not available for binary streams") ;
	while( n-- )
		StreamPut(currOut, ' ') ;
	JumpNext() ;
}

static void PSTab()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	Size n = XTestInt(X1) ; ;
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
	while( n-- )
		StreamPut(srm, ' ') ;
	JumpNext() ;
}

static void PGetByte()
{
	if( !StreamIsBinary(currIn) )
		FileError("This operation is not available for text streams") ;
	MustBe( UnifyWithAtomic(X0, MakeByte(StreamGet(currIn))) ) ;
}

static void PSGetByte()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( !StreamIsBinary(srm) )
		FileError("This operation is not available for text streams") ;
	MustBe( UnifyWithAtomic(X1, MakeByte(StreamGet(srm))) ) ;
}

static void PPeekByte()
{
	if( !StreamIsBinary(currIn) )
		FileError("This operation is not available for text streams") ;
	MustBe( UnifyWithAtomic(X0, MakeByte(StreamPeek(currIn))) ) ;
}

static void PSPeekByte()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( !StreamIsBinary(srm) )
		FileError("This operation is not available for text streams") ;
	MustBe( UnifyWithAtomic(X1, MakeByte(StreamPeek(srm))) ) ;
}

static void PPutByte()
{
	if( !StreamIsBinary(currOut) && StreamKind(currOut) != nullStream )
		FileError("This operation is not available for text streams") ;
	StreamPut(currOut, XTestByte(X0)) ;
	JumpNext() ;
}

static void PSPutByte()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	if( !StreamIsBinary(srm) && StreamKind(srm) != nullStream )
		FileError("This operation is not available for text streams") ;
	StreamPut(srm, XTestByte(X1)) ;
	JumpNext() ;
}

static void PAtEndOfStream()
{
	MustBe( StreamAtEnd(currIn) ) ;
}

static void PSAtEndOfStream()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	MustBe( StreamAtEnd(srm) ) ;
}

static Bool StreamPropertyHandle(VoidPt x)
{
	register StreamPt srm = cStreamPt(x) ;
	return UnifyWithAtomic(X1, MakeTempAtom(StreamName(srm))) ;
}
static void PNDStreamProperty()
{
	ExtraPNDCurrent(streamType, StreamPropertyHandle, 2, 0) ;
	JumpNext() ;
}

static void PFlushOutput()
{
	StreamFlush(currOut) ;
	JumpNext() ;
}

static void PSFlushOutput()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamFlush(srm) ;
	JumpNext() ;
}

static void PSFlushOutputAll()
{
	StreamFlushAll() ;
	JumpNext() ;
}

static void PGet0()
{
 	int c = StreamGet(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X0, t) ) ;
}

static void PSGet0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
 	int c = StreamGet(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X1, t) ) ;
}

static void PSkip()
{
	int c = StreamIsBinary(currIn) ? XTestByte(X0) : XTestCode(X0) ;
	while( StreamGet(currIn) != c ) ;
	JumpNext() ;
}

static void PSSkip()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamIsBinary(srm) ? XTestByte(X1) : XTestCode(X1) ;
	while( StreamGet(srm) != c ) ;
	JumpNext() ;
}

static void PPeek0()
{
 	int c = StreamPeek(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X0, t) ) ;
}

static void PSPeek0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
 	int c = StreamPeek(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X1, t) ) ;
}

static void PPut()
{
	int c = StreamIsBinary(currOut) ? XTestByte(X0) : XTestCode(X0) ;
	StreamPut(currOut, c) ;
	JumpNext() ;
}

static void PSPut()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	int c = StreamIsBinary(srm) ? XTestByte(X1) : XTestCode(X1) ;
	StreamPut(srm, c) ;
 	JumpNext() ;
}

static void PSGetBlock2()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	BufferPt b = XTestBuffer(X1) ;
	Size n ;
	BufferRewrite(b) ;
	do {
		BufferEnsureSpace(b, BufferSize(b) + 256) ;
		n = StreamReadBytes(srm, BufferContents(b) + BufferSize(b), 256) ;
		BufferSetSizeUnsafe(b, BufferSize(b) + n) ;
	} while( n == 256 ) ;
	JumpNext() ;
}

static void PSGetBlock3()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	BufferPt b = XTestBuffer(X1) ;
	PInt n = XTestPosInt(X2) ;
	BufferRewrite(b) ;
	BufferEnsureSpace(b, n) ;
	n = StreamReadBytes(srm, BufferContents(b), n) ;	/* may read less */
	BufferSetSizeUnsafe(b, n) ;
	JumpNext() ;
}

static void PSPutBlock()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	BufferPt b = XTestBuffer(X1) ;
	if( BufferSize(b) > 0 ) {
		Size n = StreamWriteBytes(srm, BufferContents(b), BufferSize(b)) ;
		if( n == 0 ) FileError("Could not write block") ;
		elif( n < BufferSize(b) ) FileError("Could not write entire block") ;
	}
	JumpNext() ;
}

static void PTTTest()
{
 	int i ;  /* 7.84 - 9.42 - 32.32 */
	StreamPt srm = FileStreamOpen("tttest", mWrite, nil) ;
	dotimes(i, 30000000)
		StreamPut(srm, 'a') ;
	StreamClose(srm) ;
	JumpNext() ;
}

static void PTTTest2()
{
#if 0
#include <readline/readline.h>
#include <readline/history.h>
	CharPt s ;
    s = readline("ola: ") ;
	if( s != nil ) {
    	Mesg(s) ;
    	add_history(s) ;
	}
#endif
	JumpNext() ;
}


/* TEST, EXTRACT & INIT */

void StreamsSane()
{
	currIn = userIn ;
	currOut = userOut ;
}

Bool IsStream(Pt t)
{
	return IsThisExtra(streamType, t) ;
}

StreamPt XTestStream(register Pt t, StreamMode mode)
{
	VarValue(t) ;
	if( IsAtom(t) ) {
		if( t == tUserAtom )
			return mode == mRead ? userIn : userOut ;
		t = IVarGet(XAtom(t), true) ;
	}
	if( IsThisExtra(streamType, t) )
		return UsingStream(cStreamPt(XExtra(t)), mode) ;
	return TypeError2("STREAM", t) ;
}

void StreamsInit2()
{
	tReadAtom = MakeAtom("read") ;
	tWriteAtom = MakeAtom("write") ;
	tAppendAtom = MakeAtom("append") ;
	tTextAtom = MakeAtom("text") ;
	tBinaryAtom = MakeAtom("binary") ;

	SetStdStreamsIVars() ;

	InstallBasicGCHandler(StreamsBasicGCMark) ;
	
	InstallCBuiltinPred("stream", 1, PStreamCheck) ;

	InstallCBuiltinPred("open", 3, POpenFileStream3) ;
	InstallCBuiltinPred("open", 4, POpenFileStream4) ;
	InstallCBuiltinPred("open_buffer_stream", 3, POpenBufferStream3) ;
	InstallCBuiltinPred("open_buffer_stream", 4, POpenBufferStream4) ;
	InstallCBuiltinPred("open_null_stream", 1, POpenNull) ;
	InstallCBuiltinPred("close", 1, PClose) ;
	InstallNDeterCBuiltinPred("current_stream", 3, PNDCurrentStream3) ;
	InstallNDeterCBuiltinPred("current_stream", 4, PNDCurrentStream4) ;
	InstallCBuiltinPred("streams", 0, PStreams) ;
	InstallCBuiltinPred("$set_user_streams", 3, PSetStdStreams) ;
	InstallCBuiltinPred("$restore_user_streams", 0, PRestoreStdStreams) ;

	InstallCBuiltinPred("set_input", 1, PSetInput) ;
	InstallCBuiltinPred("current_input", 1, PCurrentInput) ;
	InstallCBuiltinPred("set_output", 1, PSetOutput) ;
	InstallCBuiltinPred("current_output", 1, PCurrentOutput) ;

	InstallCBuiltinPred("see", 1, PSee) ;
	InstallCBuiltinPred("seeing", 1, PSeeing) ;
	InstallCBuiltinPred("seen", 0, PSeen) ;
	InstallCBuiltinPred("tell", 1, PTell) ;
	InstallCBuiltinPred("telling", 1, PTelling) ;
	InstallCBuiltinPred("told", 0, PTold) ;

	InstallCBuiltinPred("get_char", 1, PGetChar) ;
	InstallCBuiltinPred("get_char", 2, PSGetChar) ;
	InstallCBuiltinPred("get_code", 1, PGetCode) ;
	InstallCBuiltinPred("get_code", 2, PSGetCode) ;
	InstallCBuiltinPred("get", 1, PGet) ;
	InstallCBuiltinPred("get", 2, PSGet) ;
	InstallCBuiltinPred("get_line", 1, PGetLine) ;
	InstallCBuiltinPred("get_line", 2, PSGetLine) ;
	InstallCBuiltinPred("get_single_char", 1, PGetSingleChar) ;
	InstallCBuiltinPred("peek_char", 1, PPeekChar) ;
	InstallCBuiltinPred("peek_char", 2, PSPeekChar) ;
	InstallCBuiltinPred("peek_code", 1, PPeekCode) ;
	InstallCBuiltinPred("peek_code", 2, PSPeekCode) ;
	InstallCBuiltinPred("peek", 1, PPeek) ;
	InstallCBuiltinPred("peek", 2, PSPeek) ;
	InstallCBuiltinPred("put_char", 1, PPutChar) ;
	InstallCBuiltinPred("put_char", 2, PSPutChar) ;
	InstallCBuiltinPred("put_code", 1, PPutCode) ;
	InstallCBuiltinPred("put_code", 2, PSPutCode) ;
	InstallCBuiltinPred("nl", 0, PNl) ;	
	InstallCBuiltinPred("nl", 1, PSNl) ;
	InstallCBuiltinPred("tab", 1, PTab) ;
	InstallCBuiltinPred("tab", 2, PSTab) ;

	InstallCBuiltinPred("get_byte", 1, PGetByte) ;
	InstallCBuiltinPred("get_byte", 2, PSGetByte) ;
	InstallCBuiltinPred("peek_byte", 1, PPeekByte) ;
	InstallCBuiltinPred("peek_byte", 2, PSPeekByte) ;
	InstallCBuiltinPred("put_byte", 1, PPutByte) ;
	InstallCBuiltinPred("put_byte", 2, PSPutByte) ;

	InstallCBuiltinPred("at_end_of_stream", 0, PAtEndOfStream) ;
	InstallCBuiltinPred("at_end_of_stream", 1, PSAtEndOfStream) ;
	InstallNDeterCBuiltinPred("stream_property", 2, PNDStreamProperty) ;
	InstallCBuiltinPred("flush_output", 0, PFlushOutput) ;
	InstallCBuiltinPred("flush_output", 1, PSFlushOutput) ;
	InstallCBuiltinPred("flush_output_all", 0, PSFlushOutputAll) ;
	InstallCBuiltinPred("get0", 1, PGet0) ;
	InstallCBuiltinPred("get0", 2, PSGet0) ;
	InstallCBuiltinPred("skip", 1, PSkip) ;
	InstallCBuiltinPred("skip", 2, PSSkip) ;
	InstallCBuiltinPred("peek0", 1, PPeek0) ;
	InstallCBuiltinPred("peek0", 2, PSPeek0) ;
	InstallCBuiltinPred("put", 1, PPut) ;
	InstallCBuiltinPred("put", 2, PSPut) ;

	InstallCBuiltinPred("get_block", 2, PSGetBlock2) ;
	InstallCBuiltinPred("get_block", 3, PSGetBlock3) ;
	InstallCBuiltinPred("put_block", 2, PSPutBlock) ;

	InstallCBuiltinPred("$_test", 0, PTTTest) ;
	InstallCBuiltinPred("$_test2", 0, PTTTest2) ;
}

void StreamsInit()
{
	streamType = ExtraTypeNew("STREAM", WordsOf(Stream), nil, nil) ;
	currIn = userIn = origIn =
		StreamNew(stdin, "user_input", mRead, textFileStream) ;
	currOut = userOut = origOut =
		StreamNew(stdout, "user_output", mWrite, textFileStream) ;
	userErr = origErr =
		StreamNew(stderr, "user_error", mWrite, textFileStream) ;
	SetInteractive(origIn) ;
	ExtraSetPermanent(userIn) ;
	ExtraSetPermanent(userOut) ;
	ExtraSetPermanent(userErr) ;
}
