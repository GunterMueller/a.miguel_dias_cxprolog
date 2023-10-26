/*
 *   This file is part of the CxProlog system

 *   Stream.c
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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

#if UNDERSTAND_EXTERNAL_ENCODINGS
#include <wchar.h>
#define MyUseLocale(l)				UseLocale(l)
#else
#define MyUseLocale(l)				/* nothing */
#endif

ExtraTypePt streamType ;
static StreamPt origIn, origOut, origErr ;
StreamPt userIn, userOut, userErr, userPrt ;
StreamPt currIn, currOut ;

static Pt tReadAtom, tWriteAtom, tAppendAtom ;
static CharPt stringStreamData ;
static Pt listStreamData ;

/* PRIVATE FUNCTIONS */

static StreamPt StreamNew(VoidPt chn, CharPt n, StreamMode m, StreamKind k)
{
	StreamPt srm = ExtraNew(streamType, 0) ;
	StreamKind(srm) = k ;
	StreamMode(srm) = m ;
	StreamChannel(srm) = chn ;
	StreamAtom(srm) = LookupTempAtom(n) ;
	StreamPath(srm) =
		Booting() || !IsFileStream(srm)
				? StreamAtom(srm)
				: AbsoluteFileName(n) ;
	StreamLocale(srm) = DefaultLocale() ;
	StreamAllowReposition(srm) = false ;
	StreamEofAction(srm) = eofCode ;
	StreamIsATty(srm) = false ;
	StreamEofSeen(srm) = false ;
	StreamAlias(srm) = nil ;
	StreamIsEdinburgh(srm) = false ;
	return srm ;
}

static StreamPt UsingStream(StreamPt srm, StreamMode mode)
{
	if( !XExtraCheck(streamType, TagExtra(streamType, srm)) )
		FileError("Invalid operation over closed stream %s", ExtraAsStr(srm)) ;
	if( mode == mNone )
		return srm ;
	if( (StreamMode(srm)==mRead) == (mode==mRead) )
		return srm ;
	elif( StreamMode(srm) == mRead )
		return FileError("'%s' is a read stream; cannot be used as write stream",
							ExtraAsStr(srm)) ;
	else
		return FileError("'%s' is a write stream; cannot be used as read stream",
							ExtraAsStr(srm)) ;
}

static StreamPt XTestStreamGen(Pt t, StreamMode mode, Bool err)
{
	if( Drf(t) == tUserAtom ) {
		switch( mode ) {
			case mRead: return userIn ;
			case mWrite: return userOut ;
			case mAppend: return userOut ;
			default: return userIn ; /* user == user_input by default */
		}
	}
	else {
		StreamPt srm = XTestExtraGen(streamType, t, true, err) ;
		if( srm != nil ) UsingStream(srm, mode) ;
		return srm ;
	}
}

static void EnsureTextStream(StreamPt srm)
{
	if( StreamIsBinary(srm) )
		FileError("This operation is not available for binary streams") ;
}

#if !COMPAT_0_90_3
static void EnsureBinaryStream(StreamPt srm)
{
	if( !StreamIsBinary(srm) )
		FileError("This operation is not available for text streams") ;
}
#endif

static void SetStdStreamsAliases()
{
	AtomPt a ;
	a = LookupTempAtom("user_input") ;
	StreamAlias(userIn) = TagAtom(a) ;
	AliasSet(a, cExtraPt(userIn)) ;
	a = LookupTempAtom("user_output") ;
	StreamAlias(userOut) = TagAtom(a) ;
	AliasSet(a, cExtraPt(userOut)) ;
	a = LookupTempAtom("user_error") ;
	StreamAlias(userErr) = TagAtom(a) ;
	AliasSet(a, cExtraPt(userErr)) ;
}

static void UnsetStdStreamsAliases()
{
	AtomPt a ;
	a = LookupTempAtom("user_input") ;
	StreamAlias(userIn) = nil ;
	AliasUnset(a, cExtraPt(userIn)) ;
	a = LookupTempAtom("user_output") ;
	StreamAlias(userOut) = nil ;
	AliasUnset(a, cExtraPt(userOut)) ;
	a = LookupTempAtom("user_error") ;
	StreamAlias(userErr) = nil ;
	AliasUnset(a, cExtraPt(userErr)) ;
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

static CharPt StreamKindStr(StreamPt srm)
{
	switch( StreamKind(srm) ) {
		case textFileStream: return "file" ;
		case textBufferStream: return "buffer" ;
		case binaryFileStream: return "file" ;
		case binaryBufferStream: return "buffer" ;
		case netStream: return "net" ;
		case interactiveStream: return "interactive" ;
		case nullStream: return "null" ;
		case stringStream: return "string" ;
		case listStream: return "list" ;
		case innerStream: return "inner" ;
		default: return InternalError("StreamKindStr") ;
	}
}

static EofAction XTestEofAction(Pt t)
{
	t = Drf(t) ;
	if( t == tErrorAtom ) return eofError ;
	elif( t == tEofCodeAtom ) return eofCode ;
	elif( t == tResetAtom ) return eofReset ;
	else { FileError("Invalid eof action") ; return 0 ; }
}

static CharPt StreamEofActionStr(StreamPt srm)
{
	switch( StreamEofAction(srm) ) {
		case eofError: return "error" ;
		case eofCode: return "eof_code" ;
		case eofReset: return "reset" ;
		default: return InternalError("StreamEofActionStr") ;
	}
}

static Size StreamSizeFun(VoidPt x)
{
	return WordsOf(Stream) ;
}

static void StreamsBasicGCMarkContents(VoidPt x)
{
	StreamPt srm = cStreamPt(x) ;
	ExtraGCMark(StreamAtom(srm)) ;
	ExtraGCMark(StreamPath(srm)) ;
	if( StreamChannel(srm) != nil )
		ExtraGCMark(StreamChannel(srm)) ;
}

static Bool StreamBasicGCDelete(VoidPt x)
{
	StreamPt srm = cStreamPt(x) ;
	StreamClose(srm, nil) ;
	return true ;
}
	


/* PRIVATE EDINBURGH FUNCTIONS */

static Bool FindEdinburghStreamAux(VoidPt x, VoidPt name)
{
	return StreamIsEdinburgh(cStreamPt(x))
		&& StrEqual(StreamName(cStreamPt(x)), cCharPt(name)) ;
}
static StreamPt FindEdinburghStream(CharPt name, StreamMode mode)
{
	StreamPt res ;
	if( name[0] == '\0' )
		FileError("'' is an invalid file name") ;
	if( (res = ExtraFindFirst(streamType, 0, FindEdinburghStreamAux, name)) != nil )
		return UsingStream(res, mode) ;
	return nil ;
}

static StreamPt FileEdinburghStreamOpen(CharPt name, StreamMode mode)
{
	StreamPt srm ;
	if( (srm = FindEdinburghStream(name, mode)) != nil )
		return srm ;
	srm = FileStreamOpen(name, mode, nil)  ;
	StreamIsEdinburgh(srm) = true ;
	return srm ;
}


/* OPEN & CLOSE OPERATIONS */

StreamPt FileStreamOpen(CharPt name, StreamMode mode, OpenStreamOptionsPt s)
{
	OpenStreamOptions soDef ;
	OpenStreamOptionsPt so =
				s != nil ? s : StreamPropertyGetDefaultOpenOptions(&soDef) ;
	CharPt locale = so->encoding == tTextAtom ? DefaultLocale()
					: so->encoding == tBinaryAtom ? BinaryLocale()
					: MakeLocaleCType(XAtomName(so->encoding)) ; /* may fail */

	FilePt f = FileNew(nil, name, mode) ;
	StreamPt srm = StreamNew(f, name, mode,
					so->type == tBinaryAtom ? binaryFileStream : textFileStream) ;	
	StreamLocale(srm) = locale ;
	StreamEofAction(srm) = XTestEofAction(so->eofAction) ;
	StreamAllowReposition(srm) = so->reposition == tTrueAtom ;
	StreamAlias(srm) = so->alias ;
	StreamIsATty(srm) = FileIsATty(f) ;
	return srm ;
}

StreamPt BufferStreamOpen(BufferPt buff, StreamMode mode, OpenStreamOptionsPt s)
{
	OpenStreamOptions soDef ;
	OpenStreamOptionsPt so =
				s != nil ? s : StreamPropertyGetDefaultOpenOptions(&soDef) ;
	CharPt locale = so->encoding == tTextAtom ? DefaultLocale()
					: so->encoding == tBinaryAtom ? BinaryLocale()
					: MakeLocaleCType(XAtomName(so->encoding)) ; /* may fail */
	StreamPt srm ;
	CharPt name = GStrFormat("_%%buffer_%lx", cWord(buff)) ;

/* open buffer */
	if( mode == mWrite ) BufferRewrite(buff) ;
	elif( mode == mRead ) BufferReset(buff) ;
	elif( mode == mAppend ) BufferAppend(buff) ;
	else InternalError("BufferStreamOpen") ;

/* setup descriptor */
	srm = StreamNew(buff, name, mode,
			so->type == tBinaryAtom ? binaryBufferStream : textBufferStream) ;	
	StreamLocale(srm) = locale ;
	StreamEofAction(srm) = XTestEofAction(so->eofAction) ;
	StreamAllowReposition(srm) = so->reposition == tTrueAtom ;
	StreamAlias(srm) = so->alias ;
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
		ExtraHide(srm) ;
		ExtraPermanent(srm) ;
	}
	stringStreamData = string ;
	return srm ;
}

StreamPt ListStreamOpen(Pt list)
{
	static StreamPt srm = nil ;
	if( srm == nil ) { /* There is only one  stream */
		srm = StreamNew(nil, "_%list", mRead, listStream) ;
		ExtraHide(srm) ;
		ExtraPermanent(srm) ;
	}
	listStreamData = Drf(list) ;
	return srm ;
}

StreamPt InnerStreamOpen()
{
	static StreamPt srm = nil ;
	if( srm == nil ) { /* There is only one inner stream */
		srm = StreamNew(nil, "_%inner", mWrite, innerStream) ;
		ExtraHide(srm) ;
		ExtraPermanent(srm) ;
	}
	BigStrOpen() ;
	return srm ;
}

StreamPt FILEToStream(FILE *file, CharPt n, StreamMode m, StreamKind k)
{
	FilePt f = FileNew(file, n, m) ;
	StreamPt srm = StreamNew(f, n, m, k) ;
	StreamIsATty(srm) = FileIsATty(f) ;
	return srm ;
}

CharPt StreamClose(StreamPt srm, CloseStreamOptionsPt co)
{
	CloseStreamOptions coDef ;
	CharPt res = nil ; /* keep nil here */

	if( srm == userIn || srm == userOut || srm == userErr )
		return nil ;
	
	if( co == nil )
		StreamPropertyGetDefaultCloseOptions(co = &coDef) ;

	switch( StreamKind(srm) ) {
		case textFileStream:
		case binaryFileStream:
		case netStream:
		case interactiveStream: {
			if( ExtraDisable(srm) )
				FileDelete(StreamChannel(srm), co->force == tTrueAtom) ;
			break ;
		}
		case textBufferStream:
		case binaryBufferStream: {
			if( ExtraDisable(srm) )
				BufferDelete(StreamChannel(srm)) ;
			break ;
		}
		case nullStream: {
			ExtraDisable(srm) ;
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

void StreamsReAliasing(AtomPt a, StreamPt srm, Bool on)
{
	/* TO BE DONE */
	/* sync the user stream aliases with userIn, userOut, etc. */
}



/* BASIC INPUT OPERATIONS */

static void CheckDoubleEOF(StreamPt srm, Bool advance)
{
	if( StreamEofAction(srm) == eofError ) {
		if( StreamEofSeen(srm) )
			FileError("Attempt to read beyond the end_of_file") ;
	}
	if( advance )
		StreamEofSeen(srm) = true ;
}

WChar StreamGet(StreamPt srm)
{
	WChar c ;
	switch( StreamKind(srm) ) {
		case textFileStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			c = FileGetChar(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, true) ;
			return c ;
		}
		case binaryFileStream:
		case netStream: {
			c = FileGetByte(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, true) ;
			return c ;
		}
		case textBufferStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			c = BufferGetChar(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, true) ;
			return c ;
		}
		case binaryBufferStream: {
			c = BufferGetByte(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, true) ;
			return c ;
		}
		case interactiveStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			c = InterLineGet() ;
			if( c == EOF ) CheckDoubleEOF(srm, true) ;
			return c ;
		}
		case nullStream: {
			FileError("This operation is not available for null streams") ;
			return 0 ;
		}
		case stringStream: {
			if( *stringStreamData == '\0' ) return EOF ;
			return CharDecode(stringStreamData) ;
		}
		case listStream: {
			if( listStreamData == tNilAtom ) return EOF ;
			if( IsList(listStreamData) ) {
				Pt t = Drf(XListHead(listStreamData)) ;
				listStreamData = Drf(XListTail(listStreamData)) ;
				return XTestCode(t) ;
			}
			else TypeError("PROPERLY-TERMINATED-LIST", nil) ;
		}
		case innerStream: {
			FileError("This operation is not available for inner streams") ;
			return 0 ;
		}		
		default: return IInternalError("StreamGet") ;
	}
}

WChar StreamPeek(StreamPt srm)
{
	WChar c ;
	switch( StreamKind(srm) ) {
		case textFileStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			c = FilePeekChar(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, false) ;
			return c ;
		}
		case binaryFileStream:
		case netStream: {
			c = FilePeekByte(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, false) ;
			return c ;
		}
		case textBufferStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			c = BufferPeekChar(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, false) ;
			return c ;
		}
		case binaryBufferStream: {
			c = BufferPeekByte(StreamChannel(srm)) ;
			if( c == EOF ) CheckDoubleEOF(srm, false) ;
			return c ;
		}
		case interactiveStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			c = InterLinePeek() ;
			if( c == EOF ) CheckDoubleEOF(srm, true) ;
			return c ;
		}
		case nullStream: {
			FileError("This operation is not available for null streams") ;
			return 0 ;
		}
		case stringStream: {
			if( *stringStreamData == '\0' ) return EOF ;
			return CharFirst(stringStreamData) ;
		}
		case listStream: {
			if( listStreamData == tNilAtom ) return EOF ;
			if( IsList(listStreamData) ) {
				Pt t = Drf(XListHead(listStreamData)) ;
				return XTestCode(t) ;
			}
			else TypeError("PROPERLY-TERMINATED-LIST", nil) ;
		}
		case innerStream: {
			FileError("This operation is not available for inner streams") ;
			return 0 ;
		}		
		default: return IInternalError("StreamPeek") ;
	}
}

Size StreamReadBytes(StreamPt srm, VoidPt v, Size n)
{
	switch( StreamKind(srm) ) {
		case textFileStream: {
			return FileGetNBytes(StreamChannel(srm), v, n, true) ;
		}
		case binaryFileStream: {
			return FileGetNBytes(StreamChannel(srm), v, n, false) ;
		}
		case textBufferStream:
		case binaryBufferStream: {
			return BufferGetNBytes(StreamChannel(srm), v, n) ;
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
		case textFileStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			return FilePeekChar(StreamChannel(srm)) == EOF ;
		}
		case binaryFileStream:
		case netStream: {
			return FilePeekByte(StreamChannel(srm)) == EOF ;
		}
		case textBufferStream: {
		case binaryBufferStream:
			return BufferAtEnd(StreamChannel(srm)) ;
		}
		case interactiveStream: {
			MyUseLocale(StreamLocale(srm)) ;	
			return InterLinePeek() == EOF ;
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

void StreamPut(StreamPt srm, WChar c)
{
	switch( StreamKind(srm) ) {
		case textFileStream: {
			MyUseLocale(StreamLocale(srm)) ;
			FilePutChar(StreamChannel(srm), c) ;
			break ;
		}
		case binaryFileStream:
		case netStream: {
			FilePutByte(StreamChannel(srm), c) ;
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
		case interactiveStream: {
			FileError("This operation is not available for readline streams") ;
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
			FilePutCharStr(StreamChannel(srm), s) ;
			break ;
		}
		case binaryFileStream:
		case netStream: {
			while( *s )
				FilePutByte(StreamChannel(srm), *s++) ;
			break ;
		}
		case textBufferStream: {
			MyUseLocale(StreamLocale(srm)) ;
			BufferPutCharStr(StreamChannel(srm), s) ;
			break ;
		}
		case binaryBufferStream: {
			while( *s )
				BufferPutByte(StreamChannel(srm), *s++) ;
			break ;
		}
		case interactiveStream: {
			FileError("This operation is not available for readline streams") ;
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
    va_end(p) ;
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
			return FilePutNBytes(StreamChannel(srm), v, n, true) ;
		}
		case binaryFileStream: {
			return FilePutNBytes(StreamChannel(srm), v, n, false) ;
		}
		case textBufferStream:
		case binaryBufferStream: {
			return BufferPutNBytes(StreamChannel(srm), v, n) ;
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
		     Streams have not been initialized yet.
		     Also the memory manager may have not been initialized yet. */
		vfprintf(stderr, fmt, v) ;
		freopen(nil, "a", stderr) ;	/* Set no-orientation on stderr */
	}
	else {
		CharPt s = GStrFormatV(fmt, v) ;		
		if( srm == userErr ) {
			StreamFlush(userOut) ;
			StreamPutStr(srm, s) ;
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
		case binaryFileStream:
		case netStream: {
			FileFlush(StreamChannel(srm)) ;
			break ;
		}
		case textBufferStream:
		case binaryBufferStream:
		case nullStream: {
			break ;
		}
		case interactiveStream: {
			FileError("This operation is not available for readline streams") ;
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

WChar StreamGetNonBlank(StreamPt srm)
{
	for(;;) {
		WChar c = StreamGet(srm) ;
		if( cx_isspace(c) )
			/* continue */ ;
		else
			return c ;
	}
}

WChar StreamPeekNonBlank(StreamPt srm)
{
	for(;;) {
		WChar c = StreamPeek(srm) ;
		if( cx_isspace(c) )
			StreamGet(srm) ;
		else
			return c ;
	}
}

CharPt StreamGetLine(StreamPt srm)
{
	register WChar c = StreamGet(srm) ;
	if( c == EOF ) return nil ;
	GStrOpen() ;
	while( c != '\n' && c != EOF ) {
		GStrAddChar(c) ;
		c = StreamGet(srm) ;
	}
	return GStrClose() ;
}

WChar GetCharCommand(CharPt prompt, int *arg)
{
	WChar c, res ;
	int n ;
	Bool hasArg ;
	InterruptOff() ;
	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	InterLineBeginLinePrompt(prompt != nil ? prompt : "") ;
	while( (c = StreamGet(userIn)) <= ' ' && c != '\n' && c != EOF ) ;
	InterLineEndLinePrompt() ;
	res = InRange(c,'A','Z') ? (c - 'A' + 'a') : c ;
	hasArg = 0 ;
	n = 0 ;
	while( c != '\n' && c != EOF ) {
		if( InRange(c, '0', '9') ) {
			hasArg = true ;
			n = n * 10 + c - '0' ;
		}
		c = StreamGet(userIn) ;
	}
	if( arg != nil )
		*arg = hasArg ? n : -1 ;
	while( c != '\n' && c != EOF )
		c = StreamGet(userIn) ;
	InterruptOn() ;
	return res ;
}


/* MORE OUTPUT OPERATIONS */

void StreamWrite(StreamPt srm, CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	StreamWriteV(srm, fmt, v) ;
}

void Write(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	StreamWriteV(currOut, fmt, v) ;
}

void WriteStd(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	StreamWriteV(userOut, fmt, v) ;
}

void WriteErr(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	StreamWriteV(userErr, fmt, v) ;
}

void WriteEOF()
{
	StreamWrite(userOut, "EOF\n") ;
}

void Dot(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	StreamWriteV(userErr, fmt, v) ;
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
	StreamPt srm = FileStreamOpen(XTestFileName(X0), XTestStreamMode(X1), nil) ;
	BindVarWithExtra(X2, srm) ;
	JumpNext() ;
}

#if COMPAT_0_90_3
static void POpenFileStream4()
{
	OpenStreamOptions so ;
	StreamPt srm = FileStreamOpen(XTestFileName(X0), XTestStreamMode(X1),
								StreamPropertyGetOpenOptions(X2, &so)) ;
	if( so.alias != nil ) BindVarWithExtra(so.alias, srm) ;
	BindVarWithExtra(X3, srm) ;
	JumpNext() ;
}
#else
static void POpenFileStream4()
{
	OpenStreamOptions so ;
	StreamPt srm = FileStreamOpen(XTestFileName(X0), XTestStreamMode(X1),
								StreamPropertyGetOpenOptions(X3, &so)) ;
	if( so.alias != nil ) BindVarWithExtra(so.alias, srm) ;
	BindVarWithExtra(X2, srm) ;
	JumpNext() ;
}
#endif

static void POpenBufferStream3()
{
	StreamPt srm = BufferStreamOpen(XTestBuffer(X0), XTestStreamMode(X1), nil) ;
	BindVarWithExtra(X2, srm) ;
	JumpNext() ;
}

#if COMPAT_0_90_3
static void POpenBufferStream4()
{
	OpenStreamOptions so ;
	StreamPt srm = BufferStreamOpen(XTestBuffer(X0), XTestStreamMode(X1),
								StreamPropertyGetOpenOptions(X2, &so) ;
	if( so.alias != nil ) BindVarWithExtra(so.alias, srm) ;
	BindVarWithExtra(X3, srm) ;
	JumpNext() ;
}
#else
static void POpenBufferStream4()
{
	OpenStreamOptions so ;
	StreamPt srm = BufferStreamOpen(XTestBuffer(X0), XTestStreamMode(X1),
								StreamPropertyGetOpenOptions(X3, &so)) ;
	if( so.alias != nil ) BindVarWithExtra(so.alias, srm) ;
	BindVarWithExtra(X2, srm) ;
	JumpNext() ;
}
#endif

static void POpenNull()
{
	BindVarWithExtra(X0, NullStreamOpen()) ;
	JumpNext() ;
}

static void PClose1()
{
	StreamPt srm = XTestStream(X0, mNone) ;
	StreamClose(srm, nil) ;
	JumpNext() ;
}

static void PClose2()
{
	CloseStreamOptions co ;
	StreamPt srm = XTestStream(X0, mNone) ;
	StreamClose(srm, StreamPropertyGetCloseOptions(X1, &co)) ;
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
	Pt t = IsAbsoluteFileName(AtomName(StreamPath(srm)))
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
	if( IsAbsoluteFileName(AtomName(StreamPath(srm))) )
		Write("absolute_file_name('%s')", AtomName(StreamPath(srm))) ;
	Write("\n%10s", "") ;	
	Write("name('%s'), kind(%s), type(%s), mode(%s), ",
		StreamName(srm),
		StreamKindStr(srm),
		StreamIsBinary(srm) ? "binary" : "text",
		StreamModeStr(srm)) ;
	Write("\n%10s", "") ;	
	Write("encoding(%s), reposition(%s), eof_action(%s)",
		StreamLocale(srm),
		StreamAllowReposition(srm) ? "true" : "false",
		StreamEofActionStr(srm)) ;
	Write("\n%10s", "") ;	
	Write("tty(%s), current(%s)",
		StreamIsATty(srm) ? "true" : "false",
		(srm == currIn || srm == currOut) ? "true" : "false") ;
	if( StreamIsATty(srm) && StreamMode(srm) == mRead ) /* avoids waiting for i */
		Write(", at_eof(false)") ;
	else
		Write(", at_eof(%s)", StreamAtEnd(srm) ? "true" : "false") ;
	return 1 ;
}
static void PStreams()
{
	ExtraShow(streamType, PStreamsAux) ;
	JumpNext() ;
}

static void PSetUserStreams()
{
	StreamPt s0 = XTestStream(X0, mRead) ;
	StreamPt s1 = XTestStream(X1, mWrite) ;
	StreamPt s2 = XTestStream(X2, mWrite) ;
	
	UnsetStdStreamsAliases() ;
	if( currIn == userIn ) currIn = s0 ;
	if( currOut == userOut ) currOut = s1 ;
	if( currOut == userErr ) currOut = s2 ;

	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	userIn = s0 ;
	userOut = s1 ;
	userErr = s2 ;
	SetStdStreamsAliases() ;
	InterLineChangedStreams() ;
	JumpNext() ;
}

static void PRestoreUserStreams()
{
	UnsetStdStreamsAliases() ;
	if( currIn == userIn ) currIn = origIn ;
	if( currOut == userOut ) currOut = origOut ;
	if( currOut == userErr ) currOut = origErr ;

	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	userIn = origIn ;
	userOut = origOut ;
	userErr = origErr ;
	SetStdStreamsAliases() ;
	InterLineChangedStreams() ;
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
	StreamPt srm ;
	if( (srm = XTestStreamGen(X0, mRead, false)) != nil )
		currIn = srm ;
	elif( IsAtom(Drf(X0)) )
		currIn = FileEdinburghStreamOpen(XTestFileName(X0), mRead) ;
	else ExtraTypeError(streamType, "FILENAME", X0) ;
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
	StreamClose(currIn, nil) ;
	currIn = userIn ;
	JumpNext() ;
}

static void PTell()
{
	StreamPt srm ;
	if( (srm = XTestStreamGen(X0, mWrite, false)) != nil )
		currOut = srm ;
	elif( IsAtom(Drf(X0)) )
		currOut = FileEdinburghStreamOpen(XTestFileName(X0), mWrite) ;
	else ExtraTypeError(streamType, "FILENAME", X0) ;
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
	StreamClose(currOut, nil) ;
	currOut = userOut ;
	JumpNext() ;
}

#if !COMPAT_0_90_3
static void PGetChar()
{
	EnsureTextStream(currIn) ;
	MustBe( UnifyWithAtomic(X0, MakeChar(StreamGet(currIn))) ) ;
}

static void PSGetChar()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureTextStream(srm) ;
	MustBe( UnifyWithAtomic(X1, MakeChar(StreamGet(srm))) ) ;
}

static void PGetCode()
{
	EnsureTextStream(currIn) ;
	MustBe( UnifyWithAtomic(X0, MakeCode(StreamGet(currIn))) ) ;
}

static void PSGetCode()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureTextStream(srm) ;
	MustBe( UnifyWithAtomic(X1, MakeCode(StreamGet(srm))) ) ;
}
#endif

static void PGet()
{
	EnsureTextStream(currIn) ;
	MustBe( UnifyWithNumber(X0, MakeCode(StreamGetNonBlank(currIn))) ) ;
}

static void PSGet()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureTextStream(srm) ;
	MustBe( UnifyWithNumber(X1, MakeCode(StreamGetNonBlank(srm))) ) ;
}

static void PGetLine()
{
	CharPt str ;
	EnsureTextStream(currIn) ;
	if( (str = StreamGetLine(currIn)) == nil )
		MustBe( UnifyWithAtomic(X0, MakeCode(EOF)) ) ;
	else
		MustBe( UnifyWithAtomic(X0, MakeTempAtom(str)) ) ;
}

static void PSGetLine()
{
	CharPt str ;
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureTextStream(srm) ;
	if( (str = StreamGetLine(srm)) == nil )
		MustBe( UnifyWithAtomic(X1, MakeCode(EOF)) ) ;
	else
		MustBe( UnifyWithAtomic(X1, MakeTempAtom(str)) ) ;
}

static void PGetSingleChar()
{
	WChar c ;
	if( !InterLineGetSingleChar(&c) ) {
		CharPt s = StreamGetLine(userIn) ;
		if( s == nil )
			c = EOF ;
		else {
			while( *s == ' ' ) s++ ;
			c = *s == '\0' ? 10 : *s ;
		}
	}
	MustBe( UnifyWithNumber(X0, MakeCode(c)) ) ;
}

#if !COMPAT_0_90_3
static void PPeekChar()
{
	EnsureTextStream(currIn) ;
	MustBe( UnifyWithAtomic(X0, MakeChar(StreamPeek(currIn))) ) ;
}

static void PSPeekChar()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureTextStream(srm) ;
	MustBe( UnifyWithAtomic(X1, MakeChar(StreamPeek(srm))) ) ;
}

static void PPeekCode()
{
	EnsureTextStream(currIn) ;
	MustBe( UnifyWithAtomic(X0, MakeCode(StreamPeek(currIn))) ) ;
}

static void PSPeekCode()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureTextStream(srm) ;
	MustBe( UnifyWithAtomic(X1, MakeCode(StreamPeek(srm))) ) ;
}
#endif

static void PPeek()
{
	EnsureTextStream(currIn) ;
	MustBe( UnifyWithNumber(X0, MakeCode(StreamPeekNonBlank(currIn))) ) ;
}

static void PSPeek()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureTextStream(srm) ;
	MustBe( UnifyWithNumber(X1, MakeCode(StreamPeekNonBlank(srm))) ) ;
}

#if !COMPAT_0_90_3
static void PPutChar()
{
	EnsureTextStream(currOut) ;
	StreamPut(currOut, XTestChar(X0)) ;
	JumpNext() ;
}

static void PSPutChar()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	EnsureTextStream(srm) ;
	StreamPut(srm, XTestChar(X1)) ;
	JumpNext() ;
}

static void PPutCode()
{
	EnsureTextStream(currOut) ;
	StreamPut(currOut, XTestCode(X0)) ;
	JumpNext() ;
}

static void PSPutCode()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	EnsureTextStream(srm) ;
	StreamPut(srm, XTestCode(X1)) ;
	JumpNext() ;
}
#endif

static void PNl()
{
	EnsureTextStream(currOut) ;
	StreamPut(currOut, '\n') ;
	JumpNext() ;
}

static void PSNl()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	EnsureTextStream(srm) ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PTab()
{
	Size n = XTestInt(X0) ;
	EnsureTextStream(currOut) ;
	while( n-- )
		StreamPut(currOut, ' ') ;
	JumpNext() ;
}

static void PSTab()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	Size n = XTestInt(X1) ;
	EnsureTextStream(srm) ;
	while( n-- )
		StreamPut(srm, ' ') ;
	JumpNext() ;
}

#if !COMPAT_0_90_3
static void PGetByte()
{
	EnsureBinaryStream(currIn) ;
	MustBe( UnifyWithAtomic(X0, MakeByte(StreamGet(currIn))) ) ;
}

static void PSGetByte()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureBinaryStream(srm) ;
	MustBe( UnifyWithAtomic(X1, MakeByte(StreamGet(srm))) ) ;
}

static void PPeekByte()
{
	EnsureBinaryStream(currIn) ;
	MustBe( UnifyWithAtomic(X0, MakeByte(StreamPeek(currIn))) ) ;
}

static void PSPeekByte()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	EnsureBinaryStream(srm) ;
	MustBe( UnifyWithAtomic(X1, MakeByte(StreamPeek(srm))) ) ;
}

static void PPutByte()
{
	if( StreamKind(currOut) != nullStream )
		EnsureBinaryStream(currOut) ;
	StreamPut(currOut, XTestByte(X0)) ;
	JumpNext() ;
}

static void PSPutByte()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	if( StreamKind(srm) != nullStream )
		EnsureBinaryStream(srm) ;
	StreamPut(srm, XTestByte(X1)) ;
	JumpNext() ;
}
#endif

static void PAtEndOfStream()
{
	MustBe( StreamAtEnd(currIn) ) ;
}

static void PSAtEndOfStream()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	MustBe( StreamAtEnd(srm) ) ;
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
	WChar c = StreamGet(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X0, t) ) ;
}

static void PSGet0()
{ 
	StreamPt srm = XTestStream(X0, mRead) ;
	WChar c = StreamGet(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X1, t) ) ;
}

static void PSkip()
{
	WChar c = StreamIsBinary(currIn) ? XTestByte(X0) : XTestCode(X0) ;
	while( StreamGet(currIn) != c ) ;
	JumpNext() ;
}

static void PSSkip()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	WChar c = StreamIsBinary(srm) ? XTestByte(X1) : XTestCode(X1) ;
	while( StreamGet(srm) != c ) ;
	JumpNext() ;
}

static void PPeek0()
{
	WChar c = StreamPeek(currIn) ;
	Pt t = StreamIsBinary(currIn) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X0, t) ) ;
}

static void PSPeek0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	WChar c = StreamPeek(srm) ;
	Pt t = StreamIsBinary(srm) ? MakeByte(c) : MakeCode(c) ;
	MustBe( UnifyWithNumber(X1, t) ) ;
}

static void PPut()
{
	WChar c = StreamIsBinary(currOut) ? XTestByte(X0) : XTestCode(X0) ;
	StreamPut(currOut, c) ;
	JumpNext() ;
}

static void PSPut()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WChar c = StreamIsBinary(srm) ? XTestByte(X1) : XTestCode(X1) ;
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
		BufferEnsureCapacity(b, BufferSize(b) + 256) ;
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
	BufferEnsureCapacity(b, n) ;
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
/* 7.84 - 9.42 - 32.32 */
/* 2.37 - 3.59 - 9.57 - 172 */
	StreamPt srm ;
	double start = CpuTime() ;
	int i ;
	srm = FileStreamOpen("/tmp/tttest", mWrite, nil) ;
	dotimes(i, 30000000) {
		StreamPut(srm, 'a') ;
	}
	StreamClose( srm, nil) ;
	Mesg("time = %lf", CpuTime() - start) ;
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

StreamPt XTestStream(Pt t, StreamMode mode)
{
	return XTestStreamGen(t, mode, true) ;
}

void StreamsInit2()
{
	tReadAtom = MakeAtom("read") ;
	tWriteAtom = MakeAtom("write") ;
	tAppendAtom = MakeAtom("append") ;

	SetStdStreamsAliases() ;
	
	InstallCBuiltinPred("stream", 1, PStreamCheck) ;

	InstallCBuiltinPred("open", 3, POpenFileStream3) ;
	InstallCBuiltinPred("open", 4, POpenFileStream4) ;
	InstallCBuiltinPred("open_buffer_stream", 3, POpenBufferStream3) ;
	InstallCBuiltinPred("open_buffer_stream", 4, POpenBufferStream4) ;
	InstallCBuiltinPred("open_null_stream", 1, POpenNull) ;
	InstallCBuiltinPred("close", 1, PClose1) ;
	InstallCBuiltinPred("close", 2, PClose2) ;
	InstallGNDeterCBuiltinPred("current_stream", 3, 2, PNDCurrentStream3) ;
	InstallGNDeterCBuiltinPred("current_stream", 4, 2, PNDCurrentStream4) ;
	InstallCBuiltinPred("streams", 0, PStreams) ;
	InstallCBuiltinPred("$set_user_streams", 3, PSetUserStreams) ;
	InstallCBuiltinPred("$restore_user_streams", 0, PRestoreUserStreams) ;

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

#if !COMPAT_0_90_3
	InstallCBuiltinPred("get_char", 1, PGetChar) ;
	InstallCBuiltinPred("get_char", 2, PSGetChar) ;
	InstallCBuiltinPred("get_code", 1, PGetCode) ;
	InstallCBuiltinPred("get_code", 2, PSGetCode) ;
#endif
	InstallCBuiltinPred("get", 1, PGet) ;
	InstallCBuiltinPred("get", 2, PSGet) ;
	InstallCBuiltinPred("get_line", 1, PGetLine) ;
	InstallCBuiltinPred("get_line", 2, PSGetLine) ;
	InstallCBuiltinPred("get_single_char", 1, PGetSingleChar) ;

#if !COMPAT_0_90_3
	InstallCBuiltinPred("peek_char", 1, PPeekChar) ;
	InstallCBuiltinPred("peek_char", 2, PSPeekChar) ;
	InstallCBuiltinPred("peek_code", 1, PPeekCode) ;
	InstallCBuiltinPred("peek_code", 2, PSPeekCode) ;
#endif
	InstallCBuiltinPred("peek", 1, PPeek) ;
	InstallCBuiltinPred("peek", 2, PSPeek) ;

#if !COMPAT_0_90_3
	InstallCBuiltinPred("put_char", 1, PPutChar) ;
	InstallCBuiltinPred("put_char", 2, PSPutChar) ;
	InstallCBuiltinPred("put_code", 1, PPutCode) ;
	InstallCBuiltinPred("put_code", 2, PSPutCode) ;
#endif
	InstallCBuiltinPred("nl", 0, PNl) ;	
	InstallCBuiltinPred("nl", 1, PSNl) ;
	InstallCBuiltinPred("tab", 1, PTab) ;
	InstallCBuiltinPred("tab", 2, PSTab) ; 
 
#if !COMPAT_0_90_3
	InstallCBuiltinPred("get_byte", 1, PGetByte) ;
	InstallCBuiltinPred("get_byte", 2, PSGetByte) ;
	InstallCBuiltinPred("peek_byte", 1, PPeekByte) ;
	InstallCBuiltinPred("peek_byte", 2, PSPeekByte) ;
	InstallCBuiltinPred("put_byte", 1, PPutByte) ;
	InstallCBuiltinPred("put_byte", 2, PSPutByte) ;
#endif

	InstallCBuiltinPred("at_end_of_stream", 0, PAtEndOfStream) ;
	InstallCBuiltinPred("at_end_of_stream", 1, PSAtEndOfStream) ;
	InstallCBuiltinPred("flush_output", 0, PFlushOutput) ;
	InstallCBuiltinPred("flush_output", 1, PSFlushOutput) ;
	InstallCBuiltinPred("flush_output_all", 0, PSFlushOutputAll) ;
#if COMPAT_0_90_3
	InstallCBuiltinPred("flush", 0, PFlushOutput) ;
	InstallCBuiltinPred("flush", 1, PSFlushOutput) ;
	InstallCBuiltinPred("flushall", 0, PSFlushOutputAll) ;
#endif

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

	InstallCBuiltinPred("ttttt", 0, PTTTest) ;
}

void StreamsInit()
{
	StreamKind inKind = InteractiveSession() && UseInteractiveStreams()
										? interactiveStream : textFileStream ;
	
	if( sizeof(WChar) < sizeof(wchar_t)  )
		FatalError("Invalid size of WChar %d %d", sizeof(WChar), sizeof(wchar_t)) ;

	streamType = ExtraTypeNew("STREAM", StreamSizeFun, StreamsBasicGCMarkContents, StreamBasicGCDelete, 1) ;
	
	currIn = userIn = origIn = FILEToStream(stdin, "user_input", mRead, inKind) ;
	StreamEofAction(userIn) = eofReset ;
	
	currOut = userOut = origOut = FILEToStream(stdout, "user_output", mWrite, textFileStream) ;
	userErr = origErr = FILEToStream(stderr, "user_error", mWrite, textFileStream) ;
	
	ExtraPermanent(userIn) ;
	ExtraPermanent(userOut) ;
	ExtraPermanent(userErr) ;
}
