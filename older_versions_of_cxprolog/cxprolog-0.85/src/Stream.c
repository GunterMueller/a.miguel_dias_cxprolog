/*
 *   This file is part of the CxProlog system

 *   Stream.c
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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
#define StreamFile(s)		((s)->file)
#define StreamMode(s)		((s)->mode)
#define StreamIsText(s)		((s)->isText)
#define StreamIsBinary(s)	(!(s)->isText)
#define StreamName(s)		((s)->name)

static ExtraTypePt streamType ;
static StreamPt userIn, userOut, userErr ;
static StreamPt origIn, origOut, origErr ;
StreamPt currIn, currOut ;

static Pt tReadAtom, tWriteAtom, tAppendAtom, tTextAtom, tBinaryAtom ;


/* AUXILIARY FUNCTIONS */

static StreamPt StreamNew(FILE *f, CharPt n, StreamMode m, Bool isText)
{
	StreamPt srm = ExtraNew(streamType) ;
	StreamFile(srm) = f ;
	StreamMode(srm) = m ;
	StreamIsText(srm) = isText ;
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
		FileError("'%s' is a read stream; cannot be used as write stream",
							StreamName(srm)) ;
	else
		FileError("'%s' is a write stream; cannot be used as read stream",
							StreamName(srm)) ;
	return nil ;
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
			return mode == mRead  ? userIn : userOut ;
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

static CharPt StreamModeAsStr(StreamMode sm)
{
	switch( sm ) {
		case mRead: return "Read" ;
		case mWrite: return "Write" ;
		case mAppend: return "Append" ;
		default: Default("StreamModeAtStr") ;
	}
	return nil ; /* Avoids warning */
}


/* PRIMITIVE OPERATIONS */

StreamPt FileStreamOpen(CharPt name, StreamMode mode, Bool isText)
{
	StreamPt srm ;
	FILE *file ;
	char m[10] ;
	if( (srm = FindNamedStream(name, mode)) != nil )
		return srm ;
	strcpy(m, mode==mWrite ? "w" : mode==mRead ? "r" : "a") ;
	if( !isText ) strcat(m, "b") ;
	if( (file = fopen(name, m)) == nil )
		FileError("Cannot open file '%s'", name) ;
	return StreamNew(file, name, mode, isText) ;
}

static StreamPt FileStreamOpenP(CharPt name, Pt mode, Pt option)
{
	StreamMode m ;
	Bool t ;
	if( mode == tReadAtom )
		m = mRead ;
	elif( mode == tWriteAtom )
		m = mWrite ;
	elif( mode == tAppendAtom )
		m = mAppend ;
	else FileError("Invalid stream mode") ;
	
	if( option == tTextAtom )
		t = true ;
	elif( option == tBinaryAtom )
		t = false ;
	else FileError("Invalid stream option") ;

	return FileStreamOpen(name, m, t) ;
}

StreamPt FILEToStream(FILE *file, StreamMode mode, CharPt prefName)
{
	Str256 s ;
	sprintf(s, "%s_%lx", prefName, file) ;
	return StreamNew(file, s, mode, true) ;
}

void StreamClose(StreamPt srm)
{
	if( srm == userIn || srm == userOut || srm == userErr ) return ;

	fclose(StreamFile(srm)) ;
	ExtraDelete(streamType, srm) ;

	if( srm == currIn ) currIn = userIn ;
	elif( srm == currOut ) currOut = userOut ;
}

int StreamGetChar(StreamPt srm)
{	/* pre: StreamIsText(srm) */
	register int c ;
	for(;;) { /* this works even with raw input or sockets */
		c = fgetc(StreamFile(srm)) ;
		if( c >= ' ' || c == '\n' ) return c ;
		if( c == 0 || c == 10 || c == 13 ) continue ;
		if( c == EOF || c == 4 || c == 26 ) /* CNTL-D, CNTL-Z */
			if( InterruptHandle() ) continue ;
			else return eofMark ;
		return c ;
	}
}

int StreamPeekChar(StreamPt srm)
{	/* pre: StreamIsText(srm) */
	register int c ;
	for(;;) { /* this works even with raw input or sockets */
		c = fgetc(StreamFile(srm)) ;
		if( c >= ' ' || c == '\n' ) return ungetc(c, StreamFile(srm)) ;
		if( c == 0 || c == 10 || c == 13 ) continue ;
		if( c == EOF || c == 4 || c == 26 ) /* CNTL-D, CNTL-Z */
			if( InterruptHandle() ) continue ;
			else return eofMark ;
		return ungetc(c, StreamFile(srm)) ;
	}
}

int StreamGetNonBlank(StreamPt srm)
{	/* pre: StreamIsText(srm) */
	register int c ;
	for(;;) {  /* this works even with raw input or sockets */
		c = fgetc(StreamFile(srm)) ;
		if( c > ' ' ) return c ;
		if( c == EOF || c == 4 || c == 26 ) /* CNTL-D, CNTL-Z */
			if( InterruptHandle() ) continue ;
			else return eofMark ;
	}
}

int StreamPeekNonBlank(StreamPt srm)
{	/* pre: StreamIsText(srm) */
	register int c ;
	for(;;) {  /* this works even with raw input or sockets */
		c = fgetc(StreamFile(srm)) ;
		if( c > ' ' ) return ungetc(c, StreamFile(srm)) ;
		if( c == EOF || c == 4 || c == 26 ) /* CNTL-D, CNTL-Z */
			if( InterruptHandle() ) continue ;
			else return eofMark ;
	}
}

int StreamGetByte(StreamPt srm)
{	/* pre: StreamIsBinary(srm) */
	register int c ;
	for(;;) {
		c = fgetc(StreamFile(srm)) ;
		if( c == EOF  )
			if( InterruptHandle() ) continue ;
			else return -1 ;
		return c ;
	}
}

int StreamPeekByte(StreamPt srm)
{	/* pre: StreamIsBinary(srm) */
	register int c ;
	for(;;) {
		c = fgetc(StreamFile(srm)) ;
		if( c == EOF  )
			if( InterruptHandle() ) continue ;
			else return -1 ;
		return ungetc(c, StreamFile(srm)) ;
	}
}

void StreamReadN(StreamPt srm, VoidPt v, Size n)
{
	fread(v, 1, n, StreamFile(srm)) ;
}

void StreamPut(StreamPt srm, int c)
{
	putc(c, StreamFile(srm)) ;
}

void StreamWriteN(StreamPt srm, VoidPt v, Size n)
{
	fwrite(v, 1, n, StreamFile(srm)) ;
}

void StreamWriteV(StreamPt srm, CharPt fmt, VoidPt v)
{
	if( srm == userErr ) StreamFlush(userOut) ;
	vfprintf(StreamFile(srm), fmt, v) ;
}

void StreamFlush(StreamPt srm)
{
	fflush(StreamFile(srm)) ;
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


/* INPUT STREAMS */

void See(CharPt name)
{
	currIn = FileStreamOpen(name, mRead, true) ;
}

void Seen()
{
	StreamClose(currIn) ;
}

CharPt StreamGetLine(StreamPt srm)
{
	if( feof(StreamFile(srm)) ) return nil ;
	UseBuffer() ;
	for(;;) {
		register int c = StreamGetChar(srm) ;
		if( c == '\n' || c == eofMark ) break ;
		BufferAddCh(c) ;
	}
	BufferAddCh('\0') ;
	return FreeBuffer() ;
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
	currOut = FileStreamOpen(name, mWrite, true) ;
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
	StreamPut(origOut, c) ;
	StreamFlush(origOut) ;
}

void StreamsSane()
{
	currIn = userIn ;
	currOut = userOut ;
}


/* CXPROLOG C'BUILTINS */

StreamPt XTestStream(register Pt t, StreamMode mode)
{
	VarValue(t) ;
	if( IsAtom(t) ) {
		if( t == tUserAtom )
			return mode == mRead  ? userIn : userOut ;
		t = IVarGet(XAtom(t)) ;
	}
	if( IsThisExtra(streamType, t) )
		return UsingStream(cStreamPt(XPt(t)), mode) ;
	TypeError2("STREAM", t) ;
	return nil ;
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

static void POpen()
{
	BindVarWithExtra(X2, FileStreamOpenP(XTestAtomName(X0), TestAtom(X1), tTextAtom)) ;
	JumpNext()
}

static void POpen2()
{
	BindVarWithExtra(X3, FileStreamOpenP(XTestAtomName(X0), TestAtom(X1), TestAtom(X2))) ;
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

static void PGet0()
{
	if( StreamIsText(currIn) ) {
		int c = StreamGetChar(currIn) ;
		if( UnifyWithNumber(X0, MakeChar(c)) ) JumpNext()
		DoFail()
	}
	else {
		int c = StreamGetByte(currIn) ;
		if( UnifyWithNumber(X0, MakeByte(c)) ) JumpNext()
		DoFail()
	}
}

static void PSGet0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsText(srm) ) {
		int c = StreamGetChar(srm) ;
		if( UnifyWithNumber(X1, MakeChar(c)) ) JumpNext()
		DoFail()
	}
	else {
		int c = StreamGetByte(srm) ;
		if( UnifyWithNumber(X1, MakeByte(c)) ) JumpNext()
		DoFail()
	}
}

static void PGet()
{
	int c = StreamGetNonBlank(currIn) ;
	if( UnifyWithNumber(X0, MakeChar(c)) ) JumpNext()
	DoFail()
}

static void PSGet()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamGetNonBlank(srm) ;
	if( UnifyWithNumber(X1, MakeChar(c)) ) JumpNext()
	DoFail()
}

static void PPeek0()
{
	if( StreamIsText(currIn) ) {
		int c = StreamPeekChar(currIn) ;
		if( UnifyWithNumber(X0, MakeChar(c)) ) JumpNext()
		DoFail()
	}
	else {
		int c = StreamPeekByte(currIn) ;
		if( UnifyWithNumber(X0, MakeByte(c)) ) JumpNext()
		DoFail()
	}
}

static void PSPeek0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	if( StreamIsText(srm) ) {
		int c = StreamPeekChar(srm) ;
		if( UnifyWithNumber(X1, MakeChar(c)) ) JumpNext()
		DoFail()
	}
	else {
		int c = StreamPeekByte(srm) ;
		if( UnifyWithNumber(X1, MakeByte(c)) ) JumpNext()
		DoFail()
	}
}

static void PPeek()
{
	int c = StreamPeekNonBlank(currIn) ;
	if( UnifyWithNumber(X0, MakeChar(c)) ) JumpNext()
	DoFail()
}

static void PSPeek()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamPeekNonBlank(srm) ;
	if( UnifyWithNumber(X0, MakeChar(c)) ) JumpNext()
	DoFail()
}

static void PSkip()
{
	int c = XTestChar(X0) ;
	while( StreamGetChar(currIn) != c ) ;
	JumpNext()
}

static void PSSkip()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = XTestChar(X1) ;
	while( StreamGetChar(srm) != c ) ;
	JumpNext()
}

static void PGetLine()
{
	CharPt str = StreamGetLine(currIn) ;
	if( str == nil ) {
		if( UnifyWithAtomic(X0, MakeChar(eofMark)) ) JumpNext()
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
		if( UnifyWithAtomic(X1, MakeChar(eofMark)) ) JumpNext()
		DoFail()
	}
	else {
		if( UnifyWithAtomic(X1, MakeTempAtom(str)) ) JumpNext()
		DoFail()
	}
}

static void PPut()
{
	if( StreamIsText(currOut) )
		StreamPut(currOut, XTestChar(X0)) ;
	else 
		StreamPut(currOut, XTestByte(X0)) ;
	JumpNext()
}

static void PSPut()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	if( StreamIsText(srm) )
		StreamPut(srm, XTestChar(X1)) ;
	else
		StreamPut(srm, XTestByte(X1)) ;
	JumpNext()
}

static void PNl()
{
	StreamPut(currOut, '\n') ;
	JumpNext()
}

static void PSNl()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamPut(srm, '\n') ;
	JumpNext()
}

static void PTab()
{
	Size n = XTestInt(X0) ; ;
	while( n-- )
		StreamPut(currOut, ' ') ;
	JumpNext()
}

static void PSTab()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	Size n = XTestInt(X1) ; ;
	while( n-- )
		StreamPut(srm, ' ') ;
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
	Write(" %s %16.16s -> %6s, %s%s",
			(srm == currIn || srm == currOut) ? "CURR" : "    ",
			StreamName(srm),
			StreamModeAsStr(StreamMode(srm)),
			XExtraAsStr(TagExtra(srm)),
			feof(StreamFile(srm)) ? " [at end of file]" : ""
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

void Streams2Init()
{
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

	InstallCBuiltinPred("open", 3, POpen) ;
	InstallCBuiltinPred("open", 4, POpen2) ;
	InstallCBuiltinPred("close", 1, PClose) ;
	InstallCBuiltinPred("set_input", 1, PSetInput) ;
	InstallCBuiltinPred("current_input", 1, PCurrentInput) ;
	InstallCBuiltinPred("set_output", 1, PSetOutput) ;
	InstallCBuiltinPred("current_output", 1, PCurrentOutput) ;

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

	InstallCBuiltinPred("set_user_streams", 3, PSetStdStreams) ;
	InstallCBuiltinPred("restore_user_streams", 0, PRestoreStdStreams) ;

	InstallCBuiltinPred("streams", 0, PStreams) ;
}

void StreamsInit()
{
	streamType = ExtraTypeNew("stream", WordsOf(Stream)) ;
	currIn = userIn = origIn = StreamNew(stdin, "_%stdin", mRead, true) ;
	currOut = userOut = origOut = StreamNew(stdout, "_%stdout", mWrite, true) ;
	userErr = origErr = StreamNew(stderr, "_%stderr", mWrite, true) ;
}
