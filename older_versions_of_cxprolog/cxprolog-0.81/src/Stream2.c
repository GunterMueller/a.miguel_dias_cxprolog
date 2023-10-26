/*
 *   This file is part of the CxProlog system

 *   Stream2.c
 *   by A.Miguel Dias - 2002/01/04
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

void StreamsSane()
{
	currIn = userIn ;
	currOut = userOut ;
}

static void StreamsCloseAllAux(VoidPt x)
{
	StreamClose(cStreamPt(x)) ;
}
void StreamsCloseAll()
{
	ExtraForEach(streamType, StreamsCloseAllAux) ;
}


/* INPUT STREAMS */

void See(CharPt name)
{
	currIn = StreamOpen(name, mRead, fileStream) ;
}

CharPt StreamGetLine(StreamPt srm)
{
	if( StreamAtEnd(srm) ) return nil ;
	UseBuffer() ;
	for(;;) {
		register int c = StreamGet(srm) ;
		if( c == '\n' || c == eofCode ) break ;
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
	while( ( c = StreamGet(userIn) ) <= ' ' && c != '\n' && not cx_iseof(c) ) ;
	*comm = InRange(c,'A','Z') ? (c - 'A' + 'a') : c ;
	hasArg = 0 ;
	n = 0 ;
	while( c != '\n' && not cx_iseof(c) ) {
		if( InRange(c, '0', '9') ) {
			hasArg = true ;
			n = n * 10 + c - '0' ;
		}
		c = StreamGet(userIn) ;
	}
	*arg = hasArg ? n : -1 ;
	while( c != '\n' && not cx_iseof(c) )
		c = StreamGet(userIn) ;
}


/* OUTPUT STREAMS */

void Tell(CharPt name)
{
	currOut = StreamOpen(name, mWrite, fileStream) ;
}

void StreamWrite(StreamPt srm, CharPt fmt, ...)
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
	StreamClose(currIn) ;
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

static void POpen()
{
	CharPt name = XTestAtomName(X0) ;
	CharPt mode = XTestAtomName(X1) ;
	Pt t = Drf(X2) ;
	StreamMode m ;

	if( EqualStr(mode, "read") )
		m = mRead ;
	elif( EqualStr(mode, "write") )
		m = mWrite ;
	elif( EqualStr(mode, "append") )
		m = mAppend ;
	else FileError("Invalid stream mode") ;

	if( IsVar(t) ) {
		StreamPt srm = StreamOpen(name, m, fileStream) ;
		if( UnifyWithAtomic(t, TagExtra(srm)) ) JumpNext()
		InternalError("POpen") ;
	}
	if( IsAtom(t) ) {
		StreamPt srm = StreamOpen(name, m, fileStream) ;
		IVarSet(XAtom(t), TagExtra(srm)) ;
		JumpNext()
	}
	TypeError2("VAR or IVAR", t) ;
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
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		if( UnifyWithAtomic(t, TagExtra(currIn)) ) JumpNext()
		InternalError("PCurrentInput") ;
	}
	if( IsAtom(t) ) {
		IVarSet(XAtom(t), TagExtra(currIn)) ;
		JumpNext()
	}
	TypeError2("VAR or IVAR", t) ;
}

static void PSetOutput()
{
	currOut = XTestStream(X0, mWrite) ;
	JumpNext()
}

static void PCurrentOutput()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		if( UnifyWithAtomic(t, TagExtra(currOut)) ) JumpNext()
		InternalError("PCurrentInput") ;
	}
	if( IsAtom(t) ) {
		IVarSet(XAtom(t), TagExtra(currOut)) ;
		JumpNext()
	}
	TypeError2("VAR or IVAR", t) ;
}

static void PGet0()
{
	int c = StreamGet(currIn) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSGet0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamGet(srm) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X1, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PGet()
{
	int c = StreamGetNonBlank(currIn) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSGet()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamGetNonBlank(srm) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X1, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PPeek0()
{
	int c = StreamGet(currIn) ;
	StreamUnget(currIn, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSPeek0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamGet(srm) ;
	StreamUnget(srm, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X1, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PPeek()
{
	int c = StreamGetNonBlank(currIn) ;
	StreamUnget(currIn, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSPeek()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = StreamGetNonBlank(srm) ;
	StreamUnget(srm, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSkip()
{
	int c = XTestInt(X0) ;
	if( c == 10 ) c = '\n' ;
	while( StreamGet(currIn) != c ) ;
	JumpNext()
}

static void PSSkip()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = XTestInt(X1) ;
	if( c == 10 ) c = '\n' ;
	while( StreamGet(srm) != c ) ;
	JumpNext()
}

static void PGetLine()
{
	CharPt str = StreamGetLine(currIn) ;
	if( str == nil ) {
		if( UnifyWithAtomic(X0, MakeInt(eofCode)) ) JumpNext()
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
		if( UnifyWithAtomic(X1, MakeInt(eofCode)) ) JumpNext()
		DoFail()
	}
	else {
		if( UnifyWithAtomic(X1, MakeTempAtom(str)) ) JumpNext()
		DoFail()
	}
}

static void PPut()
{
	StreamPut(currOut, XTestInt(X0)) ;
	JumpNext()
}

static void PSPut()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamPut(srm, XTestInt(X1)) ;
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
}

static void PSTab()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	Size n = XTestInt(X1) ; ;
	while( n-- )
		StreamPut(srm, ' ') ;
}

static void PStreamsAux(VoidPt x)
{
	register StreamPt srm = cStreamPt(x) ;
	AtomPt at ;
	Write(" %s %14.14s -> %5s, %6s, %s%s",
			(srm == currIn || srm == currOut) ? "CURR" : "    ",
			StreamName(srm),
			StreamTypeAtStr(StreamType(srm)),
			StreamModeAtStr(StreamMode(srm)),
			XExtraAsStr(TagExtra(srm)),
			StreamAtEnd(srm) ? ", at end of file" : ""
	) ;
	at = IVarWith(TagExtra(srm)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
}
static void PStreams()
{
	VersionShow() ;
	Write("Streams:\n") ;
	ExtraForEach(streamType, PStreamsAux) ;
	JumpNext()
}

void Streams2Init()
{
	InstallCBuiltinPred("stream", 1, PStreamCheck) ;

	InstallCBuiltinPred("see", 1, PSee) ;
	InstallCBuiltinPred("seeing", 1, PSeeing) ;
	InstallCBuiltinPred("seen", 0, PSeen) ;

	InstallCBuiltinPred("tell", 1, PTell) ;
	InstallCBuiltinPred("telling", 1, PTelling) ;
	InstallCBuiltinPred("told", 0, PTold) ;
	InstallCBuiltinPred("flush", 0, PFlush) ;
	InstallCBuiltinPred("flush", 1, PSFlush) ;

	InstallCBuiltinPred("open", 3, POpen) ;
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

	InstallCBuiltinPred("streams", 0, PStreams) ;
}
