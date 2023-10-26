/*
 *   This file is part of the CxProlog system

 *   Stream.h
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stream_
#define _Stream_

/* STREAM */
typedef enum {
	textFileStream, binaryFileStream,
	textBufferStream, binaryBufferStream,
	interactiveStream, netStream, nullStream,
/* for internal use */
	stringStream, listStream, innerStream
} StreamKind ;

typedef enum {
	mNone, mRead, mWrite,  mAppend
} StreamMode ;

typedef enum {
	eofError, eofCode, eofReset
} EofAction ;

typedef struct Stream {
	ExtraDef(Stream) ;
	VoidPt channel ;
	AtomPt atom ;
	AtomPt path ;
	StreamKind kind ;
	StreamMode mode ;
	Bool allowReposition ;
	EofAction eofAction ;
	Bool isATty ;
	Bool eofSeen ;
	Bool isBinary ;
	Bool isEdinburgh ;
} Stream, *StreamPt ;

#define cStreamPt(s)			((StreamPt)(s))

#define StreamChannel(s)		((s)->channel)
#define StreamFILE(s)			FileAsFILE(StreamChannel(s))
#define StreamAtom(s)			((s)->atom)
#define StreamName(s)			AtomName(StreamAtom(s))
#define StreamPath(s)			((s)->path)
#define StreamKind(s)			((s)->kind)
#define StreamMode(s)			((s)->mode)
#define StreamAllowReposition(s) ((s)->allowReposition)
#define StreamEofAction(s)		((s)->eofAction)
#define StreamIsATty(s)			((s)->isATty)
#define StreamEofSeen(s)		((s)->eofSeen)
#define StreamIsBinary(s)		((s)->isBinary)
#define StreamIsEdinburgh(s)	((s)->isEdinburgh)

#define IsFileStream(s) 		( (s)->kind <= binaryFileStream )

/* PUBLIC VARS */
extern ExtraTypePt streamType ;
extern StreamPt userIn, userOut, userErr, userPrt ;
extern StreamPt currIn, currOut ;

/* OPEN & CLOSE OPERATIONS */
StreamPt FileStreamOpen(CharPt name, StreamMode mode, OpenStreamOptionsPt so) ;
StreamPt BufferStreamOpen(BufferPt buff, StreamMode mode, OpenStreamOptionsPt so) ;
StreamPt NullStreamOpen(void) ;
StreamPt StringStreamOpen(CharPt string) ;
StreamPt ListStreamOpen(Pt list) ;
StreamPt InnerStreamOpen(void) ;
StreamPt FILEToStream(FILE *file, CharPt n, StreamMode m, StreamKind k) ;
CharPt StreamClose(StreamPt srm, CloseStreamOptionsPt co) ;
void StreamRealiasing(AtomPt a, StreamPt srm) ;
CharPt StreamEncodingName(StreamPt srm) ;
Bool StreamBom(StreamPt srm) ;

/* BASIC INPUT OPERATIONS */
WChar StreamGet(StreamPt srm) ;
WChar StreamPeek(StreamPt srm) ;
Size StreamReadBytes(StreamPt srm, VoidPt v, Size n) ;
Bool StreamAtEnd(StreamPt srm) ;

/* BASIC OUTPUT OPERATIONS */
void StreamPut(StreamPt srm, WChar c) ;
void StreamPutStr(StreamPt srm, CharPt s) ;
void StreamPutStrNl(StreamPt srm, CharPt s) ;
void StreamPutStrMulti(StreamPt srm, ...) ;
void StreamPutStrSegm(StreamPt srm, CharPt s, CharPt end) ;
Size StreamWriteBytes(StreamPt srm, VoidPt v, Size n) ;
void StreamWriteV(StreamPt srm, CharPt fmt, va_list v) ;
void StreamFlush(StreamPt srm) ;

/* MORE INPUT OPERATIONS */
WChar StreamGetNonBlank(StreamPt srm) ;
WChar StreamPeekNonBlank(StreamPt srm) ;
CharPt StreamGetLine(StreamPt srm) ;
WChar GetCharCommand(CharPt prompt, int *arg) ;

/* MORE OUTPUT OPERATIONS */
void StreamWrite(StreamPt srm, CharPt fmt, ...) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;
void WriteErr(CharPt fmt, ...) ;
void WriteEOF(void) ;
void Dot(CharPt fmt, ...) ;
void StreamFlushAll(void) ;

/* TEST, EXTRACT & INIT */
Bool IsStream(Pt t) ;
StreamPt XTestStream(Pt t, StreamMode mode) ;
void StreamsSane(void) ;
void StreamsInit2(void) ;
void StreamsInit(void) ;

#endif
