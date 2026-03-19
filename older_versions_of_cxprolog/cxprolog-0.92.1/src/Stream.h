/*
 *   This file is part of the CxProlog system

 *   Stream.h
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stream_
#define _Stream_

#define eofMark	-1

/* STREAM */
typedef enum {
	textFileStream, textFileStreamInteractive, textBufferStream,
	binaryFileStream, binaryBufferStream, 
	nullStream,
/* for internal use */
	stringStream, listStream, innerStream
} StreamKind ;
 
typedef enum {
	mNone, mRead, mWrite,  mAppend
} StreamMode ;

typedef struct Stream {
	ExtraDef(Stream) ;
	VoidPt channel ;
	AtomPt atom ;
	AtomPt path ;
	CharPt locale ;
	StreamKind kind ;
	StreamMode mode ;
	Bool isBinary ;
	Bool isEdinburgh ;
	Bool isInvisible ;
} Stream, *StreamPt ;

#define cStreamPt(s)			((StreamPt)(s))

#define StreamChannel(s)		((s)->channel)
#define StreamLocale(s)			((s)->locale)
#define StreamMode(s)			((s)->mode)
#define StreamKind(s)			((s)->kind)
#define StreamIsBinary(s)		((s)->isBinary)
#define StreamIsEdinburgh(s)	((s)->isEdinburgh)
#define StreamAtom(s)			((s)->atom)
#define StreamName(s)			AtomName(StreamAtom(s))
#define StreamPath(s)			((s)->path)

#define IsFileStream(s) 		( (s)->kind <= binaryFileStream )

/* PUBLIC VARS */
extern StreamPt userIn, userOut, userErr ;
extern StreamPt currIn, currOut ;

/* OPEN & CLOSE OPERATIONS */
StreamPt FileStreamOpen(CharPt name, StreamMode mode, CharPt locale) ;
StreamPt BufferStreamOpen(BufferPt buff, StreamMode mode, CharPt locale) ;
StreamPt NullStreamOpen(void) ;
StreamPt StringStreamOpen(CharPt string) ;
StreamPt ListStreamOpen(Pt list) ;
StreamPt InnerStreamOpen(void) ;
StreamPt FILEToStream(FILE *file, StreamMode mode, CharPt prefName) ;
CharPt StreamClose(StreamPt srm) ;

/* BASIC INPUT OPERATIONS */
#define StreamGet(srm)		StreamAccess(srm, true)
#define StreamPeek(srm)		StreamAccess(srm, false)
int StreamAccess(StreamPt srm, Bool advance) ;
int StreamGetSingleChar(void) ;
Size StreamReadBytes(StreamPt srm, VoidPt v, Size n) ;
Bool StreamAtEnd(StreamPt srm) ;

/* BASIC OUTPUT OPERATIONS */
void StreamPut(StreamPt srm, int c) ;
void StreamPutStr(StreamPt srm, CharPt s) ;
void StreamPutStrNl(StreamPt srm, CharPt s) ;
void StreamPutStrMulti(StreamPt srm, ...) ;
void StreamPutStrSegm(StreamPt srm, CharPt s, CharPt end) ;
Size StreamWriteBytes(StreamPt srm, VoidPt v, Size n) ;
void StreamWriteV(StreamPt srm, CharPt fmt, va_list v) ;
void StreamFlush(StreamPt srm) ;

/* MORE INPUT OPERATIONS */
int StreamGetNonBlank(StreamPt srm) ;
int StreamPeekNonBlank(StreamPt srm) ;
CharPt StreamGetLine(StreamPt srm) ;
void GetCharCommand(int *comm, int *arg) ;

/* MORE OUTPUT OPERATIONS */
void StreamWrite(StreamPt srm, CharPt fmt, ...) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;
void WriteErr(CharPt fmt, ...) ;
void Dot(CharPt fmt, ...) ;
void StreamFlushAll(void) ;

/* TEST, EXTRACT & INIT */
Bool IsStream(Pt t) ;
StreamPt XTestStream(Pt t, StreamMode mode) ;
void StreamsSane(void) ;
void StreamsInit2(void) ;
void StreamsInit(void) ;

#endif
