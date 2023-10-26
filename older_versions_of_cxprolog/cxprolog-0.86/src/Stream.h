/*
 *   This file is part of the CxProlog system

 *   Stream.h
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stream_
#define _Stream_

#define maxStreamName 255
#define eofMark	-1

typedef enum {
	textFileStream, binaryFileStream, nullStream, stringStream, bufferStream,
	localeTextFileStream
} StreamKind ;
                                                                                
typedef enum {
	mNone, mRead, mWrite,  mAppend
} StreamMode ;

typedef struct Stream {
	ExtraDef(Stream) ;
	VoidPt channel ;
	CharPt locale ;
	StreamKind kind ;
	StreamMode mode ;
	Bool isBinary ;
	char name[maxStreamName] ;
} Stream, *StreamPt ;

extern StreamPt userIn, userOut, userErr ;
extern StreamPt currIn, currOut ;

StreamPt FileStreamOpen(CharPt name, StreamMode mode, CharPt locale) ;
StreamPt NullStreamOpen(void) ;
StreamPt StringStreamOpen(CharPt string) ;
StreamPt BufferStreamOpen(void) ;
StreamPt FILEToStream(FILE *file, StreamMode mode, CharPt prefName) ;

int StreamGetChar(StreamPt srm) ;
int StreamPeekChar(StreamPt srm) ;
void StreamReadN(StreamPt srm, VoidPt v, Size n) ;

void StreamPutChar(StreamPt srm, int c) ;
void StreamPutStr(StreamPt srm, CharPt s) ;
void StreamWriteN(StreamPt srm, VoidPt v, Size n) ;
void StreamWriteV(StreamPt srm, CharPt fmt, VoidPt v) ;
void StreamFlush(StreamPt srm) ;
CharPt StreamClose(StreamPt srm) ;

void See(CharPt name) ;
void Seen(void) ;
int StreamGetNonBlank(StreamPt srm) ;
int StreamPeekNonBlank(StreamPt srm) ;
CharPt StreamGetLine(StreamPt srm) ;
void GetCharCommand(int *comm, int *arg) ;

void Tell(CharPt name) ;
void StreamWrite(StreamPt srm, CharPt fmt, ...) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;
void WriteErr(CharPt fmt, ...) ;
void Dot(int c) ;
void StreamsSane(void) ;
void StreamFlushAll(void) ;

StreamPt XTestStream(register Pt t, StreamMode mode) ;

void StreamsInit2(void) ;
void StreamsInit(void) ;
#endif
