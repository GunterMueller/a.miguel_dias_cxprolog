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
#define eofMark	0

typedef enum { mNone, mRead, mWrite, mAppend } StreamMode ;

typedef struct Stream {
	ExtraDef(Stream) ;
	FILE *file ;
	StreamMode mode ;
	Bool isText ;
	char name[maxStreamName] ;
} Stream, *StreamPt ;

extern StreamPt currIn, currOut ;

StreamPt FileStreamOpen(CharPt name, StreamMode mode, Bool isText) ;
StreamPt FILEToStream(FILE *file, StreamMode mode, CharPt prefName) ;
void StreamClose(StreamPt srm) ;

int StreamGetChar(StreamPt srm) ;
int StreamPeekChar(StreamPt srm) ;
int StreamGetNonBlank(StreamPt srm) ;
int StreamPeekNonBlank(StreamPt srm) ;
int StreamGetByte(StreamPt srm) ;
int StreamPeekByte(StreamPt srm) ;
void StreamReadN(StreamPt srm, VoidPt v, Size n) ;

void StreamPut(StreamPt srm, int c) ;
void StreamWriteN(StreamPt srm, VoidPt v, Size n) ;
void StreamWriteV(StreamPt srm, CharPt fmt, VoidPt v) ;
void StreamFlush(StreamPt srm) ;
void StreamFlushAll(void) ;

void See(CharPt name) ;
void Seen(void) ;
CharPt StreamGetLine(StreamPt srm) ;
void GetCharCommand(int *comm, int *arg) ;

void Tell(CharPt name) ;
void StreamWrite(StreamPt srm, CharPt fmt, ...) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;
void WriteErr(CharPt fmt, ...) ;
void Dot(int c) ;
void StreamsSane(void) ;

StreamPt XTestStream(register Pt t, StreamMode mode) ;

void Streams2Init(void) ;
void StreamsInit(void) ;
#endif
