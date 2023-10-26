/*
 *   This file is part of the CxProlog system

 *   Stream.h
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stream_
#define _Stream_

#define maxStreamName 255

typedef enum { mNone, mRead, mWrite, mAppend } StreamMode ;

typedef struct Stream {
	ExtraDef(Stream) ;
	FILE *file ;
	StreamMode mode ;
	char name[maxStreamName] ;
} Stream, *StreamPt ;

extern StreamPt currIn, currOut ;

StreamPt FileStreamOpen(CharPt name, StreamMode mode) ;
StreamPt FILEToStream(FILE *file, StreamMode mode, CharPt prefName) ;
void StreamClose(StreamPt srm) ;

int StreamGet(StreamPt srm) ;
int StreamGetNonBlank(StreamPt srm) ;
void StreamUnget(StreamPt srm, int c) ;
void StreamReadN(StreamPt srm, VoidPt v, Size n) ;
Bool StreamAtEnd(StreamPt srm) ;

void StreamPut(StreamPt srm, int c) ;
void StreamWriteV(StreamPt srm, CharPt fmt, VoidPt v) ;
void StreamWriteN(StreamPt srm, VoidPt v, Size n) ;
void StreamFlush(StreamPt srm) ;

void See(CharPt name) ;
void Seen(void) ;
int StreamPeek(StreamPt srm) ;
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
