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

#define eofCode	26
#define maxStreamName 255

typedef enum { fileStream, nullStream } StreamType ;
typedef enum { mNone, mRead, mWrite, mAppend } StreamMode ;

typedef struct Stream
{
	short tagHolder ;
	short inUse ;
	struct Stream *next ;
	StreamType type ;
	StreamMode mode ;
	FILE *file ;
	char name[maxStreamName] ;
} Stream, *StreamPt ;

extern StreamPt userIn, userOut, userErr, currIn, currOut ;

void StreamsInit(void) ;
StreamPt UsingStream(StreamPt srm, StreamMode mode) ;
CharPt StreamTypeAtStr(StreamType st) ;
CharPt StreamModeAtStr(StreamMode sm) ;

StreamPt StreamOpen(CharPt name, StreamMode mode, StreamType type) ;
void StreamClose(StreamPt srm) ;

int StreamGet(StreamPt srm) ;
int StreamGetNonBlank(StreamPt srm) ;
void StreamUnget(StreamPt srm, int c) ;
Bool StreamAtEnd(StreamPt srm) ;

void StreamPut(StreamPt srm, int c) ;
void StreamFlush(StreamPt srm) ;

#endif
