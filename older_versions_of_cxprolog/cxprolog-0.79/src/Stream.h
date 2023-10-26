/*
 *   This file is part of the CxProlog system

 *   Stream.h
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stream_
#define _Stream_

#define eofCode	26
#define maxStreamName 255

typedef enum { notInUse, fileStream, nullStream, bufferStream } StreamType ;
typedef enum { mNone, mRead, mWrite, mAppend } StreamMode ;

typedef struct Stream
{
	Word tagHolder ;
	StreamType type ;
	StreamMode mode ;
	FILE *file ;
	Char name[maxStreamName] ;
} Stream, *StreamPt ;

#define StreamTagHolder(s)		((s)->tagHolder)
#define StreamType(s)			((s)->type)
#define StreamMode(s)			((s)->mode)
#define StreamFile(s)			((s)->file)
#define StreamName(s)			((s)->name)

#define cStreamPt(x)			((StreamPt)(x))

extern StreamPt userIn, userOut, userErr, currIn, currOut ;

void StreamsInit(void) ;
Bool StreamCheck(VoidPt ref) ;
StreamPt XTestStream(Pt t, StreamMode mode) ;
void StreamsSane(void) ;
StreamPt StreamFileOpen(CharPt name, StreamMode mode) ;
void StreamClose(StreamPt srm) ;
void StreamsCloseAll(void) ;
void StreamsShow(void) ;

void See(CharPt name) ;
Bool StreamAtEnd(StreamPt srm) ;
int StreamGet(StreamPt srm) ;
int StreamGetNonBlank(StreamPt srm) ;
void StreamUnget(StreamPt srm, int c) ;
CharPt StreamGetLine(StreamPt srm) ;

void Tell(CharPt name) ;
void StreamFlush(StreamPt srm) ;
void StreamPut(StreamPt srm, int c) ;
void StreamWrite(StreamPt srm, CharPt fmt, ...) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;

#endif
