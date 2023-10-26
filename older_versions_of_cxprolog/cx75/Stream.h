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

typedef enum { notInUse, fileStream } StreamType ;
typedef enum { mNone, mRead, mWrite, mAppend } StreamMode ;

typedef struct Stream
{
	Word tagHolder ;
	StreamType type ;
	StreamMode mode ;
	FILE *file ;
	char name[maxStreamName] ;
} Stream, *StreamPt ;

#define StreamTagHolder(s)		((s)->tagHolder)
#define StreamType(s)			((s)->type)
#define StreamMode(s)			((s)->mode)
#define StreamFile(s)			((s)->file)
#define StreamName(s)			((s)->name)

#define cStreamPt(x)			((StreamPt)(x))

extern StreamPt userIn, userOut, userErr, currIn, currOut ;

void InitStreams(void) ;
Bool StreamCheck(VoidPt ref) ;
StreamPt XTestStream(Pt t, StreamMode mode) ;
void SaneStreams(void) ;
CharPt NameOfStream(StreamPt srm) ;
StreamPt OpenFileStream(CharPt name, StreamMode mode) ;
void CloseStream(StreamPt srm) ;
void CloseAllStreams(void) ;
void ShowStreams(void) ;

void See(CharPt name) ;
Bool EndOfStream(StreamPt srm) ;
int GetStream(StreamPt srm) ;
int GetStreamNonBlank(StreamPt srm) ;
void UngetStream(StreamPt srm, int c) ;
CharPt GetLineStream(StreamPt srm) ;

void Tell(CharPt name) ;
void FlushStream(StreamPt srm) ;
void PutStream(StreamPt srm, int c) ;
void WriteStream(StreamPt srm, CharPt fmt, ...) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;

#endif
