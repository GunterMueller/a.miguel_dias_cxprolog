/*
 *   This file is part of the CxProlog system

 *   Stream.h
 *   by A.Miguel Dias - 2002/01/04
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stream2_
#define _Stream2_

void StreamsSane(void) ;
void StreamsCloseAll(void) ;

void See(CharPt name) ;
void Seen(void) ;
CharPt StreamGetLine(StreamPt srm) ;
void GetCharCommand(int *comm, int *arg) ;

void Tell(CharPt name) ;
void StreamWrite(StreamPt srm, CharPt fmt, ...) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;
void WriteStdFlush(CharPt fmt, ...) ;
void WriteErr(CharPt fmt, ...) ;
void WriteErrFlush(CharPt fmt, ...) ;

StreamPt XTestStream(Pt t, StreamMode mode) ;
void DefineStdStreamsIVars(void) ;
void Streams2Init(void) ;

#endif
