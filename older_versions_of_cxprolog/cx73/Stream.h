/*
 *   This file is part of the CxProlog system

 *   Stream.h
 *   by A.Miguel Dias - 1989/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Stream_
#define _Stream_

#define dflInputStream	0
#define dflOutputStream 1

void InitStreams(void) ;
void RecoverStreams(void) ;
void CloseAllStreams(void) ;
void Flush(CharPt name) ;
void Seen(void) ;
void Told(void) ;
CharPt Seeing(void) ;
CharPt Telling(void) ;
void See(CharPt name) ;
void Tell(CharPt name) ;
void Put(int c) ;
void PutString(CharPt s) ;
void Nl(void) ;
void Tab(int n) ;
void Write(CharPt fmt, ...) ;
void WriteStd(CharPt fmt, ...) ;
void Prompt(CharPt s) ;
CharPt GetLine(void) ;
int Get0(void) ;
int Get(void) ;
CharPt GetLine() ;
void Skip(int c) ;
int Peek0(void) ;
void SetTempOutput(int id) ;
void RestoreOutput(void) ;
void GetCharCommand(int *comm, int *arg) ;

#endif
