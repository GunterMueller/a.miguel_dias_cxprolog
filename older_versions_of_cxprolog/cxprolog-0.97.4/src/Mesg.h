/*
 *   This file is part of the CxProlog system

 *   Mesg.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2009 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Mesg_
#define _Mesg_

void Mesg(Str fmt, ...) ;
void MesgP(Str fmt, ...) ;
void MesgW(Str fmt, ...) ;
void BasicInfo(Str fmt, ...) ;
void Info(int level, Str fmt, ...) ;
void MemoryInfo(Str fmt, ...) ;
void MemoryGrowInfo(Str what, Size oldSize, Size newSize) ;
void Warning(Str fmt, ...) ;

VoidPt Error(Str fmt, ...) ;
VoidPt ErrorV(Str fmt, va_list v) ;
int IError(Str fmt, ...) ;

VoidPt TypeError(Str expected, Pt found) ;
int ITypeError(Str expected, Pt found) ;

VoidPt SyntaxError(Str fmt, ...) ;

VoidPt GenericError(Str kind, Str fmt, ...) ;
VoidPt ArithError(Str fmt, ...) ;
VoidPt FileError(Str fmt, ...) ;
VoidPt DatabaseError(Str fmt, ...) ;
VoidPt ImperativeError(Str fmt, ...) ;

VoidPt FatalError(Str fmt, ...) ;
VoidPt InternalError(Str fun) ;
VoidPt UndisclosedError(Str s) ;
int IInternalError(Str fun) ;

void MesgInit(void) ;

#endif
