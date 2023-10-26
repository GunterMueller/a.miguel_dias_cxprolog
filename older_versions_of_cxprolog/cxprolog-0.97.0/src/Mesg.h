/*
 *   This file is part of the CxProlog system

 *   Mesg.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Mesg_
#define _Mesg_

void Mesg(CharPt fmt, ...) ;
void MesgW(CharPt fmt, ...) ;
void BasicInfo(CharPt fmt, ...) ;
void Info(int level, CharPt fmt, ...) ;
void MemoryInfo(CharPt fmt, ...) ;
void MemoryGrowInfo(CharPt what, Size oldSize, Size newSize, CharPt where) ;
void Warning(CharPt fmt, ...) ;

VoidPt Error(CharPt fmt, ...) ;
VoidPt ErrorV(CharPt fmt, va_list v) ;

VoidPt TypeError(CharPt expected, Pt found) ;
int ITypeError(CharPt expected, Pt found) ;

VoidPt SyntaxError(CharPt fmt, ...) ;

VoidPt GenericError(CharPt kind, CharPt fmt, ...) ;
VoidPt ArithError(CharPt fmt, ...) ;
VoidPt FileError(CharPt fmt, ...) ;
VoidPt DatabaseError(CharPt fmt, ...) ;
VoidPt ImperativeError(CharPt fmt, ...) ;

VoidPt FatalError(CharPt fmt, ...) ;
VoidPt InternalError(CharPt fun) ;
VoidPt UndisclosedError(CharPt s) ;
int IInternalError(CharPt fun) ;

void MesgInit(void) ;

#endif
