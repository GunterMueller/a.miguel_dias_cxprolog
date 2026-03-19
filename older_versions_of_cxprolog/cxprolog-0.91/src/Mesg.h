/*
 *   This file is part of the CxProlog system

 *   Mesg.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Mesg_
#define _Mesg_

void Mesg(CharPt fmt, ...) ;
void MesgW(CharPt fmt, ...) ;
void Warning(CharPt fmt, ...) ;
void MemoryWarning(CharPt fmt, ...) ;
void MemoryGrowWarning(CharPt what, Size oldSize, Size newSize, CharPt where) ;

VoidPt Error(CharPt fmt, ...) ;
VoidPt ErrorV(CharPt fmt, va_list v) ;

VoidPt TypeError(CharPt fmt, ...) ;
VoidPt TypeError2(CharPt kind, Pt t) ;
int ITypeError2(CharPt kind, Pt t) ;
VoidPt TypeError3(CharPt kind, CharPt alt, Pt t) ;

VoidPt GenericError(CharPt kind, CharPt fmt, ...) ;
VoidPt ArithError(CharPt fmt, ...) ;
VoidPt FileError(CharPt fmt, ...) ;
VoidPt DatabaseError(CharPt fmt, ...) ;
VoidPt ImperativeError(CharPt fmt, ...) ;

VoidPt FatalError(CharPt fmt, ...) ;
VoidPt InternalError(CharPt fun) ;
int IInternalError(CharPt fun) ;

#endif
