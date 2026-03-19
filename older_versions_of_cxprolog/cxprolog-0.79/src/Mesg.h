/*
 *   This file is part of the CxProlog system

 *   Mesg.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Mesg_
#define _Mesg_

void Mesg(CharPt fmt, ...) ;
void MesgW(CharPt fmt, ...) ;
void Warning(CharPt fmt, ...) ;
void MemoryWarning(CharPt fmt, ...) ;
void MemoryGrowWarning(CharPt what, Size oldSize, Size newSize) ;
void Error(CharPt fmt, ...) ;
VoidPt RError(CharPt fmt, ...) ;
void ArithError(CharPt fmt, ...) ;
void TypeError(CharPt kind, Pt t) ;
void FatalError(CharPt fmt, ...) ;
void SysErrorN(CharPt fmt, ...) ;
void SysFatalError(CharPt fmt, ...) ;
void InternalError(CharPt fun) ;
void Default(CharPt fun) ;

#endif
