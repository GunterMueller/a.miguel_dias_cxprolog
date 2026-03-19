/*
 *   This file is part of the CxProlog system

 *   Mesg.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

static va_list _p ;	

static void DoVMesg(CharPt kind, Bool showPred, CharPt fmt, VoidPt v)
{
	CharPt s = nil ;
	Str1000 theMesg ;
	vsprintf(theMesg, fmt, v) ;
	StreamFlush(userOut) ;
	if( showPred && P != nil ) {
		if( P[-1] == CallVar || P[-1] == ExecuteVar )
			s = "call/1" ;
		else s = CPredNameArity(P[-1]) ;
	}
	if( kind == nil ) WriteStd("{%s}\n", theMesg) ;
	elif( s == nil ) WriteStd("{%s: %s}\n", kind, theMesg) ;
	else WriteStd("{%s (%s): %s}\n", kind, s, theMesg) ;
}

static void DoMesg(CharPt kind, Bool pred, CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVMesg(kind, pred, fmt, _p) ;
}

static void DoVError(CharPt kind, CharPt fmt, VoidPt v)
{
	if( failOnError_flag )
		EventForceFail() ;
	else {
		DoVMesg(kind, true, fmt, v) ;
		EventRestart() ;
	}
}

void Mesg(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVMesg(nil, false, fmt, _p) ;
}

void MesgW(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVMesg(nil, false, fmt, _p) ;
	getchar() ;
}

void Warning(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVMesg("WARNING", false, fmt, _p) ;
}

void MemoryWarning(CharPt fmt, ...)
{
	InterruptHandle() ;
	if( not memoryWarnings_flag ) return ;
	va_start(_p, fmt) ;
	DoVMesg("MEMORY", false, fmt, _p) ;
}

void MemoryGrowWarning(CharPt what, Size oldSize, Size newSize)
{
	MemoryWarning("Expanding %s from %ldKb to %ldKb",
			what, WordsAsKBytes(oldSize), WordsAsKBytes(newSize)) ;
}

VoidPt Error(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVError("ERROR", fmt, _p) ;
	return nil ;
}

void TypeError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVError("TYPE ERROR", fmt, _p) ;
}

void ArithError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVError("ERROR", fmt, _p) ;
}

void FileError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVError("ERROR", fmt, _p) ;
}

void DatabaseError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVError("ERROR", fmt, _p) ;
}

void ImperativeError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVError("ERROR", fmt, _p) ;
}

void TypeError2(CharPt kind, Pt t)
{
	if( t == nil ) TypeError("%s expected", kind) ;
	else TypeError("%s expected, seen %s", kind, TermTypeStr(t)) ;
}

void FatalError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVMesg("FATAL ERROR", true, fmt, _p) ;
	EventHalt() ;
}

void InternalError(CharPt fun)
{
	DoMesg("INTERNAL ERROR", true, "At function %s", fun) ;
	EventHalt() ;
}

void Default(CharPt fun)
{
	DoMesg("INTERNAL ERROR", true, "Default of function %s", fun) ;
	EventHalt() ;
}

void SysErrorN(CharPt fmt, ...)
{
/*
	Str1000 s ;
	extern int errno ;
	perror(nil) ;
	sprintf(s, "%d", errno) ;
	DoMesg("ERRNO", s) ;
	va_start(_p, fmt) ;
	vsprintf(s, fmt, p) ;
	DoMesg("SYSTEM ERROR", s) ;
	EventRestart() ;*/
}

void SysFatalError(CharPt fmt, ...)
{
/*
	Str1000 s ;
	extern int errno ;
	perror(nil) ;
	sprintf(s, "%d", errno) ;
	DoMesg("ERRNO", s) ;
	va_start(_p, fmt) ;
	vsprintf(s, fmt, p) ;
	DoMesg("SYSTEM FATAL ERROR", s) ;
	EventHalt() ;*/
}
