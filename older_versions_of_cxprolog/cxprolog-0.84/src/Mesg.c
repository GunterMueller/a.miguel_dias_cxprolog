/*
 *   This file is part of the CxProlog system

 *   Mesg.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

static void DoVErrMesg(CharPt kind, Bool showPred, CharPt fmt, VoidPt v)
{
	extern int errno ;
	Str256 predName, errN ;
	Str1000 theMesg ;
	CharPt s ;

	vsprintf(theMesg, fmt, v) ; /* Must be here */

	strcpy(predName, "") ;
	if( showPred && P != &NotRunning ) {
		if( P[-1] == CallVar || P[-1] == ExecuteVar )
			strcat(predName, " (call/1)") ;
		elif( (s = CPredNameArity(P[-1])) != nil )
			sprintf(predName, " (%s)", s) ;
	}

	strcpy(errN, "") ;
	if( 0 && errno != 0 ) {
		sprintf(errN, "[%d]", errno) ;
		errno = 0 ;
	}

	if( kind == nil ) WriteStd("{%s.}\n", theMesg) ;
	else WriteErr("{%s%s%s: %s.}\n", kind, errN, predName, theMesg) ;
}

static void DoErrMesg(CharPt kind, Bool pred, CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVErrMesg(kind, pred, fmt, _p) ;
}

static void DoVError(CharPt kind, CharPt fmt, VoidPt v)
{
	if( failOnError_flag < 2 )
		DoVErrMesg(kind, true, fmt, v) ;
	if( failOnError_flag > 0 )
		EventForceFail() ;
	EventRestart() ;
}

void Mesg(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVErrMesg(nil, false, fmt, _p) ;
}

void MesgW(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVErrMesg(nil, false, fmt, _p) ;
	getchar() ;
}

void Warning(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	DoVErrMesg("WARNING", false, fmt, _p) ;
}

void MemoryWarning(CharPt fmt, ...)
{
	InterruptHandle() ;
	if( !memoryWarnings_flag ) return ;
	va_start(_p, fmt) ;
	DoVErrMesg("MEMORY", false, fmt, _p) ;
}

void MemoryGrowWarning(CharPt what, Size oldSize, Size newSize, CharPt where)
{
	if( where )
		MemoryWarning("Expanding %s from %ldKb to %ldKb  [%s]",
			what, WordsAsKBytes(oldSize), WordsAsKBytes(newSize), where, P) ;
	else
		MemoryWarning("Expanding %s from %ldKb to %ldKb",
			what, WordsAsKBytes(oldSize), WordsAsKBytes(newSize), P) ;
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
	DoVErrMesg("FATAL ERROR", true, fmt, _p) ;
	EventHalt() ;
}

void InternalError(CharPt fun)
{
	DoErrMesg("INTERNAL ERROR", true, "At function %s", fun) ;
	EventHalt() ;
}

void Default(CharPt fun)
{
	DoErrMesg("INTERNAL ERROR", true, "Default of function %s", fun) ;
	EventHalt() ;
}
