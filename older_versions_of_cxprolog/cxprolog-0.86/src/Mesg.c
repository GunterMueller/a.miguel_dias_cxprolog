/*
 *   This file is part of the CxProlog system

 *   Mesg.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

static void ErrorMesgV(CharPt kind, Bool showPred, CharPt fmt, VoidPt v)
{
	Str256 predName, errN ;
	Str1000 theMesg ;
	PredicatePt pr ;

	errno = 0 ;
	vsprintf(theMesg, fmt, v) ; /* Must be here */

	strcpy(predName, "") ;
	if( showPred && P != &NotRunning && (pr = FindCPredByInst(P[-1], true)) != nil )
			sprintf(predName, " (%s)", UPredNameArity(pr)) ;

	strcpy(errN, "") ;
	if( 0 && errno != 0 ) {
		sprintf(errN, "[%d]", errno) ;
		errno = 0 ;
	}

	if( kind == nil ) WriteStd("{%s.}\n", theMesg) ;
	else WriteErr("{%s%s%s: %s.}\n", kind, errN, predName, theMesg) ;
}

static void ErrorActionV(CharPt kind, CharPt fmt, VoidPt v)
{
	errno = 0 ;
	switch( failOnError_flag ) {
		case 0:
			FreeBuffer() ; /* needed for a reason */
			Throw(BuildExceptionTerm(kind, fmt, v)) ;
		case 1:
			ErrorMesgV(kind, true, fmt, v) ;
			EventForceFail() ;
		case 2:
			EventForceFail() ;
		default: InternalError("ErrorActionV") ;
	}
}

static void ErrorMesg(CharPt kind, Bool pred, CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorMesgV(kind, pred, fmt, _p) ;
}

void Mesg(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorMesgV(nil, false, fmt, _p) ;
}

void MesgW(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorMesgV(nil, false, fmt, _p) ;
	getchar() ;
}

void Warning(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorMesgV("WARNING", false, fmt, _p) ;
}

void MemoryWarning(CharPt fmt, ...)
{
	InterruptHandle() ;
	if( !memoryWarnings_flag ) return ;
	va_start(_p, fmt) ;
	ErrorMesgV("MEMORY", false, fmt, _p) ;
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
	ErrorActionV("ERROR", fmt, _p) ;
	return nil ;
}

VoidPt TypeError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorActionV("TYPE ERROR", fmt, _p) ;
	return nil ;
}

VoidPt TypeError2(CharPt kind, Pt t)
{
    if( t == nil ) TypeError("%s expected", kind) ;
    else TypeError("%s expected, seen %s - %s", kind, TermTypeStr(t), TermAsStr(t)) ;
	return nil ;
}

int ITypeError2(CharPt kind, Pt t)
{
	TypeError2(kind, t) ;
	return 0 ;
}

VoidPt ArithError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorActionV("ERROR", fmt, _p) ;
	return nil ;
}

VoidPt FileError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorActionV("ERROR", fmt, _p) ;
	return nil ;
}

VoidPt DatabaseError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorActionV("ERROR", fmt, _p) ;
	return nil ;
}

VoidPt ImperativeError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorActionV("ERROR", fmt, _p) ;
	return nil ;
}

VoidPt FatalError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	ErrorMesgV("FATAL ERROR", true, fmt, _p) ;
	EventHalt() ;
	return nil ;
}

VoidPt InternalError(CharPt fun)
{
    ErrorMesg("INTERNAL ERROR", true, "At function %s", fun) ;
    EventHalt() ;
	return nil ;
}

int IInternalError(CharPt fun)
{
    InternalError(fun) ;
	return 0 ;
}
