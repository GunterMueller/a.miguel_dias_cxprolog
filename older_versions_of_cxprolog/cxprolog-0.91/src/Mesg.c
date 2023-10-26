/*
 *   This file is part of the CxProlog system

 *   Mesg.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

static void ErrorMesgV(CharPt kind, Bool showPred, CharPt fmt, va_list v)
{	/* The memory manager may have not been initialized yet.
	   So, cannot relly on GStrFormat here. */
	Str1K buff ;
 	PredicatePt pr ;
	
	strcpy(buff, kind == nil ? cCharPt("") : kind ) ;
	if( showPred && P != &NotRunning && (pr = CurrCPred()) != nil )
		sprintf(buff + strlen(buff), " (%s)", PredNameArity(pr)) ;
	if( 0 && errno != 0 ) {
		sprintf(buff + strlen(buff), "[%d]", errno) ;
		errno = 0 ;
	}

	WriteErr(strlen(buff) > 0 ? "{%s: " : "{", buff) ;
	StreamWriteV(userErr, fmt, v) ;
	WriteErr(".}\n") ;
}

static VoidPt ErrorEventV(CharPt kind, CharPt fmt, va_list v)
{
	errno = 0 ;
	ScratchRestart() ;
	switch( failOnError_flag ) {
		case 0:
			if( Booting() ) {
				ErrorMesgV("FATAL BOOTING ERROR", true, fmt, v) ;
				EventHalt() ;
			}
			else ThrowPrologExceptionMesgV(kind, fmt, v) ;
		case 1:
			ErrorMesgV(kind, true, fmt, v) ;
			EventForceFail() ;
		case 2:
			EventForceFail() ;
		default: InternalError("ErrorEventV") ;
	}
	return nil ;
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
/* sys_trace on forces memoryWarnings */
	if( memoryWarnings_flag || sysTrace_flag != 0 ) {
		va_start(_p, fmt) ;
		ErrorMesgV("MEMORY", false, fmt, _p) ;
	}
}

void MemoryGrowWarning(CharPt what, Size oldSize, Size newSize, CharPt where)
{
	CharPt o, n ;
	if( oldSize < 1 K )
		o = GStrFormat("%ld bytes", WordsAsBytes(oldSize)) ;
	else o = GStrFormat("%ldKB", WordsAsKBytes(oldSize)) ;
	if( newSize < 1 K )
		n = GStrFormat("%ld bytes", WordsAsBytes(newSize)) ;
	else n = GStrFormat("%ldKB", WordsAsKBytes(newSize)) ;
	if( where == nil )
		where = GetInstInfo(P[-1], nil) ;

	MemoryWarning("Expanding %s from %s to %s  [%s]", what, o, n, where) ;
}

VoidPt TypeError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	return ErrorEventV("TYPE ERROR", fmt, _p) ;
}

VoidPt TypeError2(CharPt kind, Pt t)
{
	if( t == nil )
		return TypeError("%s expected", kind) ;
	else
		return TypeError("%s expected, found '%s'", kind, TermAsStr(t)) ;
}

int ITypeError2(CharPt kind, Pt t)
{
	TypeError2(kind, t) ;
	return 0 ;
}

VoidPt TypeError3(CharPt kind, CharPt alt, Pt t)
{
	CharPt s = GStrFormat("%s or %s", kind, alt) ;
	return TypeError2(s, t) ;
}

VoidPt Error(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	return ErrorEventV("ERROR", fmt, _p) ;
}

VoidPt ErrorV(CharPt fmt, va_list v)
{
	ErrorEventV("ERROR", fmt, v) ;
	return nil ;
}

VoidPt GenericError(CharPt kind, CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	return ErrorEventV(kind, fmt, _p) ;
}

VoidPt ArithError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	return ErrorEventV("ERROR", fmt, _p) ;
}

VoidPt FileError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	return ErrorEventV("ERROR", fmt, _p) ;
}

VoidPt DatabaseError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	return ErrorEventV("ERROR", fmt, _p) ;

}

VoidPt ImperativeError(CharPt fmt, ...)
{
	va_start(_p, fmt) ;
	return ErrorEventV("ERROR", fmt, _p) ;
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
    ErrorMesgV("INTERNAL ERROR", true, GStrFormat("At function %s", fun), nil) ;
    EventHalt() ;
	return nil ;
}

int IInternalError(CharPt fun)
{
    InternalError(fun) ;
	return 0 ;
}
