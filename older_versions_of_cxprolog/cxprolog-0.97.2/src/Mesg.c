/*
 *   This file is part of the CxProlog system

 *   Mesg.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL

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

typedef enum {
	instantiationErr, typeErr, domainErr, existenceErr,
	permissionErr, representationErr,  evaluationErr,
	resourceErr,  syntaxErr,  systemErr, genErr
} ErrorKind ;

static void ErrorMesgV(CharPt kind, Bool showPred, CharPt fmt, va_list v)
{	/* The memory manager may have not been initialized yet.
	   So, cannot relly on GStrFormat here. */
	Str1K newFmt ;
	PredicatePt pr ;

	if( fmt == nil ) fmt = "(null)" ;

	strcpy(newFmt, "{") ;
	if( kind != nil )
		strcat(newFmt, kind) ;
	if( showPred && (pr = CurrCPred()) != nil )
		sprintf(newFmt + strlen(newFmt), " (%s)", PredNameArity(pr)) ;
	if( strlen(newFmt) > 1 )
		strcat(newFmt, ": ") ;
	strcat(newFmt, fmt) ;
#if 0
	if( errno != 0 ) {
		sprintf(newFmt + strlen(newFmt), " (%s)", sys_errlist[errno]) ;
		errno = 0 ;
	}
#endif
	strcat(newFmt, ".}\n") ;
	StreamWriteV(userErr, newFmt, v) ;
}

static VoidPt ErrorEventV(CharPt kind, CharPt fmt, va_list v)
{
	ScratchRestart() ;
	EncodingsRestart() ;
	switch( onError_flag ) {
		case 0: {
			if( Booting() ) {
				ErrorMesgV("ERROR DURING BOOT", true, fmt, v) ;
				EventFatalError() ;
			}
			else Throw(BuildExceptionTermV(kind, fmt, v)) ;
		}
		case 1: {
			ErrorMesgV(kind, true, fmt, v) ;
			EventForceFail() ;
		}
		case 2: {
			EventForceFail() ;
		}
		case 3: {
			PredicatePt onError = LookupPredicateForMetaCall(
										LookupFunctorByName("on_error", 1)) ;
			X0 = BuildExceptionTermV(kind, fmt, v) ;
			P = PredCode(onError) ;
			EventContinue() ;
		}
		default: InternalError("ErrorEventV") ;
	}
	return nil ;
}

void Mesg(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	ErrorMesgV(nil, false, fmt, v) ;
}

void MesgP(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	vfprintf(stderr, fmt, v) ;
	fprintf(stderr, "\n") ;
}

void MesgW(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	ErrorMesgV(nil, false, fmt, v) ;
	getchar() ;
}

void BasicInfo(CharPt fmt, ...)
{
	if( infoMessages_flag >= 0 || mesg_debugging ) {
		va_list v ;
		va_start(v, fmt) ;
		WriteErr("%% ") ;
		StreamWriteV(userErr, fmt, v) ;
		WriteErr(".\n") ;
	}
}

void Info(int level, CharPt fmt, ...)
{
	if( infoMessages_flag >= level || mesg_debugging ) {
		va_list v ;
		va_start(v, fmt) ;
		ErrorMesgV("INFO", false, fmt, v) ;
	}
}

void MemoryInfo(CharPt fmt, ...)
{
	InterruptHandle() ;
	if( memoryWarnings_flag || infoMessages_flag >= 2 || mesg_debugging ) {
		va_list v ;
		va_start(v, fmt) ;
		ErrorMesgV("MEMORY", false, fmt, v) ;
	}
}

void MemoryGrowInfo(CharPt what, Size oldSize, Size newSize)
{
	if( memoryWarnings_flag || infoMessages_flag >= 2 || mesg_debugging ) {
		Str32 o, n ;
		if( oldSize < 1 K )
			sprintf(o, "%ld bytes", WordsAsBytes(oldSize)) ;
		else sprintf(o, "%ldKB", WordsAsKBytes(oldSize)) ;
		if( newSize < 1 K )
			sprintf(n, "%ld bytes", WordsAsBytes(newSize)) ;
		else sprintf(n, "%ldKB", WordsAsKBytes(newSize)) ;
		if( !Running() )
			MemoryInfo("Expanding %s from %s to %s", what, o, n) ;
		else
			MemoryInfo("Expanding %s from %s to %s [%s]", what, o, n, GetInstNameSearch(P-1)) ;
	}
}

void Warning(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	ErrorMesgV("WARNING", false, fmt, v) ;
}

static VoidPt TypeErrorAux(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV("TYPE ERROR", fmt, v) ;
}

VoidPt TypeError(CharPt expected, Pt found)
{
	if( found == nil )
		return TypeErrorAux("%s expected", expected) ;
	else
		return TypeErrorAux("%s expected, found '%s'", expected, TermAsStr(found)) ;
}

int ITypeError(CharPt expected, Pt found)
{
	TypeError(expected, found) ;
	return 0 ;
}

VoidPt SyntaxError(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV("SYNTAX ERROR", fmt, v) ;
}

VoidPt Error(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV("ERROR", fmt, v) ;
}

VoidPt ErrorV(CharPt fmt, va_list v)
{
	ErrorEventV("ERROR", fmt, v) ;
	return nil ;
}

VoidPt GenericError(CharPt kind, CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV(kind, fmt, v) ;
}

VoidPt ArithError(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV("ERROR", fmt, v) ;
}

VoidPt FileError(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV("ERROR", fmt, v) ;
}

VoidPt DatabaseError(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV("ERROR", fmt, v) ;

}

VoidPt ImperativeError(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	return ErrorEventV("ERROR", fmt, v) ;
}

VoidPt FatalError(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	ErrorMesgV("FATAL ERROR", true, fmt, v) ;
	EventFatalError() ;
	return nil ;
}

VoidPt InternalError(CharPt fun)
{
	Str256 fmt ;
	sprintf(fmt, "At function %s", fun) ;
	ErrorMesgV("INTERNAL ERROR", true, fmt, nil) ;
	EventFatalError() ;
	return nil ;
}

VoidPt UndisclosedError(CharPt s)
{
	ErrorMesgV("UNDISCLOSED FEATURE", false, s, nil) ;
	EventFatalError() ;
	return nil ;
}

int IInternalError(CharPt fun)
{
	InternalError(fun) ;
	return 0 ;
}


/* CXPROLOG C'BUILTINS */

static void PWarning()
{
	CharPt s ;
	X0 = Drf(X0) ;
	s = IsList(X0) ? TermsAsStrQ(X0) : TermAsStrQ(X0) ;
	Warning("%s", s) ;
	JumpNext() ;
}

static void PError()
{
	CharPt s ;
	X0 = Drf(X0) ;
	s = IsList(X0) ? TermsAsStrQ(X0) : TermAsStrQ(X0) ;
	Error("%s", s) ;
	JumpNext() ;
}

void MesgInit()
{
	InstallCBuiltinPred("warning", 1, PWarning) ;
	InstallCBuiltinPred("error", 1, PError) ;
}
