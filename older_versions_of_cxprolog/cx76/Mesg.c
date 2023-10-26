/*
 *   This file is part of the CxProlog system

 *   Mesg.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"

static void BeginMesg(CharPt kind)
{
	CharPt s = MachineIsOn() ? CPredNameArity(P[-1]) : nil ;

	if( s != nil )
		WriteStd("{%s@(%s): ", kind, s) ;
	else
		WriteStd("{%s: ", kind) ;
}

static void EndMesg(void)
{
	WriteStd("}\n") ;
}

static void DoMesg(CharPt kind, CharPt theMesg)
{
	BeginMesg(kind) ;
	WriteStd("%s", theMesg) ;
	EndMesg() ;
}

void Mesg(CharPt fmt, ...)
{
	char s[1 K] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	WriteStd("{%s}\n", s) ;
}

void Warning(CharPt fmt, ...)
{
	char s[1 K] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	WriteStd("{WARNING: %s}\n", s) ;
}

void Error(CharPt fmt, ...)
{
	char s[1 K] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	if( failOnError_flag ) {
		QueueError(s) ;
		EventForceFail() ;
	}
	else {
		DoMesg("ERROR", s) ;
		EventRestart() ;
	}
}

int IError(CharPt fmt, ...)	/* same as Error, only returns an integer */
{
	char s[1 K] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	if( failOnError_flag ) {
		QueueError(s) ;
		EventForceFail() ;
	}
	else {
		DoMesg("ERROR", s) ;
		EventRestart() ;
	}
	return 0 ;
}

void ArithError(CharPt fmt, ...)
{
	char s[1 K] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	if( failOnError_flag ) {
		QueueError(s) ;
		EventForceFail() ;
	}
	else {
		DoMesg("ARITHMETIC ERROR", s) ;
		EventRestart() ;
	}
}

void TypeError(CharPt kind, Pt t)
{
	char s[1 K] ;

	sprintf(s, "%s expected (arg was '%s')", kind, TermTypeStr(t)) ;
	if( failOnError_flag ) {
		QueueError(s) ;
		EventForceFail() ;
	}
	else {
		DoMesg("TYPE ERROR", s) ;
		EventRestart() ;
	}
}

void FatalError(CharPt fmt, ...)
{
	char s[1 K] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	DoMesg("FATAL ERROR", s) ;
	EventHalt() ;
}

void InternalError(CharPt fun)
{
	char s[1 K] ;

	sprintf(s, "At function %s", fun) ;
	DoMesg("INTERNAL ERROR", s) ;
	EventHalt() ;
}

void Default(CharPt fun)
{
	char s[1 K] ;

	sprintf(s, "Default of function %s", fun) ;
	DoMesg("INTERNAL ERROR", s) ;
	EventHalt() ;
}

void SysErrorN(CharPt fmt, ...)
{
	char s[1 K] ;
	va_list p ;
	extern int errno ;

	perror(nil) ;
	sprintf(s, "%d", errno) ;
	DoMesg("ERRNO", s) ;
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	DoMesg("SYSTEM ERROR", s) ;
	EventRestart() ;
}

void SysFatalError(CharPt fmt, ...)
{
	char s[1 K] ;
	va_list p ;
	extern int errno ;

	perror(nil) ;
	sprintf(s, "%d", errno) ;
	DoMesg("ERRNO", s) ;
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	DoMesg("SYSTEM FATAL ERROR", s) ;
	EventHalt() ;
}