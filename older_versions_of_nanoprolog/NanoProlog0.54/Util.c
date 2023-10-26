/*
 *   This file is part of the NanoProlog system

 *   Util.c
 *   by A.Miguel Dias - 89/11/14
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931117: release of version 0.5

*/

#include "Util.h"

jmp_buf eventHandler ;

void Mesg(char *fmt, ...)
{
	char s[1 Kb] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	printf("%s\n", s) ;
}

void xRestart(char *fmt, ...)
{
	char s[1 Kb] ;
	va_list p ;
	
	if( fmt != nil )
	{
		va_start(p, fmt) ;
		vsprintf(s, fmt, p) ;
		printf("%s\n", s) ;
	}
	longjmp(eventHandler, 1) ;
}

void Error(char *fmt, ...)
{
	char s[1 Kb] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	printf("Error: %s!\n", s) ;
	longjmp(eventHandler, 1) ;
}

void FatalError(char *fmt, ...)
{
	char s[1 Kb] ;
	va_list p ;
	
	va_start(p, fmt) ;
	vsprintf(s, fmt, p) ;
	printf("Fatal Error: %s!\n", s) ;
	exit( 1 ) ;
}

void InternalError(char *s)
{
	FatalError("Internal Error at function %s", s) ;
}

void Default(char *s)
{
	FatalError("Default of function %s", s) ;
}

void *Align(void *pt)
{
	char *p = pt ;
	
	while( cInt(p) % sizeof(Word) != 0 ) p++ ;
	return( p ) ;
}

void *TAlloc(long bytes)
{
	CharPt mem, malloc(long) ;

	if( (mem = malloc(bytes + sizeof(Word))) == nil )
		Error("Not enough memory") ;
	return( Align(mem) ) ;
}

void CopyBytes(void *z, void *a, long n)
{
	register char *zz = z, *aa = a ;
	
	while( n-- ) *zz++ = *aa++ ;
}

void CopyWords(void *z, void *a, int n)
{
	register long *zz = z, *aa = a ;
	
	while( n-- ) *zz++ = *aa++ ;
}
