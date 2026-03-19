/*
 *   This file is part of the CxProlog system

 *   String.c
 *   by A.Miguel Dias - 2004/12/12
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
#include <ctype.h>

int StrHash(CharPt n)
{
	register UCharPt u = n ;
	register int val = 0 ;
#if 1
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;

	if( *u == '\0' ) return val ; else val += *u++ ;		
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;

	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;

	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;
	if( *u == '\0' ) return val ; else val += *u++ ;		
	if( *u == '\0' ) return val ; else val += *u++ ;
#else
	while( *u ) val += *u++ ;
#endif
	return val ;
}

Bool StrSimilar(CharPt a, CharPt b, int n, Bool ignoreCase)
{
	if( ignoreCase )
		while( n-- ) {
			if( *a == 0 || *b == 0 ) return *a == *b ;
			if( toupper(*a) != toupper(*b)) return false ;
			a++ ; b++ ;
		}
	else
		while( n-- ) {
			if( *a == 0 || *b == 0 ) return *a == *b ;
			if( *a++ != *b++) return false ;
		}
	return true ;
}

CharPt StrPerm(CharPt str)
{
    return strcpy(PermAllocate(Words(strlen(str) + 1)), str) ;
}


/* GROWINGS TEMPORARY STRINGS */

#define maxGStr			6
#define initGStrSizes	16	/* words */

static CharPt gStrs[maxGStr] ;
static Size gSizes[maxGStr] ; /* bytes */
static int gCurr ;
CharPt gStrBegin, gStrEnd, gStrPt ;

static void GStrInit(int strSizes)
{
	int i ;
	dotimes(i, maxGStr) {
		gStrs[i] = TempAllocate(strSizes) ;
		gSizes[i] = WordsAsBytes(strSizes) ;
	}
	gCurr = 0 ;
}

static void GStrResize(Size newSize)
{
	Release(gStrs[gCurr]) ;
	gStrs[gCurr] = TempAllocate(newSize) ;
	gSizes[gCurr] = WordsAsBytes(newSize) ;
}

Size GStrSize()
{
	Size n = 0 ;
	int i ;
	dotimes(i, maxGStr)
		n += Words(gSizes[i]) ;
	return n ;
}

int GStrExpand()
{
	Size strSize = Words(gSizes[gCurr]) ;
/* Cannot have MemoryGrowWarning here!! */
	gStrs[gCurr] = Reallocate(gStrBegin, strSize * 2) ;
	gSizes[gCurr] = WordsAsBytes(strSize * 2) ;

	gStrPt += gStrs[gCurr] - gStrBegin ;
	gStrBegin = gStrs[gCurr] ;
	gStrEnd = gStrBegin + gSizes[gCurr] - 8 ;
	return 0 ;
}

void GStrOpen()
{
	if( ++gCurr == maxGStr ) gCurr = 0 ;
	gStrPt = gStrBegin = gStrs[gCurr] ;
	gStrEnd = gStrBegin + gSizes[gCurr] - 8 ;
}

CharPt GStrClose()
{
	*gStrPt = '\0' ;
	return gStrBegin ;
}

void GStrAddStr(CharPt s) {
	while( *s ) {
		GStrCheck() ;
		*gStrPt++ = *s++ ;
	}
}

#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

CharPt GStrFormatV(CharPt fmt, va_list v)
{
	Size size ;	
	if( ++gCurr == maxGStr ) gCurr = 0 ;
/* vsnprintf in antique libc ou Visual Studio */
	errno = 0 ;
	while( (size = vsnprintf(gStrs[gCurr], gSizes[gCurr], fmt, v)) < 0 ) {
		if( errno != 0 )
			InternalError("GStrFormatV") ;
		else
			GStrResize(2 * gSizes[gCurr]) ;
	}
/* vsnprintf in modern libc */
	if( ++size > gSizes[gCurr] ) {
/* No MemoryGrowWarning here!! */
		GStrResize(Words(size)) ;
		size = 1 + vsnprintf(gStrs[gCurr], gSizes[gCurr], fmt, v) ;
	#if 0
		fprintf(stderr, "----- %d %ld(%ld) %s\n", gCurr, gSizes[gCurr], size, gStrs[gCurr]) ;
		fflush(stderr) ;
	#endif
		if( size > gSizes[gCurr] )
			InternalError("GStrFormatV") ;
	}
	return gStrs[gCurr] ;	
}

CharPt GStrFormat(CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
    return GStrFormatV(fmt, p) ;
}

CharPt GStrMake(CharPt s)
{
	return GStrFormat("%s", s) ;
}


/* BIG STRING */

#define bigStrInitialSize		1 K		/* words */

static Size bigStrSize ; /* bytes */
CharPt bigStrBegin, bigStrEnd, bigStrPt;
CharPt bigStrAPt, bigStrBPt ;

Size BigStrSize()
{
	return Words(bigStrSize) ;
}

int BigStrExpand()
{
	Size strSize = Words(bigStrSize) ;
	CharPt bigStr ;
	MemoryGrowWarning("bigstr", strSize, strSize * 2, nil) ;
	bigStr = Reallocate(bigStrBegin, strSize * 2) ;
	bigStrSize = WordsAsBytes(strSize * 2) ;

	bigStrPt += bigStr - bigStrBegin ;
	bigStrAPt += bigStr - bigStrBegin ;
	bigStrBPt += bigStr - bigStrBegin ;
	bigStrBegin = bigStr ;
	bigStrEnd = bigStrBegin + bigStrSize - 32 ;
	return 0 ;
}

void BigStrOpen()
{
	bigStrPt = bigStrBegin ;
}

CharPt BigStrClose()
{
	*bigStrPt = '\0' ;
	return bigStrBegin ;
}

void BigStrInit()
{
	bigStrBegin = TempAllocate(bigStrInitialSize) ;
	bigStrSize = WordsAsBytes(bigStrInitialSize) ;
	bigStrEnd = bigStrBegin + bigStrSize - 32 ;
}

void BigStrAddStr(register CharPt s)
{
	while( *s ) {
		BigStrCheck() ;
		*bigStrPt++ = *s++ ;
	}
}

void BigStrAddStrSlice(CharPt s, int a, int b)
{
	int n = b - a + 1 ;
	s = CharPos(s, a) ; /* Find the start */
	while( n-- > 0 && *s ) { /* Copy */
		BigStrCheck() ;
		CharCopy(bigStrPt, s) ;
	}
}


/* INIT */

void StringInit()
{
	GStrInit(initGStrSizes) ;
	BigStrInit() ;
}
