/*
 *   This file is part of the CxProlog system

 *   String.c
 *   by A.Miguel Dias - 2004/12/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

int StrHash(CharPt s)
{
	register UCharPt u = cUCharPt(s) ;
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

CharPt StrAllocate(CharPt str)
{
    return strcpy(TempAllocate(Words(strlen(str) + 1)), str) ;
}


/* GROWINGS TEMPORARY STRINGS */

#define maxGStr				6
#define initGStrCapacities	16	/* words */

static CharPt gStrs[maxGStr] ;
static Size gCapacities[maxGStr] ; /* bytes */
static int gCurr ;
CharPt gStrBegin, gStrEnd, gStrPt ;

static void GStrInit(int strCapacities)
{
	int i ;
	dotimes(i, maxGStr) {
		gStrs[i] = TempAllocate(strCapacities) ;
		gCapacities[i] = WordsAsBytes(strCapacities) ;
	}
	gCurr = 0 ;
}

static void GStrResize(Size newCapacity)
{
	Release(gStrs[gCurr]) ;
	gStrs[gCurr] = TempAllocate(newCapacity) ;
	gCapacities[gCurr] = WordsAsBytes(newCapacity) ;
}

Size GStrCapacity()
{
	Size n = 0 ;
	int i ;
	dotimes(i, maxGStr)
		n += Words(gCapacities[i]) ;
	return n ;
}

int GStrExpand()
{
	Size strCapacity = Words(gCapacities[gCurr]) ;
/* Cannot have MemoryGrowWarning here!! */
	gStrs[gCurr] = Reallocate(gStrBegin, strCapacity * 2) ;
	gCapacities[gCurr] = WordsAsBytes(strCapacity * 2) ;

	gStrPt += gStrs[gCurr] - gStrBegin ;
	gStrBegin = gStrs[gCurr] ;
	gStrEnd = gStrBegin + gCapacities[gCurr] - 8 ;
	return 0 ;
}

void GStrOpen()
{
	if( ++gCurr == maxGStr ) gCurr = 0 ;
	gStrPt = gStrBegin = gStrs[gCurr] ;
	gStrEnd = gStrBegin + gCapacities[gCurr] - 8 ;
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
	while( (size = vsnprintf(gStrs[gCurr], gCapacities[gCurr], fmt, v)) < 0 ) {
		GStrResize(2 * gCapacities[gCurr]) ;
	}
/* vsnprintf in modern libc */
	if( ++size > gCapacities[gCurr] ) {
/* No MemoryGrowWarning here!! */
		GStrResize(Words(size)) ;
		size = 1 + vsnprintf(gStrs[gCurr], gCapacities[gCurr], fmt, v) ;
	#if 0
		fprintf(stderr, "----- %d %ld(%ld) %s\n", gCurr,
							gCapacities[gCurr], size, gStrs[gCurr]) ;
		fflush(stderr) ;
	#endif
		if( size > gCapacities[gCurr] )
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

#define bigStrInitialCapacity	1 K		/* words */

static Size bigStrCapacity ; /* bytes */
CharPt bigStrBegin, bigStrEnd, bigStrPt;
CharPt bigStrAPt, bigStrBPt ;

Size BigStrCapacity()
{
	return Words(bigStrCapacity) ;
}

int BigStrExpand()
{
	Size strCapacity = Words(bigStrCapacity) ;
	CharPt bigStr ;
	MemoryGrowWarning("bigstr", strCapacity, strCapacity * 2, nil) ;
	bigStr = Reallocate(bigStrBegin, strCapacity * 2) ;
	bigStrCapacity = WordsAsBytes(strCapacity * 2) ;

	bigStrPt += bigStr - bigStrBegin ;
	bigStrAPt += bigStr - bigStrBegin ;
	bigStrBPt += bigStr - bigStrBegin ;
	bigStrBegin = bigStr ;
	bigStrEnd = bigStrBegin + bigStrCapacity - 32 ;
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
	bigStrBegin = TempAllocate(bigStrInitialCapacity) ;
	bigStrCapacity = WordsAsBytes(bigStrInitialCapacity) ;
	bigStrEnd = bigStrBegin + bigStrCapacity - 32 ;
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
	while( n-- > 0 && *s != '\0' ) { /* Copy */
		BigStrCheck() ;
		CharCopy(bigStrPt, s) ;
	}
}


/* INIT */

void StringInit()
{
	GStrInit(initGStrCapacities) ;
	BigStrInit() ;
}
