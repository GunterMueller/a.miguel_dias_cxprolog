/*
 *   This file is part of the CxProlog system

 *   String.c
 *   by A.Miguel Dias - 2004/12/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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
    return strcpy(Allocate(Words(strlen(str) + 1), true), str) ;
}

int StrSeqLength(CharPt *vs)
{
	register int i ;
    for( i = 0 ; *vs != nil ; i++, vs++ ) ;
	return i ;
}

int StrSeqGetIdx(CharPt a, CharPt *vs)
{
	register int i ;
    for( i = 0 ; *vs != nil ; i++, vs++ )
		if( StrEqual(a, *vs) )
			return i ;
	return -1 ;
}

CharPt StrSeqGetVal(int i, CharPt *vs)
{
    register int j ;
    for( j = 0 ; *vs != nil ; j++, vs++ )
        if( j == i )
            return *vs ;
    InternalError("StrSeqGetVal") ;
    return nil ;
}

CharPt StrSeqFormat(CharPt pre, CharPt sep, CharPt pos, CharPt *vs)
{
	GStrOpen() ;
	GStrAddStr(pre) ;
    for(  ; *vs != nil ; vs++ ) {
		GStrAddStr(*vs) ;
		GStrAddStr(sep) ;
	}
	GStrBack(strlen(sep)) ;
	GStrAddStr(pos) ;
	return GStrClose() ;
}


/* GROWINGS TEMPORARY STRINGS */

#define maxGStr				8	/* 8 or larger! */
#define initGStrCapacities	64	/* bytes */

static CharPt gStrs[maxGStr] ;
static Size gCapacities[maxGStr] ; /* in bytes */
static int gCurr, gStrInUse ;
CharPt gStrBegin, gStrEnd, gStrPt ;

static void GStrInit(void)
{
	int i ;
	dotimes(i, maxGStr) {
		gStrs[i] = Allocate(Words(initGStrCapacities), false) ;
		gCapacities[i] = initGStrCapacities ;
	}
	gCurr = 0 ;
}

static void GStrResize(int i, Size newCapacity)
{
	Release(gStrs[i], Words(gCapacities[i])) ;
	gStrs[i] = Allocate(Words(newCapacity), false) ;
	gCapacities[i] = newCapacity ;
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
	Size offset = gStrPt - gStrBegin ;
	Size oldStrCapacity = gCapacities[gStrInUse] ;
	Size newStrCapacity = oldStrCapacity * 2 ;
	gStrs[gStrInUse] = Reallocate(gStrBegin, Words(oldStrCapacity),
												Words(newStrCapacity)) ;
	gCapacities[gStrInUse] = newStrCapacity ;

	gStrBegin = gStrs[gStrInUse] ;
	gStrPt = gStrBegin + offset ;
	gStrEnd = gStrBegin + newStrCapacity - 8 ;
	return 0 ;
}

void GStrOpen()
{
	if( ++gCurr == maxGStr ) gCurr = 0 ;
	gStrInUse = gCurr ;

	gStrBegin = gStrs[gStrInUse] ;
	gStrPt = gStrBegin ;
	gStrEnd = gStrBegin + gCapacities[gStrInUse] - 8 ;
}

CharPt GStrClose()
{
	*gStrPt = '\0' ;
	return gStrBegin ;
}

void GStrAddStr(CharPt s)
{
	while( *s ) {
		GStrCheck() ;
		*gStrPt++ = *s++ ;
	}
}

void GStrAddStrConv(CharPt s, WChar oldChar, WChar newChar)
{
	if( oldChar == newChar )
		GStrAddStr(s) ;
	else {
		while( *s ) {
			GStrCheck() ;
			if( (*gStrPt++ = *s++) == oldChar )
				gStrPt[-1] = newChar ;
		}
	}
}

#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

CharPt GStrFormatV(CharPt fmt, va_list v)
{
	int i ;
	Size size ;	
	if( ++gCurr == maxGStr ) gCurr = 0 ;
	i = gCurr ;
/* vsnprintf in antique libc ou Visual Studio */
	while( (size = vsnprintf(gStrs[i], gCapacities[i], fmt, v)) < 0 )
		GStrResize(i, 2 * gCapacities[i]) ;
/* vsnprintf in modern libc */
	if( ++size > gCapacities[i] ) {
		GStrResize(i, size) ;
		size = 1 + vsnprintf(gStrs[i], gCapacities[i], fmt, v) ;
		if( size > gCapacities[i] )
			InternalError("GStrFormatV") ;
	}
	return gStrs[i] ;	
}

CharPt GStrFormat(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
    return GStrFormatV(fmt, v) ;
}

CharPt GStrMake(CharPt s)
{
	return GStrFormat("%s", s) ;
}


/* BIG STRING */

#define bigStrInitialCapacity	4 K		/* bytes */

static Size bigStrCapacity ; /* in bytes */
CharPt bigStrBegin, bigStrEnd, bigStrPt;
CharPt bigStrAPt, bigStrBPt ;

Size BigStrCapacity()
{
	return Words(bigStrCapacity) ;
}

int BigStrExpand()
{
	Size oldStrCapacity = bigStrCapacity ;
	Size newStrCapacity = oldStrCapacity * 2 ;
	CharPt bigStr ;
	MemoryGrowWarning("bigstr", Words(oldStrCapacity), Words(newStrCapacity), nil) ;
	bigStr = Reallocate(bigStrBegin, Words(oldStrCapacity), Words(newStrCapacity)) ;
	bigStrCapacity = newStrCapacity ;

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
	bigStrBegin = Allocate(Words(bigStrInitialCapacity), false) ;
	bigStrCapacity = bigStrInitialCapacity ;
	bigStrEnd = bigStrBegin + bigStrCapacity - 32 ;
}

void BigStrAddStr(register CharPt s)
{
	for( ; *s ; s++ )
		BigStrAddByte(*s) ;
}


void BigStrAddStrConv(CharPt s, WChar oldChar, WChar newChar)
{
	if( oldChar == newChar )
		BigStrAddStr(s) ;
	else {
		for( ; *s ; s++ )
			BigStrAddByte(*s == oldChar ? newChar : *s) ;
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
	GStrInit() ;
	BigStrInit() ;
}
