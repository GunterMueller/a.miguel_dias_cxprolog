/*
 *   This file is part of the CxProlog system

 *   Number.c
 *   by A.Miguel Dias - 2001/02/27
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
#include <math.h>


/* INT */

#define initialIntTableSize	256

typedef BigInt *BigIntPt ;

static BigIntPt intTableBegin, intTableEnd ;

static void InitInts()
{
	register BigIntPt i ;
	intTableBegin = PrimitiveAllocate(initialIntTableSize * WordsOf(BigInt)) ;
	intTableEnd = intTableBegin + initialIntTableSize ;
	for( i = intTableBegin ; i < intTableEnd ; i++ )
		*i = 0 ;
}

static void IntTableExpand()
{
	register BigIntPt i ;
	Size tableNElems = intTableEnd - intTableBegin ;
	BigIntPt newIntTableBegin, newIntTableEnd ;
/*	MemoryWarning("BigInt table",
					tableNElems * WordsOf(BigInt),
					tableNElems * 2 * WordsOf(BigInt)) ;
*/	newIntTableBegin = PrimitiveAllocate(tableNElems * 2 * WordsOf(BigInt)) ;
	newIntTableEnd = newIntTableBegin + tableNElems * 2 ;	
	for( i = newIntTableBegin ; i < newIntTableEnd ; i++ )
		*i = 0 ;
	CopyWords(cHdl(newIntTableBegin),
					cHdl(intTableBegin),
					tableNElems * WordsOf(BigInt)) ;
	PrimitiveRelease(intTableBegin) ;
	intTableBegin = newIntTableBegin ;
	intTableEnd = newIntTableEnd ;
}

Size BigIntTableTotalSize()
{
	return cHdl(intTableEnd) - cHdl(intTableBegin) ;
}

Size BigIntTableUsed()
{
	register BigIntPt i ;
	for( i = intTableBegin ; i < intTableEnd ; i++ )
		if( *i == 0 ) break ;
	return cHdl(i) - cHdl(intTableBegin) ;
}


static Pt MakeBigInt(BigInt n)
{
	register BigIntPt i ;
	for(;;) {
		for( i = intTableBegin ; i < intTableEnd ; i++ ) {
			if( *i == 0 )
				*i = n ;
			if( *i == n )
				return TagBigInt(EncodeBits(i - intTableBegin)) ;
		}
		IntTableExpand() ;
	}
}

#define maxSmallInt			cLong((~cWord(0))>>4)
#define minSmallInt			(-maxSmallInt - 1)

Pt MakeInt(BigInt n)
{
	if( n <= maxSmallInt && n >= minSmallInt )
		return TagInt( ClearTag(EncodeBits(n)) ) ;
	else return MakeBigInt(n) ;
}

BigInt XInt(Pt t)
{
	if( IsBigInt(t) )
		return intTableBegin[DecodeBits(ClearTag(t))] ;
	else
		return cLong(cWord(t) << 1) >> 3 ;
}

int CompareInt(BigInt i1, BigInt i2)
{
	return i1 == i2 ? 0 : i1 > i2 ? 1 : -1 ;
}



/* REAL */

static Bool ieeeFloats, useFloat ;

static void InitReals()
{
	if( sizeof(float) == sizeof(Word) )
		useFloat = true ;
	elif( sizeof(double) == sizeof(Word) )
		useFloat = false ;
	else
		FatalError("Cannot run on this machine because of the format of the real numbers") ;

	ieeeFloats = false ;
	ieeeFloats = fabs(222222.0 - XReal(MakeReal(222222.0))) > 0.5 ;
}

Pt MakeReal(double r)
{
	Mix m ;
	if( useFloat ) m.asFloat = r ; else m.asDouble = r ;
	if( ieeeFloats ) {
		m.asWord = EncodeBits(PackBits(m.asWord)) ;
	}
	else {
		m.asHalfWords.hiHalf = PackBits(m.asHalfWords.hiHalf) ;
		m.asWord = EncodeBits(m.asWord) ;
	}	
	return TagReal(m.asPt) ;
}

double XReal(Pt p)
{
	Mix m ;
	m.asPt = p ;
	if( ieeeFloats ) {
		m.asWord = UnpackBits(DecodeBits(m.asWord)) ;
	}
	else {
		m.asWord = DecodeBits(m.asWord) ;
		m.asHalfWords.hiHalf = UnpackBits(m.asHalfWords.hiHalf) ;
	}	
	return useFloat ? m.asFloat : m.asDouble ;
}

int CompareReal(double r1, double r2)
{
	return r1 == r2 ? 0 : r1 > r2 ? 1 : -1 ;
}


/* NUMBER */

Bool Narrow(double f, BigInt *i)
{
	if( f < minBigInt || f > maxBigInt ) return false ;
	*i = f ;
	return *i == f ;
}

CharPt XNumberAsStr(Pt t) /* already deref */
{
	if( IsInt(t) ) {
#if USE_LONG_LONG
		sprintf(retBuffer, "%lld", XInt(t)) ;
#else
		sprintf(retBuffer, "%ld", XInt(t)) ;
#endif
	}
	elif( IsReal(t) ) 
		sprintf(retBuffer, "%.6g", XReal(t)) ;
	elif( IsThisStruct(t, formatFunctor) ) {
		char format[100] ;
		double d =  XTestReal(XStructArg(t,0)) ;
		int n = XTestInt(XStructArg(t,1)) ;
		if( n < 0 ) sprintf(format, "%%.%de",Min(-n,20)) ;
		else sprintf(format, "%%.%df", Min(n,20)) ;
		sprintf(retBuffer, format, d) ;
		return retBuffer ;
	}	
	else InternalError("NumberAsStr") ;
	return retBuffer ;
}

int CompareNumber(register Pt t1, register Pt t2) /* already deref */
{
	if( IsInt(t1) ) {
		if( IsInt(t2) ) return CompareInt(XInt(t1), XInt(t2)) ;
		elif(IsReal(t2) ) return CompareReal(XInt(t1), XReal(t2)) ;
		else InternalError("CompareNumber (1)") ;
	}
	elif( IsReal(t1) ) {
		if( IsInt(t2) ) return CompareReal(XReal(t1), XInt(t2)) ;
		elif( IsReal(t2) ) return CompareReal(XReal(t1), XReal(t2)) ;
		else InternalError("CompareNumber (2)") ;
	}
	else InternalError("CompareNumber (3)") ;
}

void InitNumbers()
{
	InitInts() ;
	InitReals() ;
}

