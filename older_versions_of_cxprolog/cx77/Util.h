/*
 *   This file is part of the CxProlog system

 *   Util.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Util_
#define _Util_

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>

#if ULONG_MAX == 0xFFFFFFFFL
  #define WORD_SIZE 32
#else
  #define WORD_SIZE 64
#endif

#if WORD_SIZE == 32 && __GNUC__ >= 2
  #define USE_LONG_LONG	1
#else
  #define USE_LONG_LONG	0
#endif

/*   */

#if WORD_SIZE == 32
	typedef unsigned short HalfWord ;
#else
	typedef unsigned int HalfWord ;
#endif

#if USE_LONG_LONG
	typedef long long BigInt ;
	typedef unsigned long long BigUInt ;
#else
	typedef long BigInt ;
	typedef unsigned long BigUInt ;
#endif

typedef unsigned long Word ;
typedef long Size ;
typedef short Bool ;
typedef char *CharPt ;
typedef unsigned char *UCharPt ;
typedef void *VoidPt ;
typedef Word *Pt ;
typedef Pt *Hdl ;
typedef Hdl *Hdl3 ;
typedef void (*Proc)(void) ;
typedef int (*Function)(void) ;
typedef Bool (*BoolFunction)(void) ;
typedef union {
	struct { HalfWord loHalf, hiHalf ; } asHalfWords ;
	Word asWord ;
	Pt asPt ;
	Hdl asHdl ;
	CharPt asCharPt ;
	UCharPt asUCharPt ;
	float asFloat ;
	double asDouble ;
} Mix, *MixPt ;

#define cLong(x)		((long)(x))
#define cWord(x)		((Word)(x))
#define cBigInt(x)		((BigInt)(x))
#define cBigUInt(x)		((BigUInt)(x))
#define cPt(x)			((Pt)(x))
#define cHdl(x)			((Hdl)(x))
#define cHdl3(x)		((Hdl3)(x))
#define cProc(x)		((Proc)(x))
#define cCharPt(x)		((CharPt)(x))
#define cUCharPt(x)		((UCharPt)(x))
#define cVoidPt(x)		((VoidPt)(x))

#define elif			else if
#define not				!
#define dotimes(i, n)	for( i = 0 ; i < n ; i++ )
#define dotimesrev(i,n)	for( i = n - 1 ; i >= 0 ; i-- )
#define dolist(p,i,f)	for( p = i ; p != nil ; p = f )
#define dotable(e,t,s)	for( e = t ; e < t + s ; e++ )

#define K					* 1024L
#define WordsAsBytes(w)		((w) * sizeof(Word))
#define WordsAsKBytes(w)	(WordsAsBytes(w)/1024L)

#define true			1
#define false			0

#define Remainder(x,q)	((x) % (q))
#define RoundDown(x,q)	( (x) - Remainder(x,q) )
#define RoundUp(x,q)	RoundDown( (x) + q - 1, q )
#define Round(x,q)		RoundDown(x,q) + ( Remainder(x,q) > (q)/2 ) * (q)
#define DivUp(x,q)		( ((x) + (q) - 1) / (q) )
#define Words(bytes)	DivUp(bytes, sizeof(Word))
#define WordsOf(type)	Words(sizeof(type))

#define Max(x,y)		( (x) >= (y) ? (x) : (y) )
#define Min(x,y)		( (x) >= (y) ? (y) : (x) )
#define Abs(x)			Max((x), -(x))
#define InRange(x,a,b)	( (x) >= (a) && (x) <= (b) )
#define Positive(x)		((x) > 0 ? (x) : 0)
#define Logic(x)		(!!(x))
#define Bit(l, b)		( ( (l) >> (b) ) & 1 )

/* Pt Stack */
#define Push(sp, v)		( *(sp)++ = cPt(v) )
#define Pop(sp)			( *--(sp) )
#define Top(sp)			( (sp)[-1] )
#define Grow(sp, n)		( (sp) += (n) )

#define EqualStr(a, b)	( strcmp(a, b) == 0 )

#undef nil
#define	nil				(0L)

#define ToUpperCase(x)	( (x) >= 'a' && (x) <= 'z' ? (x) - 'a' + 'A' : (x) )
#define ToLowerCase(x)	( (x) >= 'A' && (x) <= 'Z' ? (x) - 'A' + 'a' : (x) )


#define retBufferSize	1024
extern char retBuffer[] ;

VoidPt PrimitiveAllocate(Size nWords) ;
void PrimitiveRelease(VoidPt mem) ;
void CopyBytes(CharPt zz, CharPt aa, Size len) ;
void CopyWords(Hdl zz, Hdl aa, Size len) ;
void CopyWordsRelloc(Hdl zz, Hdl aa, Size len) ;

#endif
