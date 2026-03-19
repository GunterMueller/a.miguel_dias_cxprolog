/*
 *   This file is part of the CxProlog system

 *   Util.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Util_
#define _Util_

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>

typedef char *CharPt ;
typedef unsigned char *UCharPt ;
typedef void *VoidPt ;
typedef unsigned long Word ;
typedef Word *Pt ;
typedef Pt *Hdl ;
typedef long Int ;
typedef int Bool ;
typedef double Real ;
typedef void (*Proc)(void) ;
typedef int (*Function)(void) ;
typedef Bool (*BoolFunction)(void) ;
typedef union {
	struct { unsigned short loHalf, hiHalf ; } asShorts ;
	struct { unsigned int loHalf, hiHalf ; } asInts ;
	Word asWord ;
	Pt asPt ;
	float asFloat ;
	double asDouble ;
} Mix, *MixPt ;

#define cWord(x)	((Word)(x))
#define cPt(x)		((Pt)(x))
#define cHdl(x)		((Hdl)(x))
#define cInt(x)		((Int)(x))
#define cShort(x)	((short)(x))
#define cBool(x)	((Bool)(x))
#define cReal(x)	((Real)(x))
#define cProc(x)	((Proc)(x))
#define cCharPt(x)	((CharPt)(x))
#define cUCharPt(x)	((UCharPt)(x))

#define elif			else if
#define not				!
#define dotimes(i, n)	for( i = 0 ; i < n ; i++ )
#define dotimesrev(i,n)	for( i = n - 1 ; i >= 0 ; i-- )
#define dolist(p,i,f)	for( p = i ; p != nil ; p = f )
#define dotable(e,t,s)	for( e = t ; e < t + s ; e++ )
#define Kb				* 1024L
#define Kw				* 4096L
#define Wd				* sizeof(Word)

#define true			1
#define false			0

#define Remainder(x,q)	((x) % (q))
#define RoundDown(x,q)	( (x) - Remainder(x,q) )
#define RoundUp(x,q)	RoundDown( (x) + q - 1, q )
#define Round(x,q)		RoundDown(x,q) + ( Remainder(x,q) > (q)/2 ) * (q)
#define DivUp(x,q)		( ((x) + (q) - 1) / (q) )
#define LWord(w)		((short)(w))
#define HWord(w)		((short)((w)>>16))
#define MakeWord(h,l)	cWord((l)+((h)<<16))
#define Max(x,y)		( (x) >= (y) ? (x) : (y) )
#define Min(x,y)		( (x) >= (y) ? (y) : (x) )
#define Abs(x)			Max((x), -(x))
#define InRange(x,a,b)	( (x) >= (a) && (x) <= (b) )
#define Positive(x)		((x) > 0 ? (x) : 0)
#define Logic(x)		(!!(x))
#define Bit(l, b)		( ( (l) >> (b) ) & 1 )
#define Words(bytes)	DivUp(bytes, sizeof(Word))
#define WordsOf(type)	Words(sizeof(type))

#define Push(sp, v)		( *(sp)++ = (v) )
#define Pop(sp)			( *--(sp) )
#define Top(sp)			( (sp)[-1] )
#define Grow(sp)		( (sp)++ )

#define EqualStr(a, b)	( strcmp(a, b) == 0 )

#undef nil
#define	nil				(0L)

#define ToUpperCase(x)	( (x) >= 'a' && (x) <= 'z' ? (x) - 'a' + 'A' : (x) )
#define ToLowerCase(x)	( (x) >= 'A' && (x) <= 'Z' ? (x) - 'A' + 'a' : (x) )

VoidPt AllocAligned(long bytes) ;
void CopyWords(Hdl zz, Hdl aa, Int len) ;
void CopyWordsRelloc(Hdl zz, Hdl aa, Int len) ;

#endif
