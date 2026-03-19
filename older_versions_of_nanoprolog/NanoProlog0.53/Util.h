/*
 *   This file is part of the NanoProlog system

 *   Util.h
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

 931203: type Mix changed.
 931117: release of version 0.5

*/

#ifndef _Util_
#define _Util_

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>

typedef char *CharPt ;
typedef char SByte ;
typedef unsigned char UByte ;
typedef unsigned long Word ;
typedef Word *Pt ;
typedef Pt *Hdl ;
typedef long Int ;
typedef int Bool ;
typedef double Real ;
typedef void (*Proc)() ;
typedef int (*Function)() ;
typedef Bool (*BoolFunction)() ;
typedef union
{
	struct { unsigned short loHalf, hiHalf ; } asShorts ;
	struct { unsigned int loHalf, hiHalf ; } asInts ;
	Word asWord ;
	Pt asPt ;
	float asFloat ;
	double asDouble ;
} Mix, *MixPt ;

#define cByte(x)	((Byte)(x))
#define cUByte(x)	((UByte)(x))
#define cWord(x)	((Word)(x))
#define cPt(x)		((Pt)(x))
#define cHdl(x)		((Hdl)(x))
#define cInt(x)		((Int)(x))
#define cShort(x)	((short)(x))
#define cBool(x)	((Bool)(x))
#define cReal(x)	((Real)(x))
#define cProc(x)	((Proc)(x))
#define cCharPt(x)	((char *)(x))

#define elif			else if
#define not				!
#define block
#define loop			for(;;)
#define dotimes(i, n)	for( i = 0 ; i < n ; i++ )
#define dotimesrev(i,n)	for( i = n - 1 ; i >= 0 ; i-- )
#define foreach(pt,i,f)	for( pt = i ; pt != nil ; pt = f )
#define dotable(e,t,s)	for( e = t ; e < t + s ; e++ )
#define Kb				* 1024L
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

extern jmp_buf eventHandler ;
#define StartEventHandler()	setjmp(eventHandler)

void Mesg(char *fmt, ...) ;
void Restart(char *fmt, ...) ;
void Error(char *fmt, ...) ;
void FatalError(char *fmt, ...) ;
void InternalError(char *s) ;
void Default(char *s) ;
void *Align(void *pt) ;
void *TAlloc(long bytes) ;

void CopyBytes(void *z, void *a, long n) ;
void CopyWords(void *z, void *a, int n) ;

#endif
