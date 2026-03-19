/*
 *   This file is part of the CxProlog system

 *   Util.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Util_
#define _Util_

typedef unsigned long Word ;
typedef long Size ;
typedef long PInt ;
#ifndef __APPLE__
	typedef long double PFloat ;
#else
	typedef double PFloat ;
#endif
typedef unsigned short Bool ;
typedef char Char ;
typedef Char *CharPt ;
typedef Char Str4[4], Str32[32], Str256[256], Str1K[1024], Str2K[2048] ;
typedef unsigned char UChar ;
typedef UChar *UCharPt ;
typedef unsigned char Byte ;
#if USE_WIDECHARS
    typedef int WChar ;
#else
    typedef Char WChar ;
#endif
typedef WChar *WCharPt ;
typedef void *VoidPt ;
typedef Word *Pt ;
typedef Pt *Hdl ;
typedef Hdl *Hdl3 ;
#ifdef LLONG_MAX
	typedef long long LLInt ;
	#define HAS_LONG_LONG	1
#else
	typedef long LLInt ;
#endif
typedef unsigned short Word16 ;
typedef void (*VFun)() ;
typedef void (*VFunI)(int) ;
typedef void (*VFunII)(int,int) ;
typedef void (*VFunV)(VoidPt) ;
typedef Bool (*BFun)() ;
typedef Bool (*BFunI)(int) ;
typedef Bool (*BFunII)(int,int) ;
typedef Bool (*BFunV)(VoidPt) ;
typedef Bool (*BFunVV)(VoidPt, VoidPt) ;
typedef CharPt (*CFunV)(VoidPt) ;

typedef Pt Inst ;	/* Abstract Machine Instruction */
typedef Inst *InstPt ;

#define cPChar(x)		((PChar)(x))
#define cPInt(x)		((PInt)(x))
#define cPFloat(x)		((PFloat)(x))
#define cWord(x)		((Word)(x))
#define cPt(x)			((Pt)(x))
#define cHdl(x)			((Hdl)(x))
#define cHdl3(x)		((Hdl3)(x))
#define cVFun(x)		((VFun)(x))
#define cCharPt(x)		((CharPt)(x))
#define cUCharPt(x)		((UCharPt)(x))
#define cVoidPt(x)		((VoidPt)(x))
#define cInstPt(x)		((InstPt)(x))

#define Ignore(x)		((void)(x))
#define Do(c)			do { c } while( 0 )

#define maxWord			(~cWord(0))
#define maxAddr			cVoidPt(maxWord)

#define elif			else if
#define dotimes(i,n)	for( i = 0 ; i < n ; i++ )
#define dotimesrev(i,n)	for( i = n - 1 ; i >= 0 ; i-- )
#define doseq(p,i,f)	for( p = i ; p != nil ; p = f )
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
#define InRange(x,a,b)	( (a) <= (x) && (x) <= (b) )
#define Positive(x)		((x) > 0 ? (x) : 0)
#define Logic(x)		(!!(x))
#define Bit(l, b)		( ( (l) >> (b) ) & 1 )

/* Pt Stack */
#define Push(sp, v)		( *(sp)++ = cPt(v) )
#define Pop(sp)			( *--(sp) )
#define Top(sp)			( (sp)[-1] )
#define XTop(sp,n)		( (sp)[-(n)-1] )
#define Grow(sp, n)		( (sp) += (n) )

#undef nil
#define	nil				(0L)

#define ToUpperCase(x)	( (x) >= 'a' && (x) <= 'z' ? (x) - 'a' + 'A' : (x) )
#define ToLowerCase(x)	( (x) >= 'A' && (x) <= 'Z' ? (x) - 'A' + 'a' : (x) )

void ClearBytes(VoidPt v, Size len) ;
void ClearWords(Hdl h, Size len) ;
void CopyBytes(CharPt zz, CharPt aa, Size len) ;
void CopyWords(Hdl zz, Hdl aa, Size len) ;
void CopyWordsReloc(Hdl zz, Hdl aa, Size len) ;
void ShiftWords(Hdl h, Size len, Size offset) ;

#endif
