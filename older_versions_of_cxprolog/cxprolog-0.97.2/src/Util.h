/*
 *   This file is part of the CxProlog system

 *   Util.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
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
typedef unsigned char Bool3 ;
typedef unsigned char Bool ;
typedef char Char ;
typedef Char *CharPt, **CharHdl ;
typedef Char Str4[4], Str8[8], Str32[32], Str256[256], Str1K[1024], Str2K[2048] ;
typedef unsigned char UChar ;
typedef UChar *UCharPt, **UCharHdl ;
#if _WIN32
typedef INT32 WChar ;
#elif INT_MAX == 0x7fffffff
typedef int WChar ;
#else
typedef int32_t WChar ;
#endif
typedef WChar *WCharPt ;
typedef void *VoidPt ;
typedef Word *Pt, **Hdl, ***Hdl3 ;
#ifdef LLONG_MAX
	typedef long long LLInt ;
	#define HAS_LONG_LONG	1
#else
	typedef long LLInt ;
#endif
typedef unsigned short Word16 ;
typedef void (*Fun)(/* nothing */) ;
typedef void (*FunI)(int) ;
typedef void (*FunII)(int,int) ;
typedef void (*FunV)(VoidPt) ;
typedef Bool (*BFun)(void) ;
typedef Bool (*BFunI)(int) ;
typedef Bool (*BFunII)(int,int) ;
typedef Bool (*BFunV)(VoidPt) ;
typedef Bool (*BFunVV)(VoidPt, VoidPt) ;
typedef CharPt (*CFun)(void) ;
typedef CharPt (*CFunV)(VoidPt) ;
typedef VoidPt (*VFun)(void) ;

typedef Pt Inst ;	/* Abstract Machine Instruction */
typedef Inst *InstPt ;

#define cPChar(x)		((PChar)(x))
#define cPInt(x)		((PInt)(x))
#define cPIntPt(x)		((PInt *)(x))
#define cPFloat(x)		((PFloat)(x))
#define cPFloatPt(x)	((PFloat *)(x))
#define cWord(x)		((Word)(x))
#define cSize(x)		((Size)(x))
#define cPt(x)			((Pt)(x))
#define cHdl(x)			((Hdl)(x))
#define cHdl3(x)		((Hdl3)(x))
#define cFun(x)			((Fun)(x))
#define cVFun(x)		((VFun)(x))
#define cChar(x)		((Char)(x))
#define cCharPt(x)		((CharPt)(x))
#define cCharHdl(x)		((CharHdl)(x))
#define cUChar(x)		((UChar)(x))
#define cUCharPt(x)		((UCharPt)(x))
#define cWChar(x)		((WChar)(x))
#define cVoidPt(x)		((VoidPt)(x))
#define cC99Fix(x)		((VoidPt)(x)) /* Ensures strict-aliasing rules of C99 */
#define cInstPt(x)		((InstPt)(x))

#define Ignore(x)		((void)(x))
#define Do(c)			do { c } while( 0 )

#define maxWord			(~cWord(0))

#define elif			else if
#define doint(i,a,n)	for( i = a ; i < n ; i++ )
#define dotimes(i,n)	doint(i,0,n)
#define dotimesrev(i,n)	for( i = n - 1 ; i >= 0 ; i-- )
#define doseq(p,i,f)	for( p = i ; p != nil ; p = f )
#define dotable(e,t,s)	for( e = t ; e < t + s ; e++ )

#define K					* 1024L
#define WordsAsBytes(w)		((w) * sizeof(Word))
#define WordsAsKBytes(w)	(WordsAsBytes(w)/1024L)

#define false			0
#define true			1

#define false3			0	/* LOG3 type constants */
#define true3			1
#define absent3			2

#define even(n)			( ((n) & 0x1) == 0 )
#define odd(n)			( ((n) & 0x1) != 0 )

#define Remainder(x,q)	((x) % (q))
#define RoundDown(x,q)	( (x) - Remainder(x,q) )
#define RoundUp(x,q)	RoundDown( (x) + q - 1, q )
#define Round(x,q)		RoundDown(x,q) + ( Remainder(x,q) > (q)/2 ) * (q)
#define DivUp(x,q)		( ((x) + (q) - 1) / (q) )
#define Words(bytes)	DivUp(bytes, sizeof(Word))
#define WordsOf(type)	Words(sizeof(type))

#define CheckRoundTowardZero()	((-5)/2 == -2)

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
void ClearWords(VoidPt h, Size len) ;
Bool CheckIfZeroedBytes(VoidPt v, Size len) ;
void CopyBytes(UCharPt zz, UCharPt aa, Size len) ;
void CopyWords(Hdl zz, Hdl aa, Size len) ;
void CopyWordsReloc(Hdl zz, Hdl aa, Size len) ;
void ShiftWords(Hdl h, Size len, Size offset) ;

#endif
