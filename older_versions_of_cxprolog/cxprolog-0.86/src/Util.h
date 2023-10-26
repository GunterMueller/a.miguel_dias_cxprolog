/*
 *   This file is part of the CxProlog system

 *   Util.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Util_
#define _Util_

typedef unsigned long Word ;
typedef long Size ;
typedef long PInt ;
typedef long double PFloat ;

typedef unsigned short Bool ;
typedef char *CharPt ;
typedef char Str16[16], Str256[256], Str1000[1000], Str2000[2000] ;
typedef unsigned char *UCharPt ;
typedef void *VoidPt ;
typedef Word *Pt ;
typedef Pt *Hdl ;
typedef Hdl *Hdl3 ;
typedef void (*Proc)(void) ;
typedef void (*Proc1)(int) ;
typedef void (*Proc2)(int,int) ;
typedef void (*ProcV)(VoidPt) ;
typedef Bool (*BoolFun)(void) ;
typedef Bool (*BoolFun1)(int) ;
typedef Bool (*BoolFun2)(int,int) ;
typedef struct {
	union {
		Pt asPt ;
		PInt asPInt ;
		PFloat asPFloat ;
	} val ;
	enum { nPt, nPInt, nPFloat } kind ; 
} NMix, *NMixPt ;
typedef union {
	Word asWord ;
	Pt asPt ;
	Hdl asHdl ;
	CharPt asCharPt ;
	UCharPt asUCharPt ;
	NMixPt asNMixPt ;
} Mix, *MixPt ;

#define cWord(x)		((Word)(x))
#define cPInt(x)		((PInt)(x))
#define cPFloat(x)		((PFloat)(x))
#define cPt(x)			((Pt)(x))
#define cHdl(x)			((Hdl)(x))
#define cHdl3(x)		((Hdl3)(x))
#define cProc(x)		((Proc)(x))
#define cProcV(x)		((ProcV)(x))
#define cCharPt(x)		((CharPt)(x))
#define cUCharPt(x)		((UCharPt)(x))
#define cVoidPt(x)		((VoidPt)(x))

#define maxWord			(~cWord(0))
#define maxAddr			cVoidPt(maxWord)

#define elif			else if
#define dotimes(i, n)	for( i = 0 ; i < n ; i++ )
#define dotimesrev(i,n)	for( i = n - 1 ; i >= 0 ; i-- )
#define doseq(p,i,f)	for( p = i ; p != nil ; p = f )
#define dotable(e,t,s)	for( e = t ; e < t + s ; e++ )

#define K					* 1024L
#define WordsAsBytes(w)		((w) * sizeof(Word))
#define WordsAsKBytes(w)	(WordsAsBytes(w)/1024L)

#define eofPt			cPt(-1)

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

#define EqualStr(a,b)	( strcmp(a, b) == 0 )
#define EqualStrN(a,b,n) ( strncmp(a, b, n) == 0 )

#undef nil
#define	nil				(0L)

#define ToUpperCase(x)	( (x) >= 'a' && (x) <= 'z' ? (x) - 'a' + 'A' : (x) )
#define ToLowerCase(x)	( (x) >= 'A' && (x) <= 'Z' ? (x) - 'A' + 'a' : (x) )


#define retBufferSize	(256 * sizeof(Word))
extern char retBuffer[] ;

VoidPt PrimitiveAllocate(Size nWords) ;
VoidPt PrimitiveAllocateAndClear(Size nWords) ;
VoidPt PrimitiveRellocate(VoidPt mem, Size copySize, Size newSize) ;
void PrimitiveRelease(VoidPt mem) ;
void ClearBytes(VoidPt v, Size len) ;
void ClearWords(Hdl h, Size len) ;
void CopyBytes(CharPt zz, CharPt aa, Size len) ;
void CopyWords(Hdl zz, Hdl aa, Size len) ;
void CopyWordsRelloc(Hdl zz, Hdl aa, Size len) ;
void ShiftWords(Hdl h, Size len, Size offset) ;
Bool SimilarStr(CharPt a, CharPt b, int n, Bool ignoreCase) ;
CharPt StrUpper(CharPt s) ;

#endif