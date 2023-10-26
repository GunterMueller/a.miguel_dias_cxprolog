/*
 *   This file is part of the CxProlog system

 *   String.h
 *   by A.Miguel Dias - 2004/12/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _String_
#define _String_

#define StrEqual(a,b)		( strcmp(a, b) == 0 )
#define StrEqualN(a,b,n)	( strncmp(a, b, n) == 0 )
#define StrCompare(a,b)		( CharReorderCompare(a, b) )

int StrHash(CharPt s) ;
CharPt StrAllocate(CharPt str) ;
int StrSeqLength(CharPt *vs) ;
int StrSeqGetIdx(CharPt a, CharPt *vs) ;
CharPt StrSeqGetVal(int i, CharPt *vs) ;
CharPt StrSeqFormat(CharPt pre, CharPt sep, CharPt pos, CharPt *vs) ;


/* GROWINGS TEMPORARY STRINGS */

#define GStrCheck()			( ( gStrPt >= gStrEnd ) ? GStrExpand() : 0 )
#define GStrAddByte(c)		( GStrCheck(), *gStrPt++ = (c) )
#define GStrAddChar(c)		( GStrCheck(), CharEncode(gStrPt,c) )
#define GStrBack(n)			( gStrPt -= (n) )
#define GStrExtend(n)		( GStrCheck(), gStrPt += (n) )
#define GStrBegin()			( gStrBegin )
#define GStrCurr()			( gStrPt )
#define GStrBackTo(pt)		( gStrPt = (pt) )

extern CharPt gStrBegin, gStrEnd, gStrPt ;

int GStrExpand(void) ;
Size GStrCapacity(void) ;
void GStrOpen(void) ;
CharPt GStrClose(void) ;
void GStrAddStr(CharPt s) ;
void GStrAddStrConv(CharPt s, WChar oldChar, WChar newChar) ;
void GStrAddStrSlice(CharPt s, int a, int b) ;
CharPt GStrFormatV(CharPt fmt, va_list v) ;
CharPt GStrFormat(CharPt fmt, ...) ;
CharPt GStrMake(CharPt s) ;
CharPt GStrMakeSegm(CharPt s, CharPt end) ;


/* BIG STRING */

#define BigStrCheck()			( ( bigStrPt >= bigStrEnd ) ? BigStrExpand() : 0 )
#define BigStrAddByte(c)		( BigStrCheck(), *bigStrPt++ = (c) )
#define BigStrAddChar(c)		( BigStrCheck(), CharEncode(bigStrPt,c) )
#define BigStrMarkNull()		( *bigStrPt = '\0' )

#define BigStrBegin()			( bigStrBegin )
#define BigStrCurr()			( bigStrPt )
#define BigStrBackTo(pt)		( bigStrPt = (pt) )
#define BigStrOffset(pt)		( (pt) - bigStrBegin )
#define BigStrAddr(offset)		( bigStrBegin + cPInt(offset) )

extern CharPt bigStrBegin, bigStrEnd, bigStrPt;
extern CharPt bigStrAPt, bigStrBPt ;

int BigStrExpand(void) ;
Size BigStrCapacity(void) ;
void BigStrOpen(void) ;
CharPt BigStrClose(void) ;
void BigStrInit(void) ;
void BigStrAddStr(CharPt s) ;
void BigStrAddStrConv(CharPt s, WChar oldChar, WChar newChar) ;
void BigStrAddStrSlice(CharPt s, int a, int b) ;


/* INIT */

void StringInit(void) ;

#endif
