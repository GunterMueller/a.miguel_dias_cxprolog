/*
 *   This file is part of the CxProlog system

 *   Utf8.h
 *   by A.Miguel Dias - 2004/12/19
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Utf8_
#define _Utf8_

#define USE_UTF8			1

#if USE_WIDECHARS && USE_UTF8
/* Use UTF-8 as the internal encoding of text */
#if ULONG_MAX == 0xFFFFFFFFL
#define CharValidCode(c)	( true )
#else
#define CharValidCode(c)	( (c) <= 0x8FFFFFFF )
#endif
#define CharType(c)			( ((c)<256) ? BasicCharType(c) : _LC )
#define CharEncode(s,c)		( Utf8Encode(&(s),c) )
#define CharDecode(s)		( Utf8Decode(&(s)) )
#define CharCopy(z,a)		( Utf8Copy(&(z),&(a)) )
#define CharFirst(s)		( Utf8First(s) )
#define CharLast(s)			( Utf8Last(s) )
#define CharPos(s,i)		( Utf8Pos(s, i) )
#define CharLen(s)			( Utf8Len(s) )

/* ENCODE UTF-8 */
void Utf8Encode(CharPt *s, int c) ;

/* DECODE UTF-8 */
int Utf8Decode(CharPt *s) ;

/* COPY UTF-8 */
void Utf8Copy(CharPt *z, CharPt *a) ;
void Utf8NCopy(CharPt *z, CharPt *a, Size n) ;

/* INDEXING UTF-8 */
int Utf8First(CharPt s) ;
int Utf8Last(CharPt s) ;
CharPt Utf8Pos(CharPt s, int i) ;
Size Utf8Len(CharPt s) ;

#else
/* Use Latin-1 as the internal encoding of text */
#define CharValidCode(c)	( (c) <= 255 )
#define CharType(c)			( BasicCharType(c) )
#define CharEncode(s,c)		( Ignore(*(s)++ = cChar(c)) )
#define CharDecode(s)		( *cUCharPt((s)++) )
#define CharCopy(z,a)		( Ignore(*(z)++ = *(a)++) )
#define CharFirst(s)		( cUCharPt(s)[0] )
#define CharLast(s)			( cUCharPt(s)[strlen(s)-1] )
#define CharPos(s,i)		( (s) + (i) )
#define CharLen(s)			( strlen(s) )
#endif

#endif
