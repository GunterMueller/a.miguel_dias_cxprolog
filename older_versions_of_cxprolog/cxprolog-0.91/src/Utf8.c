/*
 *   This file is part of the CxProlog system

 *   Utf8.c
 *   by A.Miguel Dias - 2004/12/19
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define IsContinuationByte(b)	( ((b) & 0xc0) == 0x80 ) 
#define TagContinuation(b)		( 0x80 | ((b) & 0x3f) ) 

/* ENCODE UTF-8 */

/* Convert a wide character to a UTF-8 sequence. */

void Utf8Encode(CharPt *s, int c)
{
	register CharPt pt = *s ;

	if( c < 0x80 ) {				/* 7 bits */
		*s += 1 ;
		pt[0] = c ;
	}
	elif( c < 0x800 ) {				/* 11 bits */
		*s += 2 ;
		pt[0] = 0xc0 + ((unsigned)c >> 6) ;
		pt[1] = TagContinuation(c) ;
	}
	elif( c < 0x10000 ) {			/* 16 bits */
		*s += 3 ;
		pt[0] = 0xe0 + ((unsigned)c >> 12) ;
		pt[1] = TagContinuation((unsigned)c >> 6) ;
		pt[2] = TagContinuation(c) ;
	}
	elif( c < 0x200000 ) {			/* 21 bits */
		*s += 4 ;
		pt[0] = 0xf0 + ((unsigned)c >> 18) ;
		pt[1] = TagContinuation((unsigned)c >> 12) ;
		pt[2] = TagContinuation((unsigned)c >> 6) ;
		pt[3] = TagContinuation(c) ;
	}
	elif( c < 0x4000000 ) {			/* 26 bits */
		*s += 5 ;
		pt[0] = 0xf8 + ((unsigned)c >> 24) ;
		pt[1] = TagContinuation((unsigned)c >> 18) ;
		pt[2] = TagContinuation((unsigned)c >> 12) ;
		pt[3] = TagContinuation((unsigned)c >> 6) ;
		pt[4] = TagContinuation(c) ;
	}
	else {							/* 31 bits */
		*s += 6 ;
		pt[0] = 0xfc + ((unsigned)c >> 30) ;
		pt[1] = TagContinuation((unsigned)c >> 24) ;
		pt[2] = TagContinuation((unsigned)c >> 18) ;
		pt[3] = TagContinuation((unsigned)c >> 12) ;
		pt[4]  = TagContinuation((unsigned)c >> 6) ;
		pt[5]  = TagContinuation(c) ;
	}
}


/* DECODE UTF-8 */

/* Length of a UTF-8 sequence from the first byte. Illegal bytes have a one. */

static Char utf8Len[256] =
{
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /*invalid*/
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /*invalid*/
	2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
	3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1,
};
																				
/* Convert a UTF-8 sequence to a wide character.  */

int Utf8Decode(CharPt *s)
{
	register CharPt pt = *s ;

	if( *pt < 0x80 ) {
		*s += 1 ;
		return pt[0] ;
	}

#define pt(n)		(pt[n] & 0x3f)
	switch( utf8Len[**s] ) { 
		case 2: {
			*s += 2 ;
			return ((pt[0] & 0x1f) << 6) + pt(1) ;
		}
		case 3: {
			*s += 3 ;
			return ((pt[0] & 0x0f) << 12) + (pt(1) << 6) + pt(2) ;
		}
		case 4: {
			*s += 4 ;
			return ((pt[0] & 0x07) << 18) + (pt(1) << 12) + (pt(2) << 6)
						+ pt(3) ;
		}
		case 5: {
			*s += 5 ;
			return ((pt[0] & 0x03) << 24) + (pt(1) << 18) + (pt(2) << 12)
						+ (pt(3) << 6) + pt(4) ;
		}
		case 6: {
			*s += 6 ;
			return ((pt[0] & 0x01) << 30) + (pt(1) << 24) + (pt(2) << 18)
						+ (pt(3) << 12) + (pt(4) << 6) + pt(5) ;
		}
		default:
			return IInternalError("Utf8Decode") ;		
	}
}


/* COPY UTF-8 */

/* Copy UTF-8 sequence.  */

void Utf8Copy(register CharPt *z, register CharPt *a) /* pre: *a != '\0' */
{
	do {
		*(*z)++ = *(*a)++ ;
	} while( IsContinuationByte(**a) ) ;
}

/* Copy N UTF-8 sequences.  */

void Utf8NCopy(register CharPt *z, register CharPt *a, register Size n)
{
	while( n-- && **a ) {
		do {
			*(*z)++ = *(*a)++ ;
		} while( IsContinuationByte(**a) ) ;
	}
}		


/* INDEXING UTF-8 */

/* Get first character from UTF-8 string.  */

int Utf8First(CharPt s)
{
	return Utf8Decode(&s) ;
}

/* Get last character from UTF-8 string.  */

int Utf8Last(CharPt s)
{
	int len = strlen(s) ;
	if( len == 0 ) return '\0' ;
	s += len - 1 ;
	while( IsContinuationByte(*s) ) s-- ;
	return Utf8Decode(&s) ;
}

/* Get the position of the i-th character in a UTF-8 string.  */

CharPt Utf8Pos(register CharPt s, register int i)
{
	for( ; *s ; s++ ) {
		if( !IsContinuationByte(*s) ) {
			if( i == 0 ) return s ;
			else i-- ;
		}
	}
	return s ;
}		

/* Get the position of the i-th character in a UTF-8 string.  */

Size Utf8Len(register CharPt s)
{
	Size len = 0 ;
	for( ; *s ; s++ )
		if( !IsContinuationByte(*s) )
			len++ ;
	return len ;
}
