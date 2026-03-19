/*
 *   This file is part of the CxProlog system

 *   Util.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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
#include <ctype.h>


/* RETURN BUFFER */

Char retString[retStringSize] ;


/* Handle BYTES & WORDS */

void ClearBytes(VoidPt v, register Size len)
{
	register CharPt s = v ;
	while( len-- ) *s++ = 0 ;
}

void ClearWords(register Hdl h, register Size len)
{
	while( len-- ) *h++ = 0 ;
}

void CopyBytes(register CharPt z, register CharPt a, Size len)
{
	while( len-- )
		*z++ = *a++ ;
}

void CopyWords(register Hdl z, register Hdl a, Size len)
{
	while( len-- )
		*z++ = *a++ ;
}

void CopyWordsReloc(Hdl zz, Hdl aa, Size len)
{
	register Hdl z = zz, a = aa, an = a + len ;
	register Size offset = z - a ;
	while( len-- )
		if( InRange(*a, cPt(aa), cPt(an)) )
			*z++ = *a++ + offset ;
		else
			*z++ = *a++ ;
}

void ShiftWords(Hdl h, Size len, Size offset)
{
	register Hdl a, z ;
	for( a = h + len - 1, z = a + offset ; len-- ; a--, z-- )
		*z = *a ;
}


/* OTHER */

int HashStr(register CharPt n)
{
	register int val = 0 ;
#if 1
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;

	if( *n == '\0' ) return val ; else val += *n++ ;		
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;

	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;

	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;
	if( *n == '\0' ) return val ; else val += *n++ ;		
	if( *n == '\0' ) return val ; else val += *n++ ;
#else
	while( *n ) val += *n++ ;
#endif
	return val ;
}

Bool SimilarStr(CharPt a, CharPt b, int n, Bool ignoreCase)
{
	if( ignoreCase )
		while( n-- ) {
			if( *a == 0 || *b == 0 ) return *a == *b ;
			if( toupper(*a) != toupper(*b)) return false ;
			a++ ; b++ ;
		}
	else
		while( n-- ) {
			if( *a == 0 || *b == 0 ) return *a == *b ;
			if( *a++ != *b++) return false ;
		}
	return true ;
}

CharPt StrUpper(register CharPt s)
{
	register CharPt z = retString ;
	while( (*z++ = toupper(*s++)) ) ;
	return retString ;
}
