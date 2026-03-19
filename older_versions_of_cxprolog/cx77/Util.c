/*
 *   This file is part of the CxProlog system

 *   Util.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"


/* RETURN BUFFER */

char retBuffer[retBufferSize] ;


/* PRIMITIVE ALLOCATE */

static VoidPt Align(VoidPt pt)
{
	CharPt p = pt ;
	while( cWord(p) % sizeof(Word) != 0 ) p++ ;
	return p ;
}

VoidPt PrimitiveAllocate(Size nWords)
{
	CharPt mem ;
	if( (mem = malloc(WordsAsBytes(nWords+1))) == nil )
		Error("No more computer memory available") ;
	return Align(mem) ;
}

void PrimitiveRelease(VoidPt mem)
{
	free(mem) ;
}


/* COPY BYTES & WORDS */

void CopyBytes(CharPt zz, CharPt aa, Size len)
{
	register CharPt z = zz, a = aa ;
	
	while( len-- )
		*z++ = *a++ ;
}

void CopyWords(Hdl zz, Hdl aa, Size len)
{
	register Hdl z = zz, a = aa ;
	
	while( len-- )
		*z++ = *a++ ;
}

void CopyWordsRelloc(Hdl zz, Hdl aa, Size len)
{
	register Hdl z = zz, a = aa, an = a + len ;
	register Size offset = z - a ;
	
	while( len-- )
		if( InRange(*a, cPt(aa), cPt(an)) )
			*z++ = *a++ + offset ;
		else
			*z++ = *a++ ;
}
