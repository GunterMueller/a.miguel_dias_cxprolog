/*
 *   This file is part of the CxProlog system

 *   Space.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* BUFFERS */

CharPt strBuffer, strBufferEnd ;
Hdl codeBuffer, codeBufferEnd ;
static Hdl spaceBegin, spaceCurr, spaceCurr2, spaceEnd ;


/* CODE AREA MEMORY ALLOCATION */

#define UnitsToWords(u)		( (u) * WordsOf(Header) )										

typedef struct Header
{
	struct Header *next ;
	long size ;
} Header ;

static Header *freeList = nil ;

void InitSpace()
{
	CharPt all ;
	if( (all = AllocAligned(codeAreaSize + strBufferSize + codeBufferSize)) == nil )
		Error("Not enough memory") ;
	
	spaceCurr = spaceBegin = cHdl(all) ;
	spaceCurr2 = spaceEnd = cHdl(all + codeAreaSize) ;

	strBuffer = cCharPt(all + codeAreaSize) ;
	strBufferEnd = cCharPt(all + codeAreaSize + strBufferSize) ;

	codeBuffer = cHdl(all + codeAreaSize + strBufferSize) ;
	codeBufferEnd = cHdl(all + codeAreaSize + strBufferSize + codeBufferSize) - 3 ;
}

VoidPt SpaceAlloc(int nWords, Bool permanent)
{
	register Header **p, *q ;
	int nUnits ;
	
	if( nWords <= 0 )
		FatalError("SpaceAlloc: nWords<=0") ;
	
	if( permanent ) {
		GrowAreaR(spaceCurr2, spaceCurr, nWords, "Code area") ;
		return spaceCurr2 ;
	}

	nUnits = 1 + DivUp(nWords, WordsOf(Header)) ;
	for( p = &freeList ; *p != nil && (**p).size < nUnits ; p = &((**p).next) ) ;

	if( *p == nil ) {
		q = (Header *)spaceCurr ;
		GrowArea(spaceCurr, spaceCurr2, UnitsToWords(nUnits), "Space") ;
		q->size = nUnits ;
	}
	elif( (**p).size == nUnits ) {
		q = *p ;
		*p = (**p).next ;
	}
	else {
		(**p).size -= nUnits ;
		q = *p + (**p).size ;
		q->size = nUnits ;
	}
	return q + 1 ;
}

void SpaceFree(VoidPt ptr)
{
	register Header *p, *n, *f = (Header *)ptr - 1 ;

	if( freeList == nil ) {
		f->next = nil ;
		freeList = f ;
		return ;
	}
	
	if( f < freeList ) {	/* f is to be the first block */
		if( f + f->size == freeList )	/* join to upper block */ {
			f->size += freeList->size ;
			f->next = freeList->next ;
		}
		else f->next = freeList ;
		freeList = f ;
		return ;
	}
		
	for( p = freeList ; (n = p->next) != nil && n < f ; p = n ) ;
	
	if( p + p->size == f ) {	/* join to lower block */
		p->size += f->size ;
		f = p ;
	}
	else p->next = f ;
	
	if( f + f->size == n ) {	/* join to upper block */
		f->size += n->size ;
		f->next = n->next ;
	}
	else f->next = n ;
}

/*
void SpaceBlockResize(VoidPt ptr, long newSize)
{
	register Header *b = (Header *)ptr - 1 ;
	long oldSize = b->size ;

	if( newSize > oldSize ) FatalError("Space block size can only be reduced") ;
	b->size = newSize ;

		(**p).size -= nUnits ;
		q = *p + (**p).size ;
		q->size = nUnits ;
}
*/

long SpaceUsed()
{
	Header *p ;
	long units ;
	
	units = 0 ;
	dolist(p, freeList, p->next) units += p->size ;
	return ( spaceCurr - spaceBegin ) + ( spaceEnd - spaceCurr2 )
							- UnitsToWords(units) ;
}

long StacksSize()
{
	return spaceEnd - spaceBegin ;
}

void ListSpace()
{
	Header *p ;
	
	dolist(p, freeList, p->next)
		WriteStd("%8ld %8ld\n", p, p->size) ;
	WriteStd("\n") ;
}
