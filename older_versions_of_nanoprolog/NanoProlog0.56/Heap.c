/*
 *   This file is part of the NanoProlog system

 *   Heap.c
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

 931117: release of version 0.5

*/

#include "NanoProlog.h"

static Hdl heapBegin, heapCurr, heapCurr2, heapEnd ;

void AreaStatistics(pre, name, space, used)
char *pre ;
char *name ;
long space, used ;
{
	printf("%14s%10s: %3ldK (%6ld bytes used)\n",
					pre, name, space Wd/(1 Kb), used Wd) ;
}

void HeapStatistics()
{
	AreaStatistics("", "Heap", heapEnd - heapBegin, HeapUsed()) ;
}


/* HEAP MEMORY ALLOCATION */

#define UnitsToWords(u)		( (u) * WordsOf(Header) )										

typedef struct Header
{
	struct Header *next ;
	long size ;
} Header ;

static Header *freeList = nil ;

void CreateHeap(long size)
{
	heapCurr = heapBegin = TAlloc(size) ;
	if( heapBegin == nil ) Error("Not enough memory") ;
	heapCurr2 = heapEnd = heapBegin + size / sizeof(Word) ;
}

void *HeapAlloc(int nWords, Bool permanent)
{
	register Header **p, *q ;
	int nUnits ;
	
	if( permanent )
	{
		GrowAreaR(heapCurr2, heapCurr, nWords, "Heap") ;
		return(heapCurr2) ;
	}

	nUnits = 1 + DivUp(nWords, WordsOf(Header)) ;
	for( p = &freeList ; *p != nil && (**p).size < nUnits ; p = &((**p).next) ) ;

	if( *p == nil )
	{
		q = (Header *)heapCurr ;
		GrowArea(heapCurr, heapCurr2, UnitsToWords(nUnits), "Heap") ;
		q->size = nUnits ;
	}
	elif( (**p).size == nUnits )
	{
		q = *p ;
		*p = (**p).next ;
	}
	else
	{
		(**p).size -= nUnits ;
		q = *p + (**p).size ;
		q->size = nUnits ;
	}
	return( q + 1 ) ;
}

void HeapFree(void *ptr)
{
	register Header *p, *n, *f = (Header *)ptr - 1 ;

	if( freeList == nil )
	{
		f->next = nil ;
		freeList = f ;
		return ;
	}
	
	if( f < freeList )	/* f is to be the first block */
	{
		if( f + f->size == freeList )	/* join to upper block */
		{
			f->size += freeList->size ;
			f->next = freeList->next ;
		}
		else f->next = freeList ;
		freeList = f ;
		return ;
	}
		
	for( p = freeList ; (n = p->next) != nil && n < f ; p = n ) ;
	
	if( p + p->size == f )	/* join to lower block */
	{
		p->size += f->size ;
		f = p ;
	}
	else p->next = f ;
	
	if( f + f->size == n )	/* join to upper block */
	{
		f->size += n->size ;
		f->next = n->next ;
	}
	else f->next = n ;
}

/*
void HeapBlockResize(void *ptr, long newSize)
{
	register Header *b = (Header *)ptr - 1 ;
	long oldSize = b->size ;

	if( newSize > oldSize ) FatalError("Heap block size can only be reduced") ;
	b->size = newSize ;

		(**p).size -= nUnits ;
		q = *p + (**p).size ;
		q->size = nUnits ;
}
*/

long HeapUsed()
{
	Header *p ;
	long units ;
	
	units = 0 ;
	foreach(p, freeList, p->next) units += p->size ;
	return( ( heapCurr - heapBegin ) + ( heapEnd - heapCurr2 )
							- UnitsToWords(units) ) ;
}

ListHeap()
{
	Header *p ;
	
	foreach(p, freeList, p->next)
		printf("%8ld %8ld\n", p, p->size) ;
	printf("\n") ;
}
