/*
 *   This file is part of the CxProlog system

 *   Memory.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define memoryBlockSize		32 K /* words */
#define blockSlack			2
#define HOffset(f,n)		((HeaderPt)(cPt(f) + n))

typedef struct Header
{
	Word size ;
	struct Header *next ;
} Header, *HeaderPt ;

static Size staticMemory ;
static Pt memA, memZ ;
static HeaderPt freeList ;
Bool memoryManagerActive = false ;

static void MoreMemory(Size atLeastWords)
{
	Size words ;
	Pt mem ;
	for( words = memoryBlockSize ; words < atLeastWords ;
								words += memoryBlockSize ) ;
	if( atLeastWords > 0 )
		MemoryGrowWarning("static area", staticMemory,
										staticMemory + words, nil) ;
	if( memZ - memA > blockSlack ) {
		HeaderPt q = (HeaderPt)memA ;
		q->size = memZ - memA ;
		BlockRelease(HOffset(q, 1)) ;
	}
	mem = PrimitiveAllocate(words) ;
	staticMemory += words ;
	memA = mem ;
	memZ = mem + words ;
}

void MemoryInit()
{
	staticMemory = 0 ;
	memZ = memA = nil ;
	MoreMemory(0) ;
	freeList = nil ;
	memoryManagerActive = true ;
}

VoidPt PermBlockAllocate(Size nWords)
{
	if( nWords <= 0 )
		InternalError("PermBlockAlloc: nWords<=0") ;
	if( memZ - memA < nWords ) {
		/*AtomsGC() ;*/
		MoreMemory(nWords) ;
	}
	return memZ -= nWords ;
}

VoidPt TempBlockAllocate(Size nWords)
{
	register HeaderPt *p, q = nil ;

	if( nWords <= 0 )
		InternalError("TempBlockAlloc: nWords<=0") ;
	
	nWords++ ;	/* Extra word to register the block size in each block */
	for( p = &freeList ; *p != nil && (**p).size < nWords ; p = &((**p).next) ) ;
	if( *p == nil ) {
		if( memZ - memA < nWords ) {
			/*if( q == nil )
				{ AtomsGC() ; q = freeList ; goto redo ; }
			else*/ MoreMemory(nWords) ;
		}
		q = (HeaderPt)memA ;
		memA += nWords ;
		q->size = nWords ;
	}
	elif( (**p).size - nWords <= blockSlack ) {	/* some words may rest unused */
		q = *p ;
		*p = (**p).next ;
	}
	else {
		(**p).size -= nWords ;
		q = HOffset(*p, (**p).size) ;
		q->size = nWords ;
	}
	return HOffset(q, 1) ;
}

void BlockRelease(VoidPt ptr)
{
	register HeaderPt p, n, f = HOffset(ptr, -1) ;

	if( freeList == nil ) {
		f->next = nil ;
		freeList = f ;
		return ;
	}
	
	if( f < freeList ) {	/* f is to be the first block */
		if( HOffset(f,f->size) == freeList )	/* join to upper block */ {
			f->size += freeList->size ;
			f->next = freeList->next ;
		}
		else f->next = freeList ;
		freeList = f ;
		return ;
	}
		
	for( p = freeList ; (n = p->next) != nil && n < f ; p = n ) ;
	
	if( HOffset(p,p->size) == f ) {	/* join to lower block */
		p->size += f->size ;
		f = p ;
	}
	else p->next = f ;
	
	if( HOffset(f,f->size) == n ) {	/* join to upper block */
		f->size += n->size ;
		f->next = n->next ;
	}
	else f->next = n ;
}

Size BlockSize(VoidPt ptr)
{
	return (HOffset(ptr, -1))->size - 1 ;
}

Size StaticMemory()
{
	return staticMemory ;
}

Size StaticMemoryUsed()
{
	register HeaderPt p ;
	register Size size ;
	
	size = 0 ;
	doseq(p, freeList, p->next) {
		size += p->size ;
#if 0
		WriteStd("%ld ", p->size) ;
#endif
	}
	return staticMemory - size - (memZ - memA) ;
}


/*
void MemoryBlockResize(VoidPt ptr, Size newSize)
{
	register HeaderPt b = (Header *)ptr - 1 ;
	Size oldSize = b->size ;

	if( newSize > oldSize )
		FatalError("CodeArea block size can only be reduced") ;
	b->size = newSize ;

		(**p).size -= nUnits ;
		q = *p + (**p).size ;
		q->size = nUnits ;
}

void ListMemory()
{
	HeaderPt p ;
	
	doseq(p, freeList, p->next)
		Write("%8ld %8ld\n", p, p->size) ;
	Write("\n") ;
}
*/
