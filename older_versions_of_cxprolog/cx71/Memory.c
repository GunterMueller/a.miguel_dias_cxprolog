/*
 *   This file is part of the CxProlog system

 *   Memory.c
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


/* AllocAligned */

static VoidPt Align(VoidPt pt)
{
	CharPt p = pt ;
	
	while( cInt(p) % sizeof(Word) != 0 ) p++ ;
	return p ;
}

VoidPt AllocAligned(long bytes)
{
	CharPt mem ;

	if( (mem = malloc(bytes + sizeof(Word))) == nil )
		return nil ;
	else
		return Align(mem) ;
}


/* BUFFERS */

#define strBufferSize		   (20 Kb)

Hdl codeBuffer, codeBufferEnd ;
CharPt strBuffer, strBufferEnd ;

static void InitBuffers()
{
	if( (codeBuffer = AllocAligned(maxCodePerClause)) == nil )
		FatalError("Not enough memory to create initial buffers\n") ;
	codeBufferEnd = codeBuffer + Words(maxCodePerClause) - 100 ;

	if( (strBuffer = AllocAligned(strBufferSize)) == nil )
		FatalError("Not enough memory to create initial buffers\n") ;
	strBufferEnd = strBuffer + strBufferSize - 3 ;
}


/* MEMORY MANAGER */

#define memoryBlockSize		 (100 Kb)
#define HOffset(f,n)		((HeaderPt)(cPt(f) + n))

typedef struct Header
{
	long size ;
	struct Header *next ;
} Header, *HeaderPt ;

static long totalMemory ;
static Pt memA, memZ ;
static HeaderPt freeList ;

static void MoreMemory(long atLeast)
{
	if( atLeast > Words(memoryBlockSize) )
		FatalError("Attempt to allocate too large a block of memory") ;
	if( (memA = AllocAligned(memoryBlockSize)) == nil )
		Error("No more computer memory available\n") ;
	memZ = memA + Words(memoryBlockSize) ;
	totalMemory += Words(memoryBlockSize) ;
}

void InitMemory()
{
	InitBuffers() ;
	totalMemory = 0 ;
	MoreMemory(0) ;
	freeList = nil ;
}

VoidPt PermanentAllocate(int nWords)
{
	if( nWords <= 1 )
		InternalError("PermanentAllocate: nWords<=1") ;

	if( memZ - memA < nWords ) {
		if( memZ - memA >= 3 ) {
			HeaderPt q = (HeaderPt)memA ;
			q->size = memZ - memA ;
			free(HOffset(q, 1)) ;
		}
		MoreMemory(nWords) ;
	}

	return memZ -= nWords ;
}

VoidPt TemporaryAllocate(int nWords)
{
	register HeaderPt *p, q ;
	
	if( nWords <= 1 )
		InternalError("Allocate: nWords<=1") ;
	
	nWords++ ;
	for( p = &freeList ; *p != nil && (**p).size < nWords ; p = &((**p).next) ) ;
	if( *p == nil ) {
		if( memZ - memA < nWords ) {
			if( 0 &&  memZ - memA >= 3 ) {
				q = (HeaderPt)memA ;
				q->size = memZ - memA ;
				free(HOffset(q, 1)) ;
			}
			MoreMemory(nWords) ;
		}
		q = (HeaderPt)memA ;
		memA += nWords ;
		q->size = nWords ;
	}
	elif( (**p).size - nWords < 3 ) {	/* one or two words may rest unused */
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

void Release(VoidPt ptr)
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

long TotalMemory()
{
	return totalMemory ;
}

long MemoryUsed()
{
	register HeaderPt p ;
	register long size ;
	
	size = 0 ;
	dolist(p, freeList, p->next) {
		size += p->size ;
#if 0
		printf("%ld ", p->size) ;
#endif
	}
	return totalMemory - size - (memZ - memA) ;
}


/*
void MemoryBlockResize(VoidPt ptr, long newSize)
{
	register HeaderPt b = (Header *)ptr - 1 ;
	long oldSize = b->size ;

	if( newSize > oldSize ) FatalError("CodeArea block size can only be reduced") ;
	b->size = newSize ;

		(**p).size -= nUnits ;
		q = *p + (**p).size ;
		q->size = nUnits ;
}

void ListMemory()
{
	HeaderPt p ;
	
	dolist(p, freeList, p->next)
		WriteStd("%8ld %8ld\n", p, p->size) ;
	WriteStd("\n") ;
}
*/
