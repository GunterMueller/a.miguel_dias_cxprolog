/*
 *   This file is part of the CxProlog system

 *   Memory.c
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

/*
 * THE ELASTIC STRUCTURES:
 *   Code Area (file Memory.c),
 *   ScratchPad (file Scratch.c),
 *   Global/Local stacks (file Machine.c),
 *   Trail/Finalizers stacks (file Machine.c)
 */

#include "CxProlog.h"
#include "malloc.h"

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
	
#ifdef M_MMAP_THRESHOLD
/* Considering the allocation patterns of CxProlog, malloc should
   request memory to the system via mmap and not brk. */
	static Bool firstTime = true ;
	if( firstTime ) {
		mallopt(M_MMAP_THRESHOLD, 0) ;
		firstTime = false ;
	}
#endif

	if( (mem = malloc(WordsAsBytes(nWords+1))) == nil )
		FatalError("No more computer memory available") ;

#if 0
	Mesg("%7ldKB - 0x%lx", nWords / 1024 * 4, cWord(mem)) ;
#endif
	
	if( GetTag(mem) != tagVar ) {
		if( tagVar == 0 )
			FatalError("Invalid memory allocation on FORWARDS machine") ;
		else
			FatalError("Invalid memory allocation on BACKWARDS machine") ;
	}
	return Align(mem) ;
}

VoidPt PrimitiveAllocateAndClear(Size nWords)
{
	Hdl mem = PrimitiveAllocate(nWords) ;
	ClearWords(mem, nWords) ;
	return mem ;
}

VoidPt PrimitiveRelocate(VoidPt mem, Size copySize, Size newSize)
{
    VoidPt newMem ;
    if( copySize > newSize )
        InternalError("PrimitiveReallocate") ;
    newMem = PrimitiveAllocate(newSize) ;
    CopyWords(cHdl(newMem), cHdl(mem), copySize) ;
    PrimitiveRelease(mem) ;
    return newMem ;
}

void PrimitiveRelease(VoidPt mem)
{
	free(mem) ;
}

/* CODE AREA ALLOCATE */

#define initialBlockSize	32 K /* words */
#define maxBlockSize		128 K /* words */
#define blockSlack			2
#define HOffset(f,n)		((HeaderPt)(cPt(f) + n))

typedef struct Header
{
	Word size ;
	struct Header *next ;
} Header, *HeaderPt ;

static Size codeAreaSize = 0 ;
static Pt memA = nil, memZ = nil ;
static HeaderPt freeList = nil ;

static void MoreMemory(Size atLeastWords)
{
	Size words ;
	Pt mem ;
	for( words = Min(Max(codeAreaSize, initialBlockSize), maxBlockSize)
							; words < atLeastWords ; words *= 2 ) ;
	if( codeAreaSize > 0 )  /* first grow not shown */
		MemoryGrowWarning("static area", codeAreaSize,
										codeAreaSize + words, nil) ;
	if( memZ - memA > blockSlack ) {
		HeaderPt q = (HeaderPt)memA ;
		q->size = memZ - memA ;
		Release(HOffset(q, 1)) ;
	}
	mem = PrimitiveAllocate(words) ;
	codeAreaSize += words ;
	memA = mem ;
	memZ = mem + words ;
}

VoidPt Allocate(Size nWords)
{
	if( nWords <= 0 )
		InternalError("Alloc: nWords<=0") ;
	if( memZ - memA < nWords ) {
		/*AtomsGC() ;*/
		MoreMemory(nWords) ;
	}
	return memZ -= nWords ;
}

VoidPt TempAllocate(Size nWords)
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

VoidPt TempAllocateAndClear(Size nWords)
{
	Hdl mem = TempAllocate(nWords) ;
	ClearWords(mem, nWords) ;
	return mem ;
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

VoidPt Relocate(VoidPt ptr, Size newSize)
{
	VoidPt newPtr ;
	Size copySize = TempBlockSize(ptr) ;
	if( copySize >= newSize ) InternalError("Relocate") ;
	newPtr = TempAllocate(newSize) ;
	CopyWords(cHdl(newPtr), cHdl(ptr), copySize) ;
	Release(ptr) ;
	return newPtr ;
}

Size TempBlockSize(VoidPt ptr)
{
	return (HOffset(ptr, -1))->size - 1 ;
}

Size CodeAreaSize()
{
	return codeAreaSize ;
}

Size CodeAreaUsed()
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
	return codeAreaSize - size - (memZ - memA) ;
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
