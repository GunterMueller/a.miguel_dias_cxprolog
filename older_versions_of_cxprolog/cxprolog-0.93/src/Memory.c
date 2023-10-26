/*
 *   This file is part of the CxProlog system

 *   Memory.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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
#if __APPLE__
#include <malloc/malloc.h>
#else
#include <malloc.h>
#endif

#define USE_MALLOC  0

#if !USE_MALLOC

#if M_MMAP_THRESHOLD /* check if mmap supported */
#include <sys/mman.h>
#endif

/* PRIMITIVE ALLOCATE */

VoidPt PrimitiveAllocate(Size nWords) /* Requests block of memory to the OS */
{
	Size nBytes = WordsAsBytes(nWords + 2) ;
	CharPt mem ;
#if M_MMAP_THRESHOLD
	/* If mmap is available, use it directly to avoids the inconsistent results
		produced by malloc: sometimes negative addresses, sometimes not.  */
	if( (mem = mmap(nil, nBytes, PROT_READ|PROT_WRITE,
					MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)) == MAP_FAILED )
		Error("No more computer memory available") ;
	cPt(mem)[0] = nBytes ;
	mem += sizeof(Word) ;
#else
	if( (mem = malloc(nBytes)) == nil )
		Error("No more computer memory available") ;
	while( cWord(mem) % sizeof(Word) != 0 ) mem++ ; /* align mem */
#endif /* M_MMAP_THRESHOLD */

#if 0
	Mesg("%7ldKB - 0x%lx", nWords / 1024 * 4, cWord(mem)) ;
/*	system("free") ;*/
#endif	
	if( GetTag(mem) != tagVar )
		FatalError("Invalid memory allocation on %s machine",
						tagVar == 0 ? "FORWARDS" : "BACKWARDS") ;
	return mem ;
}

VoidPt PrimitiveReallocate(VoidPt mem, Size copySize, Size newSize)
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
#ifdef M_MMAP_THRESHOLD
	if( munmap(cCharPt(mem) - sizeof(Word), cPt(mem)[-1]) == -1 )
		FatalError("Malfunction: could not free memory") ;
#else
	free(mem) ;
#endif /* M_MMAP_THRESHOLD */
}

Bool UsingMMap()
{
#if M_MMAP_THRESHOLD
	return true ;
#else
	return false ;
#endif
}


/* CODE AREA ALLOCATE */

#define initialBlockCapacity	32 K /* words */
#define maxBlockCapacity		128 K /* words */
#define blockSlack				2
#define HOffset(f,n)			((HeaderPt)(cPt(f) + n))

typedef struct Header
{
	Size size ;
	struct Header *next ;
} Header, *HeaderPt ;

static Size codeAreaSize = 0 ;
static Pt memA = nil, memZ = nil ;
static HeaderPt freeList = nil ;

static void MoreMemory(Size atLeastWords)
{
	Size words ;
	Pt mem ;
	for( words = Min(Max(codeAreaSize, initialBlockCapacity), maxBlockCapacity)
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

VoidPt PermAllocate(Size nWords)
{
	if( nWords <= 0 )
		InternalError("Allocate: nWords<=0") ;
	if( memZ - memA < nWords ) {
		/*BasicGC() ;*/
		MoreMemory(nWords) ;
	}
	return memZ -= nWords ;
}

VoidPt TempAllocate(Size nWords)
{
	register HeaderPt *p, q = nil ;

	if( nWords <= 0 )
		InternalError("TempAllocate: nWords<=0") ;
	
	nWords++ ;	/* Extra word to register the block size in each block */

	for( p = &freeList ; *p != nil && (**p).size < nWords ; p = &((**p).next) ) ;
	if( *p == nil ) {
		if( memZ - memA < nWords ) {
			/*if( q == nil )
				{ BasicGC() ; q = freeList ; goto redo ; }
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

VoidPt Reallocate(VoidPt ptr, Size newSize)
{
	VoidPt newPtr ;
	Size copySize = TempBlockSize(ptr) ;
	if( copySize >= newSize ) InternalError("Rellocate") ;
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

#else // !USE_MALLOC

static Size codeAreaSize = 0 ;

VoidPt PrimitiveAllocate(Size nWords) /* Requests block of memory to the OS */
{
	CharPt mem ;
	if( (mem = malloc(WordsAsBytes(nWords))) == nil )
		Error("No more computer memory available") ;
	codeAreaSize += nWords ;
	return mem ;
}

void PrimitiveRelease(VoidPt mem)
{
	free(mem) ;
}

Bool UsingMMap()
{
	return false ;
}

VoidPt PermAllocate(Size nWords)
{
	return PrimitiveAllocate(nWords) ;
}

VoidPt TempAllocate(Size nWords)
{
	return PrimitiveAllocate(nWords) ;
}

void Release(VoidPt mem)
{
	free(mem) ;
}

VoidPt Reallocate(VoidPt ptr, Size newSize)
{
	CharPt mem ;
	if( (mem = realloc(ptr, WordsAsBytes(newSize))) == nil )
		Error("No more computer memory available") ;
	return mem ;
}

Size TempBlockSize(VoidPt ptr)
{
	return Words(malloc_usable_size(ptr)) ;
}

Size CodeAreaSize()
{
	return codeAreaSize ;
}

Size CodeAreaUsed()
{
	return codeAreaSize ;
}

#endif // !USE_MALLOC

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
