/*
 *   This file is part of the CxProlog system

 *   Buffer.c
 *   by A.Miguel Dias - 2001/02/22
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
#define __USE_ISOC99	1
#include <wchar.h>

static ExtraTypePt bufferType ;


/* PRIVATE FUNCTIONS */

static Size BufferCapacity(BufferPt b)
{
	return BufferEnd(b) - BufferBegin(b) ;
}

static void BufferInit(BufferPt b, Size byteCapacity,
						Size positionOffset, Size lastOffset)
{
	BufferBegin(b) = TempAllocate(Words(byteCapacity)) ;
	BufferEnd(b) = BufferBegin(b) + byteCapacity ;
	BufferPosition(b) = BufferBegin(b) + positionOffset ;
	BufferLast(b) = BufferBegin(b) + lastOffset ;
}

static void BufferExpand(BufferPt b, PInt minIdx)
{
	PInt cap ;
	CharPt bb = BufferBegin(b) ;
	CharPt be = BufferEnd(b) ;
	Size positionOffset = BufferPosition(b) - bb ;
	Size lastOffset = BufferLast(b) - bb ;
	register CharPt s, h ;

	for( cap = 2 * BufferCapacity(b) ; cap <= minIdx ; cap *= 2 ) ;
	BufferInit(b, cap, positionOffset, lastOffset) ;
	for( h = BufferBegin(b), s = bb ; s < be ; *h++ = *s++ ) ;
	Release(bb) ;
}

static void BufferWrite(StreamPt srm, BufferPt b)
{ ;
	register CharPt pt ;
	StreamWrite(srm, "%s", XExtraAsStr(TagExtra(b))) ;
	StreamWrite(srm, "     (current capacity %ld, pos=%ld, last=%ld)\n",
					BufferCapacity(b),
					BufferPosition(b)-BufferBegin(b),
					BufferLast(b)-BufferBegin(b)) ;
	for( pt = BufferBegin(b) ; pt < BufferLast(b) ; pt++ )
		StreamPut(srm, cx_isprint(*pt) ? *pt : '?') ;
	StreamPut(srm, '\n') ;
}


/* MAIN OPERATIONS */

Size BufferSize(BufferPt b)
{
	return BufferLast(b) - BufferBegin(b) ;
}

CharPt BufferContents(BufferPt b)
{
	return BufferBegin(b) ;
}

BufferPt BufferNew(void)
{
	BufferPt b = ExtraNew(bufferType) ;
	BufferInit(b, 4, 0, 0) ;
	return b ;
}

void BufferClear(BufferPt b)
{
	BufferLast(b) = BufferBegin(b) ;
	BufferPosition(b) = BufferBegin(b) ;
}

void BufferDelete(BufferPt b)
{
	Release(BufferBegin(b)) ;
	ExtraDelete(bufferType, b) ;
}

int BufferGet(BufferPt b, PInt idx)
{
	if( BufferBegin(b) + idx < BufferLast(b) )
		return BufferBegin(b)[idx] ;
	else return 0 ;
}

void BufferSet(BufferPt b, PInt idx, int i)
{
	if( BufferBegin(b) + idx >= BufferEnd(b) )
		BufferExpand(b, idx) ;
	if( BufferBegin(b) + idx >= BufferLast(b) ) {
		CharPt idxPos = BufferBegin(b) + idx ;
		register CharPt pt ;
		for( pt = BufferLast(b) ; pt < idxPos ; pt++ )
			*pt = 0 ;
		BufferLast(b) = idxPos + 1 ;
	}
	BufferBegin(b)[idx] = i ;
}

void BufferResize(BufferPt b, Size newSize)
{
	if( newSize == 0 )
		BufferClear(b) ;
	else {
		int i = BufferGet(b, newSize-1) ;
		BufferSet(b, newSize-1, i) ; /* force grow if necessary */
		BufferLast(b) = BufferBegin(b) + newSize ;
		if( BufferPosition(b) > BufferLast(b) )
			BufferPosition(b) = BufferLast(b) ;
	}
}


/* SEQUENTIAL OPERATIONS */

void BufferEnsureSpace(BufferPt b, Size freeSpace)
{
	if( BufferPosition(b) + freeSpace >= BufferEnd(b) )
		BufferExpand(b, BufferPosition(b) + freeSpace - BufferBegin(b)) ;
}

void BufferSetSizeUnsafe(BufferPt b, Size size)
{
	BufferLast(b) = BufferBegin(b) + size ;
}

void BufferReset(BufferPt b)
{
	BufferPosition(b) = BufferBegin(b) ;
}

void BufferRewrite(BufferPt b)
{
	BufferClear(b) ;
}

void BufferAppend(BufferPt b)
{
	BufferPosition(b) = BufferLast(b) ;
}


/* SEQUENTIAL READ OPERATIONS */

Bool BufferAtEnd(BufferPt b)
{
	return BufferPosition(b) >= BufferLast(b) ;
}

int BufferAccessByte(BufferPt b, Bool advance)
{
	if( BufferPosition(b) >= BufferLast(b) ) return eofMark ;
	if( advance ) return *BufferPosition(b)++ ;
	else return *BufferPosition(b) ;
}

int BufferAccessChar(BufferPt b, Bool advance)
{
	wchar_t c ;
	int len ;
	if( BufferPosition(b) >= BufferLast(b) ) return eofMark ;
	len = mbrtowc(&c, BufferPosition(b), BufferLast(b)-BufferPosition(b), nil) ;
	if( len <= 0 ) {
		switch( len ) {
			case 0: {
				len++ ;
				break ;
			}
			case -1: {
				FileError("Wide character conversion error") ;
				return -1 ;
			}
			case -2: {
				FileError("Incomplete multibyte char - premature end of stream") ;
				return -1 ;
			}
			default: /* nothing */ ;
		}
	}		
	if( advance ) BufferPosition(b) += len ;
	return c ;
}


/* SEQUENTIAL WRITE OPERATIONS */

void BufferPutByte(BufferPt b, int c)
{
	if( BufferPosition(b) == BufferEnd(b) )
		BufferExpand(b, 0) ;
	*BufferPosition(b)++ = c ;
	if( BufferPosition(b) > BufferLast(b) )
		BufferLast(b) = BufferPosition(b) ;
}

void BufferPutChar(BufferPt b, int c)
{
	int len ;
	BufferEnsureSpace(b, 6) ;
	len = wcrtomb(BufferPosition(b), c, nil) ;
	if( len == -1 )
		FileError("This wide char cannot be represented as a multibyte sequence") ;
	else BufferPosition(b) += len ;
	if( BufferPosition(b) > BufferLast(b) )
		BufferLast(b) = BufferPosition(b) ;
}


/* CXPROLOG C'BUILTINS */

static void PBufferCheck()
{
	MustBe( XExtraCheck(bufferType, X0) )
}

static void PBufferNew()
{
	BindVarWithExtra(X0, BufferNew()) ;
	JumpNext()
}

static void PBufferClear()
{
	BufferPt b = XTestExtra(bufferType,X0) ;
	BufferClear(b) ;
	JumpNext()
}

static void PBufferDelete()
{
	BufferPt b = XTestExtra(bufferType,X0) ;
	BufferDelete(b) ;
	JumpNext()
}

static void PBufferSize()
{
	BufferPt b = XTestExtra(bufferType,X0) ;
	Size size = BufferSize(b) ;
	Size newSize ;
	Ensure( UnifyWithAtomic(X1, MakeInt(size)) )
	newSize = XTestNat(X2) ;
	if( newSize != size )
		BufferResize(b, newSize) ;
	JumpNext()	
}

static void PBufferSet()
{
	BufferPt b = XTestExtra(bufferType,X0) ;
	BufferSet(b, XTestPosInt(X1)-1, XTestByte(X2)) ;
	JumpNext()
}

static void PBufferGet()
{
	BufferPt b = XTestExtra(bufferType,X0) ;
	MustBe( Unify(X2, MakeByte(BufferGet(b, XTestPosInt(X1)-1))) )
}

static void PBufferWrite()
{
	BufferPt b = XTestExtra(bufferType,X0) ;
	BufferWrite(currOut, b) ;
	JumpNext()
}

static void PSBufferWrite()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	BufferPt b = XTestExtra(bufferType,X1) ;
	BufferWrite(srm, b) ;
	JumpNext()
}

static void PNDCurrentBuffer()
{
	PNDCurrentExtra(bufferType, nil, 1) ;
	JumpNext()
}

static Size BuffersAux(VoidPt x)
{
	BufferPt b = cBufferPt(x) ;
	AtomPt at = IVarWith(TagExtra(b)) ;
	Write("  %16s -> size = %ld, capacity = %ld", 
				TermAsStr(TagExtra(b)),
				BufferSize(b),
				BufferCapacity(b)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
	return 0 ;
}
static void PBuffers()
{
	ShowVersion() ;
	Write("Buffers:\n") ;
	ForEachExtra(bufferType, BuffersAux) ;
	JumpNext()
}


/* TEST, EXTRACT & INIT */

Bool IsBuffer(Pt t)
{
	return IsThisExtra(bufferType, t) ;
}

BufferPt XTestBuffer(Pt t)
{
	return XTestExtra(bufferType, t) ;
}

void BuffersInit()
{
	bufferType = ExtraTypeNew("BUFFER", WordsOf(Buffer)) ;

	InstallCBuiltinPred("buffer", 1, PBufferCheck) ;
	InstallCBuiltinPred("buffer_new", 1, PBufferNew) ;
	InstallCBuiltinPred("buffer_clear", 1, PBufferClear) ;
	InstallCBuiltinPred("buffer_delete", 1, PBufferDelete) ;
	InstallCBuiltinPred("buffer_size", 3, PBufferSize) ;
	InstallCBuiltinPred("buffer_set", 3, PBufferSet) ;
	InstallCBuiltinPred("buffer_get", 3, PBufferGet) ;
	InstallCBuiltinPred("buffer_write", 1, PBufferWrite) ;
	InstallCBuiltinPred("buffer_write", 2, PSBufferWrite) ;
	InstallNDeterCBuiltinPred("current_buffer", 1, PNDCurrentBuffer) ;
	InstallNDeterCBuiltinPred("buffers", 0, PBuffers) ;
}
