/*
 *   This file is part of the CxProlog system

 *   Buffer.c
 *   by A.Miguel Dias - 2001/02/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

/* BUFFER */

#define initialBufferSize	   1024 /* words */
#define safeBufferPortionSize	 16 /* must be 4 at least, for Gen4(.) */

typedef struct Buffer {
	ExtraDef(Buffer) ;
	CharPt begin, end  ;
	CharPt last, position  ;
} Buffer, *BufferPt ;

#define cBufferPt(x)			((BufferPt)(x))

#undef BufferBegin
#define BufferBegin(b)			((b)->begin)
#define BufferEnd(b)			((b)->end)
#define BufferLast(b)			((b)->last)
#define BufferPosition(b)		((b)->position)

static ExtraTypePt bufferType ;

static Size BufferSize(BufferPt b)
{
	return BufferEnd(b) - BufferBegin(b) ;
}

static Size BufferCapacity(BufferPt b)
{
	return BufferEnd(b) - BufferBegin(b) ;
}

static void zBufferInit(BufferPt b, Size byteCapacity,
						Size lastOffset, Size positionOffset)
{
	BufferBegin(b) = TempBlockAllocate(Words(byteCapacity)) ;
	BufferEnd(b) = BufferBegin(b) + byteCapacity ;
	BufferLast(b) = BufferBegin(b) + lastOffset ;
	BufferPosition(b) = BufferBegin(b) + positionOffset ;
}

static void zBufferExpand(BufferPt b, PInt minIdx)
{
	PInt cap ;
	CharPt bb = BufferBegin(b) ;
	CharPt be = BufferEnd(b) ;
	CharPt bl = BufferLast(b) ;
	CharPt bp = BufferPosition(b) ;
	register CharPt s, h ;
	for( cap = 2 * BufferCapacity(b) ; cap <= minIdx ; cap *= 2 ) ;
	zBufferInit(b, cap, bl-bb, bp-bb) ;
	for( h = BufferBegin(b), s = bb ; s < be ; *h++ = *s++ ) ;
	BlockRelease(bb) ;
}

static BufferPt BufferNew(void)
{
	BufferPt b = ExtraNew(bufferType) ;
	zBufferInit(b, 256, 0, 0) ;
	return b ;
}

static void BufferClear(BufferPt b)
{
	BufferLast(b) = BufferBegin(b) ;
	BufferPosition(b) = BufferBegin(b) ;
}

static void BufferDelete(BufferPt b)
{
	BlockRelease(BufferBegin(b)) ;
	ExtraDelete(bufferType, b) ;
}

static void zBufferWrite(StreamPt srm, BufferPt b)
{
	register CharPt pt ;
	StreamWrite(srm, "%s", XExtraAsStr(TagExtra(b))) ;
	StreamWrite(srm, "     (current capacity %ld)\n", BufferCapacity(b)) ;
	for( pt = BufferBegin(b) ; pt < BufferEnd(b) ; pt++ )
		StreamPut(srm, *pt) ;
}


/* CXPROLOG C'BUILTINS */

void BuffersInit()
{
	bufferType = ExtraTypeNew("buffer", WordsOf(Buffer)) ;

/*
	InstallCBuiltinPred("array", 1, PArrayCheck) ;
	InstallCBuiltinPred("array_new", 1, PArrayNew) ;
	InstallCBuiltinPred("array_clear", 1, PArrayClear) ;
	InstallCBuiltinPred("array_delete", 1, PArrayDelete) ;
	InstallCBuiltinPred("array_set", 3, PArraySet) ;
	InstallCBuiltinPred("array_get", 3, PArrayGet) ;
	InstallCBuiltinPred("array_delete_item", 2, PArrayDeleteItem) ;
	InstallCBuiltinPred("array_as_list", 2, PArrayAsList) ;
	InstallCBuiltinPred("array_write", 1, PArrayWrite) ;
	InstallCBuiltinPred("array_write", 2, PSArrayWrite) ;
	InstallNDeterCBuiltinPred("current_array", 1, PNDCurrentArray) ;
	InstallNDeterCBuiltinPred("arrays", 0, PArrays) ;
*/
}














/* THE GENERAL PURPOSE BUFFER */

CharPt bufferBegin, bufferEnd, saveBufferPt, originalBuffer ;
Mix bufferMix ;

/* It can happen that theBuffer gets expanded and rellocated in these functions:
     - Compiler
     - Evaluate
     - Unify
     - Equal
     - RawUnify
     - Compare
     - ListToArray
     - TermAtomGCMark
     - TermSize
     - CopyTerm
     - GetFreeVars
     - ZReadTermStream
     - ZReadTokensStream
*/

int BufferExpand()
{
	Size bufferSize = BufferTotalSize() ;
	CharPt newBuffer, newBufferEnd ;	
	MemoryGrowWarning("buffer", bufferSize, bufferSize * 2, nil) ;
	if( BufferIsFree() ) InternalError("BufferExpand") ;
	newBuffer = PrimitiveRellocate(bufferBegin,
						Words(bufferChPt - bufferBegin),
						bufferSize * 2 + safeBufferPortionSize) ;
	newBufferEnd = newBuffer + WordsAsBytes(bufferSize * 2) ;
	bufferChPt += newBuffer - bufferBegin ;
	saveBufferPt += newBuffer - bufferBegin ;
	bufferBegin = newBuffer ;
	bufferEnd = newBufferEnd ;
	return 0 ; /* DummyResult */
}

void BufferMakeRoom(Size nWords)
{
	while( bufferHPt + nWords >= cHdl(bufferEnd) ) 
		BufferExpand() ;
	bufferHPt += nWords ;
}

Size BufferTotalSize()
{
	return Df(bufferEnd, bufferBegin) ;
}

void BufferInit()
{
	bufferBegin = PrimitiveAllocate(initialBufferSize + safeBufferPortionSize) ;
	bufferEnd = bufferBegin + WordsAsBytes(initialBufferSize) ;
	FreeBuffer() ;
}


/* BUFFER TEXT */

void BufferAddNStr(CharPt s, Size len)
{
	while( bufferChPt + len >= bufferEnd )
		BufferExpand() ;
	strncpy(bufferChPt, s, len) ;
	bufferChPt += len ;
}

void BufferAddStr(CharPt s)
{
	while( *s )
		BufferAddCh(*s++) ;
}

void BufferAddQStr(CharPt s, Bool dirty)
{
	if( dirty ) {
		BufferAddCh('\'') ;
		while( *s ) {
			if( *s == '\'' )
				BufferAddCh('\'') ;
			BufferAddCh(*s++) ;
		}
		BufferAddCh('\'') ;
	}
	else BufferAddStr(s) ;
}

void BufferAddPString(register Pt l)
{
	register Pt t ;
	l = Drf(l) ;
	for( ; IsList(l) ; l = Drf(XListTail(l)) ) {
		t = Drf(XListHead(l)) ;
		BufferAddCh(XTestChar(t)) ;
	}
	if( l != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
}

void BufferAddTerm(Pt t)
{
	TermToBuffer(t) ;
}

void BufferWrite(CharPt fmt, ...)
{
	va_list p ;
	while( bufferChPt + 256 >= bufferEnd )
		BufferExpand() ;
	va_start(p, fmt) ;
	vsprintf(bufferChPt, fmt, p) ;
	bufferChPt += strlen(bufferChPt) ;
}
