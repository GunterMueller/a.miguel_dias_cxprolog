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

#define initialBufferSize	   1024 /* words */
#define safeBufferPortionSize	 16 /* must be 4 at least, for Gen4(.) */

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
	return 0 ;
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
