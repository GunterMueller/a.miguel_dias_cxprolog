/*
 *   This file is part of the CxProlog system

 *   Buffer.c
 *   by A.Miguel Dias - 2001/02/22
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

#define initialBufferSize	   1024 /* words */
#define safeBufferPortionSize	 16 /* must be 4 at least, for Gen4(.) */

CharPt bufferBegin, bufferEnd, saveBufferPt ;
Mix bufferMix ;

int BufferExpand()
{
	Size bufferSize = BufferTotalSize() ;
	CharPt newBuffer, newBufferEnd ;	
	MemoryGrowWarning("buffer", bufferSize, bufferSize * 2) ;
	if( BufferIsFree() ) InternalError("BufferExpand") ;
	newBuffer = PrimitiveAllocate(bufferSize * 2 + safeBufferPortionSize) ;
	newBufferEnd = newBuffer + WordsAsBytes(bufferSize * 2) ;
	CopyWords(cHdl(newBuffer), cHdl(bufferBegin),
				Words(bufferChPt - bufferBegin)) ;
	bufferChPt += newBuffer - bufferBegin ;
	saveBufferPt += newBuffer - bufferBegin ;
	PrimitiveRelease(bufferBegin) ;
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
	return cHdl(bufferEnd) - cHdl(bufferBegin) ;
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
	register int c ;
	l = Drf(l) ;
	for( ; IsList(l) ; l = Drf(XListTail(l)) ) {
		t = Drf(XListHead(l)) ;
		if( not IsByte(t) ) Error("Invalid string") ;
		c = XInt(t) ;
		if( c == 10 ) c = '\n' ;
		BufferAddCh(c) ;
	}
	if( l != tNilAtom ) Error("Invalid list") ;
}

void BufferWrite(CharPt fmt, ...)
{
	va_list p ;
	va_start(p, fmt) ;
	while( bufferChPt + 256 >= bufferEnd )
		BufferExpand() ;
	vsprintf(bufferChPt, fmt, p) ;
	bufferChPt += strlen(bufferChPt) ;
}
