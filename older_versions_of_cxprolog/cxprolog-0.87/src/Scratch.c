/*
 *   This file is part of the CxProlog system

 *   Scratch.c
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

#define initialScratchSize	   1024 /* words */
#define safeScratchPortionSize	 16 /* must be 4 at least, for Gen4(.) */

CharPt scratchBegin, scratchEnd, saveScratchPt, originalScratch ;
Mix scratchMix ;

/* The ScratchPad gets expanded and rellocated in these functions:
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

int ScratchExpand()
{
	Size scratchSize = ScratchTotalSize() ;
	CharPt newScratch, newScratchEnd ;	
	MemoryGrowWarning("scratch pad", scratchSize, scratchSize * 2, nil) ;
	if( ScratchIsFree() ) InternalError("ScratchExpand") ;
	newScratch = PrimitiveRellocate(scratchBegin,
						Words(scratchChPt - scratchBegin),
						scratchSize * 2 + safeScratchPortionSize) ;
	newScratchEnd = newScratch + WordsAsBytes(scratchSize * 2) ;
	scratchChPt += newScratch - scratchBegin ;
	saveScratchPt += newScratch - scratchBegin ;
	scratchBegin = newScratch ;
	scratchEnd = newScratchEnd ;
	return 0 ;
}

void ScratchMakeRoom(Size nWords)
{
	while( scratchHPt + nWords >= cHdl(scratchEnd) ) 
		ScratchExpand() ;
	scratchHPt += nWords ;
}

Size ScratchTotalSize()
{
	return Df(scratchEnd, scratchBegin) ;
}

void ScratchInit()
{
	scratchBegin = PrimitiveAllocate(initialScratchSize + safeScratchPortionSize) ;
	scratchEnd = scratchBegin + WordsAsBytes(initialScratchSize) ;
	FreeScratch() ;
}


/* BUFFER TEXT */

void ScratchAddNStr(CharPt s, Size len)
{
	while( scratchChPt + len >= scratchEnd )
		ScratchExpand() ;
	strncpy(scratchChPt, s, len) ;
	scratchChPt += len ;
}

void ScratchAddStr(CharPt s)
{
	while( *s )
		ScratchAddCh(*s++) ;
}

void ScratchAddPString(register Pt l)
{
	register Pt t ;
	l = Drf(l) ;
	for( ; IsList(l) ; l = Drf(XListTail(l)) ) {
		t = Drf(XListHead(l)) ;
		ScratchAddCh(XTestChar(t)) ;
	}
	if( l != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
}

void ScratchAddTerm(Pt t)
{
	TermToScratch(t) ;
}

void ScratchWriteV(CharPt fmt, VoidPt v) /* pre: no more that 1024 bytes written */
{
	while( scratchChPt + 1024 >= scratchEnd )
		ScratchExpand() ;
	vsprintf(scratchChPt, fmt, v) ;
	scratchChPt += strlen(scratchChPt) ;
}

void ScratchWrite(CharPt fmt, ...)
{
	va_list v ;
	va_start(v, fmt) ;
	ScratchWriteV(fmt, v) ;
}
