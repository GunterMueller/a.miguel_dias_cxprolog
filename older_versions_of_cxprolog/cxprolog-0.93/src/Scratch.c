/*
 *   This file is part of the CxProlog system

 *   Scratch.c
 *   by A.Miguel Dias - 2001/02/22
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

#include "CxProlog.h"

#define scratchInitialCapacity	1 K	/* words */
#define scratchSlackSize		8	/* must be 4 at least, for Gen4(.); and 8 for some reason? */

Hdl scratchBegin, scratchEnd, scratchPt, scratchActive ;
Hdl saveScratchPt, originalScratch ;

/* The ScratchPad gets expanded and reallocated in these functions:
     - Compiler
     - Evaluate
     - Unify
     - Equal
     - RawUnify
     - Compare
     - ListToArray
     - TermBasicGCMark
     - TermSize
     - CopyTerm
     - GetFreeVars
     - ZReadTermStream
     - ZReadTokensStream
*/

int ScratchExpand()
{
	Size scratchCapacity = ScratchCapacity() ;
	Hdl newScratch, newScratchEnd, h ;
	MemoryGrowWarning("scratch pad", scratchCapacity, scratchCapacity * 2, nil) ;
	newScratch = Reallocate(scratchBegin, scratchCapacity * 2) ;
	newScratchEnd = newScratch + scratchCapacity * 2 - scratchSlackSize ;
	scratchPt += newScratch - scratchBegin ;
	saveScratchPt += newScratch - scratchBegin ;
	scratchActive += newScratch - scratchBegin ;
	for( h = scratchActive ; h != newScratch ; h = cHdl(Top(h)) )
		Top(h) += newScratch - scratchBegin ;
	scratchBegin = newScratch ;
	scratchEnd = newScratchEnd ;
	return 0 ;
}

void ScratchMakeRoom(Size nWords)
{
	while( scratchPt + nWords >= scratchEnd ) 
		ScratchExpand() ;
	scratchPt += nWords ;
}

Size ScratchCapacity()
{
	return Df(scratchEnd , scratchBegin) + scratchSlackSize ;
}

Hdl VUseScratch()
{
	UseScratch() ;
	return scratchPt ;
}

Hdl VFreeScratch()
{
	FreeScratch() ;
	return scratchPt + 1 ;
}

void ScratchRestart()
{
	scratchPt = scratchBegin ;
	scratchActive = scratchPt ;
}


/* CXPROLOG C'BUILTINS */

static void PScratch()
{
	Hdl h ;
	Size n ; 
	ShowVersion() ;
	Write("Scratch:\n") ;
	Write("  Scratch capacity = %ld words\n", ScratchCapacity()) ;
	Write("  scratchBegin = %lx\n", scratchBegin) ;
	Write("  scratchPt = %lx\n", scratchPt) ;
	Write("  Segments:") ;
	for( h = scratchActive, n = 0 ; h != scratchBegin ; h = cHdl(Top(scratchActive)) )
		Write(" %lx", h) ;
	Write(" (%ld segment%s in use)\n", n, n == 1 ? "" : "s") ;
	JumpNext() ;
}


void ScratchInit()
{
	scratchBegin = TempAllocate(scratchInitialCapacity) ;
	scratchEnd = scratchBegin + scratchInitialCapacity - scratchSlackSize ;
	ScratchRestart() ;
}

void ScratchInit2()
{
	InstallCBuiltinPred("scratch", 0, PScratch) ;
}
