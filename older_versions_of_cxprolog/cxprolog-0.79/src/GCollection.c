/*
 *   This file is part of the CxProlog system

 *   GCollection.c
 *   by A.Miguel Dias - 2001/04/24
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

#define deltaThreshold		1024

static Size gcDelta ;

void AtomGCMark(AtomPt at)
{
	AtomMarked(at) = true ;
}

static void AtomGCUnmark(AtomPt at)
{
	AtomMarked(at) = false ;
}

void AtomsGCAddDelta(Size size)
{
	if( (gcDelta += size) >= deltaThreshold ) {
		if( gCollection_flag )
			AttentionActivate() ;
		gcDelta = deltaThreshold ;
	}
}

void AtomsGC()
{
	if( gCollection_flag && gcDelta >= deltaThreshold ) {
		ForEachAtom(AtomGCUnmark) ;
		ControlStacksAtomGCMark() ;
		IVarsAtomGCMark() ;
		QueuesAtomGCMark() ;
		StacksAtomGCMark() ;
		DictsAtomGCMark() ;
		AtomsClearNotMarked() ;
		gcDelta = 0 ;
	}
}

/* CXPROLOG C'BUILTINS */

static void PAtomsGC()
{
	AtomsGC() ;
	JumpNext()
}

void GCollectionInit()
{
	gcDelta = 0 ;
	InstallCBuiltinPred("atoms_gc", 0, PAtomsGC) ;
}