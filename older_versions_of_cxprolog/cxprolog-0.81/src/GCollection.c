/*
 *   This file is part of the CxProlog system

 *   GCollection.c
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define deltaThreshold		1024

static Size gcDelta ;

void AtomGCMark(AtomPt at)
{
	AtomMarked(at) = true ;
}

static Size AtomGCUnmark(AtomPt at, Pt t)
{
	AtomMarked(at) = false ;
	return 0 ;
}
static void AtomsGCUnmark()
{
	ForEachAtom(AtomGCUnmark, tNilAtom) ;
}

void AtomsGCAddDelta(Size size)
{
	if( (gcDelta += size) >= deltaThreshold ) {
		Attention() = true ;
		gcDelta = deltaThreshold ;
	}
}

void AtomsGC()
{
	if( gCollection_flag && gcDelta >= deltaThreshold ) {
		AtomsGCUnmark() ;
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
