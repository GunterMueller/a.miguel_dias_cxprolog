/*
 *   This file is part of the CxProlog system

 *   GCollection.c
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* AtomGCHandler Table */

#define maxGCHandlers  20

static VFun GCHdl[maxGCHandlers] ;
static int nGCHdl = 0 ;

void InstallAtomGCHandler(VFun p)
{
	if( nGCHdl == maxGCHandlers )
		InternalError("Too many GC handlers: increase constant") ;
	GCHdl[nGCHdl++] = p ;
}


/* ATOMS */

#define deltaThreshold		2048

static Size gcDelta ;

void AtomGCMark(AtomPt at)
{
	AtomMarked(at) = true ;
}

void AtomGCMarkRange(register Hdl a, register Hdl z)
{
	for( ; a < z ; a++ )
		if( IsAtom(*a) )
			AtomGCMark(XAtom(*a)) ;
}

#if unused
static void AtomGCMarkRangeCheck(register Hdl a, register Hdl z)
{

	for( ; a < z ; a++ )
		if( IsAtom(*a) ) {
			if( AtomCheck(XAtom(*a)) )
				AtomGCMark(XAtom(*a)) ;
			else
				FatalError("Non atom in AtomGCMarkRangeCheck %lx", *a) ;
		}
}
#endif

static Size AtomGCUnmark(AtomPt at)
{
    AtomMarked(at) = false ;
    return 0 ;
}
static void AtomsGCUnmark()
{
    ForEachAtom(AtomGCUnmark) ;
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
	int i ;
	if( testGCollection_flag )
		Attention() = true ;
	if( (gCollection_flag && gcDelta >= deltaThreshold) || testGCollection_flag ) {
		AtomsGCUnmark() ;
		dotimes(i, nGCHdl)
			GCHdl[i]() ;		
		AtomsClearNotMarked() ;
		gcDelta = 0 ;
	}
}


/* UNITS */

#if unused
static void UnitsGCUnmark()
{
	register UnitPt u ;
	doseq(u, unitList, UnitNext(u))
		UnitIsMarked(u) = false ;
}

static void UnitsGCMarkRange(register Hdl a, register Hdl z)
{
/*	for( ; a < z ; a++ )
		if( IsUnit(*a) )
			UnitIsMarked(*a) = false ;*/
}
#endif


/* CXPROLOG C'BUILTINS */

static void PAtomsGC()
{
	AtomsGC() ;
	JumpNext() ;
}

void GCollectionInit()
{
	gcDelta = 0 ;
	InstallCBuiltinPred("atoms_gc", 0, PAtomsGC) ;
}
