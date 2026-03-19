/*
 *   This file is part of the CxProlog system

 *   GCollection.c
 *   by A.Miguel Dias - 2001/04/24
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

/* BasicGCHandler Table */

#define maxGCHandlers  20

static CharPt GCHdlNames[maxGCHandlers] ;
static Fun GCHdl[maxGCHandlers] ;
static int nGCHdl = 0 ;

void InstallBasicGCHandler(CharPt name, Fun p)
{
	if( nGCHdl == maxGCHandlers )
		InternalError("Too many GC handlers: increase the table capacity") ;
	GCHdlNames[nGCHdl] = name ;
	GCHdl[nGCHdl] = p ;
	nGCHdl++ ;;
}


/* ATOMS & EXTRA Garbage Collector */

#define deltaThreshold		(4 K)

static Size gcDelta ;

static void BasicGCUnmarkAll()
{
	AtomForEach(AtomGCUnmark) ;
	ExtraForEach(nil, ExtraUnsetMarked) ;
}

void BasicGCMarkRange(register Hdl a, register Hdl z)
{
	for( ; a < z ; a++ ) {
		if( IsEmpty(*a) )
			/* Skip */ ;
		elif( IsAtom(*a) )
			AtomGCMark(XAtom(*a)) ;
		elif( IsExtra(*a) ) {
#if 0
			Mesg("%lx", *a) ;
			Mesg("     %s", TermAsStr(*a)) ;
#endif
			ExtraSetMarked(XExtra(*a)) ;
		}
	}
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

void BasicGCAddDelta(Size size)
{
	if( (gcDelta += size) >= deltaThreshold ) {
		Attention() = true ;
		gcDelta = deltaThreshold ;
	}
#if 0
	Mesg("gcDelta = %d", gcDelta) ;
#endif
}

static void BasicGCDoIt()
{
	int i ;
	BasicGCUnmarkAll() ;
	dotimes(i, nGCHdl) {
#if 0
		Mesg(GCHdlNames[i]) ;
#endif
		GCHdl[i]() ;
	}		
	AtomGCClearNotMarked() ;
	ExtraGCClearNotMarked(nil) ;
	gcDelta = 0 ;
}

void BasicGC()
{
	if( testGCollection_flag ) {
		Attention() = true ;	/* Force gc again and again */
		BasicGCDoIt() ;	
	}
	elif( gCollection_flag && gcDelta >= deltaThreshold )
		BasicGCDoIt() ;
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

static void PBasicGC()
{
	BasicGC() ;
	JumpNext() ;
}

void GCollectionInit()
{
	gcDelta = 0 ;
	InstallCBuiltinPred("basic_gc", 0, PBasicGC) ;
}
