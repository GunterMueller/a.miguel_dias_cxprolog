/*
 *   This file is part of the CxProlog system

 *   GCollection.c
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL

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

void GCollectionInit()
{
}
