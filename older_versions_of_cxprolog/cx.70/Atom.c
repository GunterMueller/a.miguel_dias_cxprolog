/*
 *   This file is part of the CxProlog system

 *   Atom.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* ATOMS */

Pt tNilAtom, tEofAtom, tCutAtom, tTrueAtom, tFailAtom, tBracketsAtom ;

#define atomTableSize 317

static AtomPt *atomTable ;

void InitAtoms(void)
{
	register int i ;

	atomTable = SpaceAlloc(atomTableSize, true) ;
	dotimes(i, atomTableSize)
		atomTable[i] = nil ;

	tNilAtom = MakeAtom("[]") ;
	tEofAtom = MakeAtom("end_of_file") ;
	tCutAtom = MakeAtom("!") ;
	tTrueAtom = MakeAtom("true") ;
	tFailAtom = MakeAtom("fail") ;
	tBracketsAtom = MakeAtom("{}") ;
}

AtomPt LookupAtom(CharPt name)
{
	register UCharPt u = cUCharPt(name) ;
	register int slot ;
	register AtomPt at ;

/* hash function */
	slot = 0 ;
	while( *u ) slot += *u++ ;
	slot = slot % atomTableSize ;
/* find atom */
	dolist(at, atomTable[slot], at->nextHash)
		if( EqualStr(AtomName(at), name) )
			return at ;	
/* create new atom */
	at = SpaceAlloc(WordsOf(Atom) + Words(strlen(name) + 1), true) ;
	at->nextHash = atomTable[slot] ;
	atomTable[slot] = at ;
	AtomFunctors(at) = nil ;
	strcpy(AtomName(at), name) ;
	return at ;
}

void ForEachAtom(AtomProc p)
{
	int i ;
	AtomPt at ;
	
	dotimes(i, atomTableSize)
		dolist(at, atomTable[i], at->nextHash)
			p(at) ;	
}

static void ListAtom(AtomPt at)
{
	WriteStd("%s\n", AtomName(at)) ;
}

void ListAtoms()
{
	ForEachAtom(ListAtom) ;
}



/* FUNCTORS */

FunctorPt commaFunctor, semicolonFunctor, neckFunctor,
		listFunctor, cutFunctor, eqFunctor, barFunctor,
		varFunctor, primitiveFunctor, unitParamFunctor ;

static FunctorPt NewFunctor(AtomPt atom, int arity)
{
	FunctorPt f = SpaceAlloc(WordsOf(Functor), true) ;
	
	FunctorAtom(f) = atom ;
	FunctorArity(f) = arity ;
	FunctorPreds(f) = nil ;
	FunctorIsMeta(f) = false ;
	return f ;
}

FunctorPt LookupFunctor(AtomPt atom, int arity)
{
	register FunctorPt pt ;
	
	dolist(pt, AtomFunctors(atom), pt->nextArity)
		if( FunctorArity(pt) == arity ) return pt ;	
	pt = NewFunctor(atom, arity) ;
	pt->nextArity = AtomFunctors(atom) ;
	return AtomFunctors(atom) = pt ;
}


FunctorPt LookupFunctorByName(CharPt name, int arity)
{
	return LookupFunctor(LookupAtom(name), arity) ;
}

CharPt FunctorNameArity(FunctorPt f)
{
	sprintf(strBuffer+500, "%s/%d", FunctorName(f), FunctorArity(f)) ;
	return strBuffer+500 ;
}

void InitFunctors()
{
	commaFunctor = LookupFunctorByName(",", 2) ;
	semicolonFunctor = LookupFunctorByName(";", 2) ;
	neckFunctor = LookupFunctorByName(":-", 2) ;
	listFunctor = LookupFunctorByName(".", 2) ;
	cutFunctor = LookupFunctorByName("cut", 1) ;
	eqFunctor = LookupFunctorByName("=", 2) ;
	barFunctor = LookupFunctorByName("|", 2) ;
	varFunctor = LookupFunctorByName("$VAR", 1) ;
	primitiveFunctor = LookupFunctorByName("$$_primitive", 1) ;
	unitParamFunctor = LookupFunctorByName("$$_unit_parameter", 1) ;

}
