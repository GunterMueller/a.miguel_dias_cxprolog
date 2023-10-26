/*
 *   This file is part of the CxProlog system

 *   Unit.c
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

static HashTablePt unitHashTable ;
UnitPt builtinUnit, bottomUnit ;

static UnitPt NewUnit(FunctorPt f)
{
	int arity = FunctorArity(f) ;
	UnitPt u = PermanentAllocate(WordsOf(Unit) + arity) ;
	AtomPt undefParamAtom ;

	UnitFunctor(u) = f ;
	UnitIsDefined(u) = false ;
	UnitIsReserved(u) = false ;
	UnitPredicates(u) = nil ;
	undefParamAtom = LookupAtom("?") ;
	while( arity-- )
		UnitParams(u)[arity] = undefParamAtom ;
	u->nextHash = HashTableAdd(unitHashTable, u) ; /* Must be at end */
	return u ;
}

static VoidPt getNextFun(VoidPt pt)
{
	return cUnitPt(pt)->nextHash ;
}

static AtomPt getAtomFun(VoidPt pt)
{
	return UnitAtom(cUnitPt(pt)) ;
}

static int filterFun(VoidPt pt)
{
	return UnitIsReserved(cUnitPt(pt)) ? 2 : 1 ;
}

void InitUnits()
{
	unitHashTable = NewHashTable(32, getNextFun, getAtomFun, filterFun) ;

	builtinUnit = NewUnit(LookupFunctorByName("builtin", 0)) ;
	bottomUnit = builtinUnit ;
}

UnitPt FindUnit(AtomPt atom)
{
	return HashTableFind(unitHashTable, atom) ;
}

UnitPt LookupUnit(FunctorPt f)
{
	UnitPt u = FindUnit(FunctorAtom(f)) ;

	if( u == nil )
		u = NewUnit(f) ;
	elif( UnitArity(u) != FunctorArity(f) )
		Error("Reference unit '%s' with bad arity", AtomName(FunctorAtom(f))) ;
	return u ;
}

UnitPt FirstUnit()
{
	return HashTableFirst(unitHashTable) ;
}

UnitPt NextUnit(UnitPt u)
{
	return HashTableNext(unitHashTable, u) ;
}

Pt LookupCurrUnitParameter(AtomPt parName)
{
	register UnitPt u = CurrUnit() ;
	register int n = UnitArity(u) ;
	while( n-- )
		if( UnitParam(u, n) == parName )
			return MakeUnStruct(unitParamFunctor, MakeInt(n + 1)) ;
	return nil ;
}

static Bool Duplicates(AtomPt *parNames, int nPars)
{
	register int i, j ;

	dotimes(i, nPars)
		for( j = i + 1 ; j < nPars ; j++ )
			if( parNames[i] == parNames[j] )
				return true ;
	return false ;
}

void DefineUnit(AtomPt atom, Hdl parNames, int nPars)
{
	UnitPt u = FindUnit(atom) ;
	int i ;

	if( u == nil )
		u = LookupUnit(LookupFunctor(atom, nPars)) ;
	elif( UnitArity(u) != nPars )
		Error("Unit '%s' was previousy %s with different arity",
					AtomName(atom),
					UnitIsDefined(u) ? "defined" : "used") ;
	if( not UnitIsDefined(u) ) {
		dotimes(i, nPars) {
			UnitParams(u)[i] = XTestAtom(parNames[i]) ;
			if( not IsVarName(AtomName(UnitParams(u)[i])) )
				Error("Unit argument names must be variable names") ;
		}
		if( Duplicates(UnitParams(u), nPars) )
			Error("All unit argument names must be distinct") ;
		UnitIsDefined(u) = true ;
	}
}

CharPt UnitSignature(UnitPt u)
{
	int i ;

	strcpy(strBuffer, UnitName(u)) ;
	if( UnitArity(u) > 0 ) {
		strcat(strBuffer, "(") ;
		dotimes(i, UnitArity(u)) {
			strcat(strBuffer, XAtomName(UnitParam(u,i))) ;
			if( i != UnitArity(u) - 1 ) strcat(strBuffer, ", ") ;
		}
		strcat(strBuffer, ")") ;
	}
	return strBuffer ;
}

UnitPt TermToUnit(register Pt term)
{
	UnitPt u ;

	term = Drf(term) ;
	if( IsVar(term) )
		Error("Unit name is not bound") ;
	elif( IsAtom(term) ) {
		if( ( u = FindUnit(XAtom(term)) ) == nil )
			Error("Unit '%s/0' is undefined", XAtomName(term)) ;
		else LookupUnit(LookupFunctor(XAtom(term), 0)) ;
	}
	elif( IsStruct(term) ) {
		if( ( u = FindUnit(XStructAtom(term)) ) == nil )
			Error("Unit '%s' is undefined", FunctorNameArity(XStructFunctor(term))) ;
		else LookupUnit(XStructFunctor(term)) ;
	}
	elif( IsList(term) ) Error("Unit name is a list") ;
	elif( IsNumber(term) ) Error("Unit name is a number") ;
	elif( IsExtra(term) ) Error("Unit name is an extra term") ;
	else InternalError("TermToUnit") ;

	return u ;
}

void ListUnits(void)
{
	UnitPt u ;

	Write("\tUnits:\n") ;
	dolist(u, FirstUnit(), NextUnit(u))
		Write("\t\t%s\n", UnitSignature(u) ) ;
}

void ListUnitPredicates(UnitPt u)
{
	register PredicatePt pr ;

	dolist(pr, UnitPredicates(u), PredNextU(pr))
		WriteStd("%s\n", PredNameArity(pr) ) ;
}

void ListUndefPredicates()
{
	register UnitPt u ;
	register PredicatePt pr ;
	
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredIsUndefined(pr) )
			WriteStd("%s\n", PredNameArity(pr) ) ;
	dolist(u, FirstUnit(), NextUnit(u))
		dolist(pr, UnitPredicates(u), PredNextU(pr))
			if( PredIsUndefined(pr) )
				WriteStd("%s\n", PredNameArity(pr) ) ;
}