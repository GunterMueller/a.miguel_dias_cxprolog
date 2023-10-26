/*
 *   This file is part of the CxProlog system

 *   Unit.c
 *   by A.Miguel Dias - 2000/04/02
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

static HashTablePt unitHashTable ;
UnitPt builtinUnit, bottomUnit ;

static UnitPt NewUnit(FunctorPt f)
{
	int arity = FunctorArity(f) ;
	UnitPt u = PermBlockAllocate(WordsOf(Unit) + arity) ;
	AtomPt undefParamAtom ;
	UnitFunctor(u) = f ;
	UnitIsDefined(u) = false ;
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
	return 1 ;
}

static void UnitsTableAndFirstUnitsInit()
{
	unitHashTable = NewHashTable(32, getNextFun, getAtomFun, filterFun) ;
	builtinUnit = NewUnit(LookupFunctorByName("$$_builtin_unit", 0)) ;
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
		DatabaseError("Reference unit '%s' with bad arity",
									AtomName(FunctorAtom(f))) ;
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

/*
Pt LookupCurrUnitParameter(AtomPt parName)
{
	register UnitPt u = CurrUnit() ;
	register int n = UnitArity(u) ;
	while( n-- )
		if( UnitParam(u, n) == parName )
			return MakeUnStruct(unitParamFunctor, MakeInt(n + 1)) ;
	return nil ;
}
*/

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
		DatabaseError("Unit '%s' was previously %s with different arity",
					AtomName(atom),
					UnitIsDefined(u) ? "defined" : "used") ;
	if( not UnitIsDefined(u) ) {
		dotimes(i, nPars) {
			UnitParams(u)[i] = XTestAtom(parNames[i]) ;
			if( AtomName(UnitParams(u)[i])[0] != '%' )
				DatabaseError("Unit parameter names must start with a '%'") ;
		}
		if( Duplicates(UnitParams(u), nPars) )
			DatabaseError("All unit argument names must be distinct") ;
		UnitIsDefined(u) = true ;
	}
}

CharPt UnitSignature(UnitPt u)
{
	int i ;
	strcpy(retBuffer, UnitName(u)) ;
	if( UnitArity(u) > 0 ) {
		strcat(retBuffer, "(") ;
		dotimes(i, UnitArity(u)) {
			strcat(retBuffer, XAtomName(UnitParam(u,i))) ;
			if( i != UnitArity(u) - 1 ) strcat(retBuffer, ", ") ;
		}
		strcat(retBuffer, ")") ;
	}
	return retBuffer ;
}

UnitPt TermToUnit(register Pt term)
{
	UnitPt u ;
	term = Drf(term) ;
	if( IsVar(term) )
		DatabaseError("Unit name is not bound") ;
	elif( IsAtom(term) ) {
		AtomPt atom = XAtom(term) ;
		if( ( u = FindUnit(atom) ) == nil )
			DatabaseError("Unit '%s/0' is undefined", AtomName(atom)) ;
		else LookupUnit(LookupFunctor(atom, 0)) ;
	}
	elif( IsStruct(term) ) {
		FunctorPt f = XStructFunctor(term) ;
		if( ( u = FindUnit(FunctorAtom(f)) ) == nil )
			DatabaseError("Unit '%s' is undefined", FunctorNameArity(f)) ;
		else LookupUnit(f) ;
	}
	elif( IsList(term) ) DatabaseError("Unit name is a LIST") ;
	elif( IsNumber(term) ) DatabaseError("Unit name is a NUMBER") ;
	elif( IsExtra(term) ) DatabaseError("Unit name is an EXTRA") ;
	else InternalError("TermToUnit") ;
	return u ;
}

void ListUnitPredicates(UnitPt u)
{
	register PredicatePt pr ;
	dolist(pr, UnitPredicates(u), PredNextU(pr))
		Write("%s\n", UPredNameArity(pr) ) ;
}

Bool UnitCheck(VoidPt ref)
{
	register UnitPt u ;
	for( u = FirstUnit() ; u != nil ; u = NextUnit(u) )
		if( u == ref ) return true ;
	return false ;
}


/* CXPROLOG C'BUILTINS */

static void PCreateUnit()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) )
		DefineUnit(XAtom(t0), nil, 0) ;
	elif( IsStruct(t0) ) {
		DefineUnit(XStructAtom(t0), XStructArgs(t0), XStructArity(t0)) ;
	}
	else TypeError2("ATOM or STRUCT", t0) ;
	JumpNext()
}

static void PGetContext()
{
	if( Unify(X0, C) ) JumpNext()
	DoFail()
}

static void PGetHContext()
{
	if( Unify(X0, CH) ) JumpNext()
	DoFail()
}

static void PNDCurrentUnit()
{
	UnitPt u =
		A(1) == tNilAtom ? FirstUnit() : NextUnit(cUnitPt(A(1))) ;
	if( u == builtinUnit ) u = NextUnit(u) ;
	A(1) = cPt(u) ;
	if( u == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanStruct(UnitFunctor(u))) ) JumpNext()
	DoFail()
}

static void PUnits(void)
{
	UnitPt u ;
	VersionShow() ;
	Write("Units:\n") ;
	dolist(u, FirstUnit(), NextUnit(u))
		if( u != builtinUnit )
			Write("    %s\n", UnitSignature(u) ) ;
	JumpNext()
}

void UnitsInit()
{
	UnitsTableAndFirstUnitsInit() ;

/* This makes CurrUnit() == bottomUnit.
   Required for installing C builtin predicates. */
	C = tNilAtom ;

	InstallCBuiltinPred("create_unit", 1, PCreateUnit) ;
	InstallCBuiltinPred("context", 1, PGetContext) ;
	InstallCBuiltinPred("hcontext", 1, PGetHContext) ;
	InstallNDeterCBuiltinPred("current_unit", 1, PNDCurrentUnit) ;
	InstallNDeterCBuiltinPred("units", 0, PUnits) ;
}
