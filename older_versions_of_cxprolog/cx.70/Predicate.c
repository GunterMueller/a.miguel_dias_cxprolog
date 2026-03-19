/*
 *   This file is part of the CxProlog system

 *   Predicate.c
 *   by A.Miguel Dias - 1989/11/14
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

static Bool forceVisibility = false ;

static PredicatePt NewPredicate(FunctorPt f)
{
	register PredicatePt pr ;

	if( UnitIsReserved(CurrUnit()) )
		if( C == tNilAtom )
			Error("No current unit when attempting to create new predicate '%s'",
													FunctorNameArity(f)) ;
		else
			Error("Cannot create the new predicate '%s' in the builtin unit",
													FunctorNameArity(f)) ;

	pr = SpaceAlloc(WordsOf(Predicate), true) ;
	PredFirstClause(pr) = nil ;

	PredFunctor(pr) = f ;
	PredNextF(pr) = FunctorPreds(f) ;
	FunctorPreds(f) = pr ;

	PredUnit(pr) = CurrUnit() ;
	PredNextU(pr) = UnitPredicates(CurrUnit()) ;
	UnitPredicates(CurrUnit()) = pr ;

	PredIsNotIndexed(pr) = PredArity(pr) == 0 ;
	PredHasIndex(pr) = false ;
	PredIsBuiltin(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsImported(pr) = false ;

	InvalidIndex(pr) ;
	return pr ;
}

void BindPredicate(PredicatePt pr, Pt inst, Pt arg0)
{
	PredStartInst(pr) = inst ;
	PredStartInstArgs(pr)[0] = arg0 ;
}

void BindPredicateMoreArgs(PredicatePt pr, Pt arg1, Pt arg2)
{
	PredStartInstArgs(pr)[1] = arg1 ;
	PredStartInstArgs(pr)[2] = arg2 ;
}

PredicatePt FindPredicate(FunctorPt f)
{
	register PredicatePt pr ;
	register UnitPt u = CurrUnit() ;

	dolist(pr, FunctorPreds(f), PredNextF(pr))
		if( PredUnit(pr) == builtinUnit || PredUnit(pr) == u )
			return pr ;
	return nil ;
}

PredicatePt FindPredicateByName(CharPt n, int a)
{
	return FindPredicate(LookupFunctor(LookupAtom(n), a)) ;
}

void MakeVisible(PredicatePt pr, Bool err)
{
	if( PredName(pr)[0] == '$' ) {
		if( err )
			Error("A predicate which name starts with a \'$\' cannot be made visible: '%s'",
					PredNameArity(pr)) ;
	}
	else PredIsVisible(pr) = true ;
}

Bool CanBeVisible(FunctorPt f)
{
	return FunctorName(f)[0] != '$' ;
}

PredicatePt FindVisiblePredicate(FunctorPt f) /* pre: CanBeVisible(f) == true */
{
	register PredicatePt pr ;
	register UnitPt u = CurrUnit() ;

	dolist(pr, FunctorPreds(f), PredNextF(pr))
		if( PredUnit(pr) == builtinUnit || PredUnit(pr) == u )
			if( PredIsVisible(pr) || forceVisibility )
				return pr ;
	return nil ;
}

PredicatePt LookupPredicate(FunctorPt f, Bool change)
{
	register PredicatePt pr ;

	if( (pr = FindPredicate(f)) != nil ) {
		if( change && PredIsBuiltin(pr) )
			Error("Attempt to change builtin predicate '%s'", PredNameArity(pr)) ;
		return pr ;
	}
	return NewPredicate(f) ;
}

CharPt PredNameArity(PredicatePt pr)
{
	sprintf(strBuffer, "%s:%s/%d",
		UnitName(PredUnit(pr)), PredName(pr), PredArity(pr)) ;
	return strBuffer ;
}

void DeletePredicate(PredicatePt pr)
{
	ClausePt cl, next ;
	
	if( PredIsBuiltin(pr) )
		Error("Builtin predicate '%s' cannot be deleted", PredNameArity(pr)) ;

	dolist(cl, PredFirstClause(pr), next) {
		next = ClauseNext(cl) ;
		DeleteClause(cl) ;
	}
	PredIsNotIndexed(pr) = PredArity(pr) == 0 ;
	PredHasIndex(pr) = false ;	
	PredIsBuiltin(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsImported(pr) = false ;
	InvalidIndex(pr) ;
}

void EnterUserMode()
{
	register PredicatePt pr ;
	register OperatorPt op ;
	register ImperativeVarPt iv ;

	if( UnitIsReserved(builtinUnit) )
		FatalError("EnterUserMode() cannot be called twice") ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr)) {
		PredIsBuiltin(pr) = not PredIsUndefined(pr) ;
		MakeVisible(pr, false) ;
	}
	for( op = FirstOperator() ; op != nil ; op = NextOperator(op) )
		OperatorIsBuiltin(op) = true ;
	for( iv = FirstImperativeVar() ; iv != nil ; iv = NextImperativeVar(iv) )
		IVarIsBuiltin(iv) = true ;	
	UnitIsReserved(builtinUnit) = true ;
}

void SetForceVisibilityFlag(Bool b)
{
	forceVisibility = b ;
}

Bool GetForceVisibilityFlag()
{
	return forceVisibility ;
}


/* IMPORTED PREDICATES */

void ImportPredicate(PredicatePt pr, Pt term)
{
	if( PredIsImported(pr) ) {
		if( XTestFunctor(GetImportTerm(pr)) != XTestFunctor(term) )
			Error("Predicate '%s' was already imported from another unit",
							PredNameArity(pr)) ;
	}
	elif( not PredIsUndefined(pr) )
		Error("Predicate '%s' cannot be imported because it was already defined in this unit",
					PredNameArity(pr)) ;

	BindPredicate(pr, SwitchContextAndCall, CopyTermToSpace(term)) ;
	BindPredicateMoreArgs(pr, cPt(PredFunctor(pr)), nil) ;
	PredIsImported(pr) = true ;
}

void CheckImports()
{
	register UnitPt u, u2 ;
	register PredicatePt pr, pr2 ;
	FunctorPt f2 ;
	Bool found ;
	
	dolist(u, FirstUnit(), NextUnit(u))
		dolist(pr, UnitPredicates(u), PredNextU(pr))
			if( PredIsImported(pr) ) {
				f2 = XTestFunctor(GetImportTerm(pr)) ;
				u2 = FindUnit(FunctorAtom(f2)) ;
				if( u2 == nil || UnitArity(u2) != FunctorArity(f2) )
					Warning("Predicate '%s' is imported from nonexistent unit '%s'",
											PredNameArity(pr), FunctorNameArity(f2)) ;
				
				found = false ;
				dolist(pr2, FunctorPreds(PredFunctor(pr)), PredNextF(pr2))
					if( PredUnit(pr2) == u2 ) {
						found = true ;
						break ;
					}			
				if( found && not PredIsVisible(pr2) || not found )
					Warning("Imported predicate '%s' is not visible in unit '%s'",
											PredNameArity(pr), FunctorNameArity(f2)) ;
			}
}


/* BUILTIN C PREDICATES */

static PredicatePt cPredicates = nil ;

PredicatePt InstallCBuiltinPred(CharPt name, int arity, Proc cPr)
{
	PredicatePt pr ;
	FunctorPt f = LookupFunctor(LookupAtom(name), arity) ;

	if( CurrUnit() != builtinUnit )
		Error("All C predicates must be installed in the 'builtin' unit") ;
	if( cPredicates != nil )
		Error("Installing C predicates had already stopped") ;
	if( FindPredicate(f) != nil )
		Error("C predicate already installed") ;
	pr = NewPredicate(f) ;
	BindPredicate(pr, cPt(Z(cPr)), Proceed) ;
	return pr ;
}

void FinishInstallingCBuiltinPreds()
{
	cPredicates = UnitPredicates(builtinUnit) ;
}

CharPt CPredNameArity(Pt p)
{
	register PredicatePt pr ;
	
	dolist(pr, cPredicates, PredNextU(pr))
		if( PredStartInst(pr) == p )
			return PredNameArity(pr) ;
	return nil ;
}

void ListCBuiltinPreds()
{
	PredicatePt pr ;
	
	dolist(pr, cPredicates, PredNextU(pr))
		WriteStd("%s\n", PredNameArity(pr)) ;
}

void PatchBuiltinPredicate(CharPt name, int arity)
{
	ClausePt cl ;
	PredicatePt pr = FindPredicateByName(name, arity) ;

	if( pr == nil || PredIsUndefined(pr) )
		FatalError("Missing patched predicate %s in boot file",
												PredNameArity(pr)) ;
	if( (cl = ClauseNext(PredFirstClause(pr))) == nil || ClauseNext(cl) != nil )
		FatalError("Patched predicate %s is incorrect in boot file", PredNameArity(pr)) ;
	ClauseInst(cl) = RetryMeElse ;
	ClauseNext(cl) = cl ;
	PredIsNotIndexed(pr) = true ;
}
