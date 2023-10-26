/*
 *   This file is part of the CxProlog system

 *   Unit.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Unit_
#define _Unit_

typedef struct Unit
{
	struct Unit *nextHash ;		/* Next unit in the hash chain */
	FunctorPt functor ;			/* Unit name and arity */
	PredicatePt predicates ;	/* Unit predicate list */
	Bool defined ;				/* Tells if the unit was defined or only referenced */
/*	AtomPt params[] ;	*/		/* Unit parameters */
} Unit, *UnitPt ;

#define cUnitPt(p)					((UnitPt)(p))

extern UnitPt builtinUnit, bottomUnit ;

#define UnitFunctor(u)				(u)->functor
#define UnitAtom(u)					FunctorAtom(UnitFunctor(u))
#define UnitArity(u)				FunctorArity(UnitFunctor(u))
#define UnitName(u)					AtomName(UnitAtom(u))
#define UnitPredicates(u)			(u)->predicates
#define UnitParams(u)				((AtomPt *)((u) + 1))
#define UnitParam(u,i)				UnitParams(u)[i]

#define UnitIsDefined(u)			(u)->defined
#define UnitIsAnonymous(u)			(UnitFunctor(u) == emptyFunctor)

#define CurrUnit()		(C == tNilAtom ? bottomUnit : cUnitPt(XPt(C)[-1]))

UnitPt FindUnit(AtomPt atom) ;
UnitPt FirstUnit(void) ;
UnitPt NextUnit(UnitPt u) ;
Pt LookupCurrUnitParameter(AtomPt parName) ;
UnitPt DefineUnit(AtomPt atom, Hdl parNames, int nPars) ;
CharPt UnitSignature(UnitPt u) ;
UnitPt TermToUnit(Pt term) ;
void ListUnitPredicates(UnitPt u) ;
Bool UnitCheck(VoidPt ref) ;
void UnitsInit(void) ;

#endif
