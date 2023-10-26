/*
 *   This file is part of the CxProlog system

 *   Unit.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Unit_
#define _Unit_

typedef struct Unit
{
	struct Unit *next ;			/* Next unit in the unit chain */
	PredicatePt predicates ;	/* Unit predicate list */
	FunctorPt functor ;			/* Unit name/arity */
	Hdl anonymousLoc ;			/* Location of anonymous unit in the GS */
	Bool defined : 1 ;
	Bool marked : 1 ;
/*	Pt params[] ;	*/			/* Unit parameter names (tagged atoms) */
} Unit, *UnitPt ;

typedef Size (*UnitFun)(UnitPt) ;

#define cUnitPt(p)					((UnitPt)(p))

extern UnitPt unitList, builtinUnit, bottomUnit ;

#define UnitFunctor(u)				(u)->functor
#define UnitAtom(u)                 FunctorAtom(UnitFunctor(u))
#define UnitArity(u)                FunctorArity(UnitFunctor(u))
#define UnitName(u)					AtomName(UnitAtom(u))
#define UnitPreds(u)				(u)->predicates
#define UnitAnonymousLoc(u)			(u)->anonymousLoc
#define UnitNext(u)					(u)->next
#define UnitParams(u)				cHdl(cUnitPt(u) + 1)
#define UnitParam(u,i)				UnitParams(u)[i]

#define UnitIsAnonymous(u)			(UnitAnonymousLoc(u) != nil)
#define UnitIsDefined(u)			(u)->defined
#define UnitIsMarked(u)				(u)->marked

#define CurrUnit()		( C == tNilAtom ? bottomUnit : cUnitPt(XListArg(C,-1)) )

Pt LookupCurrUnitParameter(Pt parName) ;
CharPt UnitSignature(UnitPt u) ;
UnitPt TermToUnit(Pt term, Hdl pushLoc) ;
void UnitsGC(Hdl top) ;
Bool UnitCheck(VoidPt ref) ;
void NoReconsultAll(void) ;
void UnitsInit(void) ;

#endif
