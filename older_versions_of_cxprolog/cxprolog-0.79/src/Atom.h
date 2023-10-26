/*
 *   This file is part of the CxProlog system

 *   Atom.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Atom_
#define _Atom_

/* ATOM */

typedef struct Atom
{
	struct Atom *nextHash ;			/* Next atom in the hash chain */
	struct Functor *functors ;		/* Functor list */
	Bool permanent : 1 ;
	Bool marked : 1 ;
/*	Char name[] ;	*/				/* Name string */
} Atom, *AtomPt ;

typedef void (*AtomProc)(AtomPt) ;

#define	cAtomPt(a)			((AtomPt)a)

#define AtomFunctors(a)		(a)->functors
#define AtomName(a)			cCharPt((a) + 1)
#define AtomPermanent(a)	(a)->permanent
#define AtomMarked(a)		(a)->marked

void AtomsInit(void) ;
AtomPt LookupAtom(CharPt name) ;
AtomPt LookupTempAtom(CharPt name) ;
void ForEachAtom(AtomProc p) ;
void AtomsClearNotMarked(void) ;
void AtomsList(void) ;
void PrintAtomTable(void) ;
void AtomsInit2(void) ;

extern Pt tNilAtom, tEmptyAtom, tEofAtom, tCutAtom, tTrueAtom, tFalseAtom,
	tFailAtom, tOnAtom, tOffAtom,
	tBracketsAtom, tStringAtom, tMinusAtom, tUnderAtom, tDotAtom,
	tEllispisAtom, tErrorAtom, tGoingAtom,
	tKilledAtom, tCompletedAtom, tFailedAtom, tUserAtom ;


/* FUNCTOR */

typedef struct Functor
{
	struct Functor *nextArity ;		/* Next functor of different arity */
	AtomPt atom ;					/* Functor's atom */
	struct Predicate *predicates ;	/* Predicates for this functor */
	short arity ;					/* Functor's arity */
	Bool meta : 1 ;					/* Functor of builtin meta predicate ",/;/not" */
} Functor, *FunctorPt ;

#define	cFunctorPt(f)		((FunctorPt)f)

#define FunctorAtom(f)		(f)->atom
#define FunctorName(f)		AtomName(FunctorAtom(f))
#define FunctorArity(f)		(f)->arity
#define FunctorPreds(f)		(f)->predicates
#define FunctorIsMeta(f)	(f)->meta

extern FunctorPt commaFunctor, semicolonFunctor, neckFunctor,
		commandFunctor, slashFunctor, formatFunctor, listFunctor,
		cutFunctor, eqFunctor, barFunctor, hifenFunctor, varFunctor,
		primitiveFunctor, unitParamFunctor ;

Bool FunctorCheck(VoidPt ref) ;
FunctorPt LookupFunctor(AtomPt atom, int arity) ;
FunctorPt LookupFunctorByName(CharPt name, int arity) ;
CharPt FunctorNameArity(FunctorPt f) ;
void FunctorsInit(void) ;

#endif
