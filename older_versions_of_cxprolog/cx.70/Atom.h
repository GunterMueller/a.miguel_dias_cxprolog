/*
 *   This file is part of the CxProlog system

 *   Atom.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Atom_
#define _Atom_

/* ATOM */

typedef struct Atom
{
	struct Atom *nextHash ;			/* Next atom in the hash chain */
	struct Functor *functors ;		/* Functor list */
/*	char name[] ;	*/				/* Name string */
} Atom, *AtomPt ;

typedef void (*AtomProc)(AtomPt) ;

#define	cAtomPt(a)			((AtomPt)a)

#define AtomFunctors(a)		(a)->functors
#define AtomName(a)			cCharPt((a) + 1)

void InitAtoms(void) ;
AtomPt LookupAtom(CharPt name) ;
void ForEachAtom(AtomProc p) ;
void ListAtoms(void) ;

extern Pt tNilAtom, tEofAtom, tCutAtom, tTrueAtom, tFailAtom, tBracketsAtom ;



/* FUNCTOR */

typedef struct Functor
{
	struct Functor *nextArity ;		/* Next functor of different arity */
	AtomPt atom ;					/* Functor's atom */
	struct Predicate *predicates ;	/* Predicates for this functor */
	int arity ;						/* Functor's arity */
	Bool meta : 1 ;					/* Functor of builtin meta predicate ",/;/not" */
} Functor, *FunctorPt ;

#define	cFunctorPt(f)		((FunctorPt)f)

#define FunctorAtom(f)		(f)->atom
#define FunctorName(f)		AtomName(FunctorAtom(f))
#define FunctorArity(f)		(f)->arity
#define FunctorPreds(f)		(f)->predicates
#define FunctorIsMeta(f)	(f)->meta

extern FunctorPt commaFunctor, semicolonFunctor, neckFunctor,
		listFunctor, cutFunctor, eqFunctor, barFunctor,
		varFunctor, primitiveFunctor, unitParamFunctor ;

FunctorPt LookupFunctor(AtomPt atom, int arity) ;
FunctorPt LookupFunctorByName(CharPt name, int arity) ;
CharPt FunctorNameArity(FunctorPt f) ;
void InitFunctors(void) ;

#endif
