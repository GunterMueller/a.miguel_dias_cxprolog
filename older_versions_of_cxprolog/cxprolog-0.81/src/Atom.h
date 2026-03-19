/*
 *   This file is part of the CxProlog system

 *   Atom.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
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
	Bool isOp : 1 ;
/*	char name[] ;	*/					/* Name string */
} Atom, *AtomPt ;

typedef Size (*AtomFun)(AtomPt, Pt) ;
typedef Size (*FunctorFun)(struct Functor *, Pt) ;

#define	cAtomPt(a)			((AtomPt)a)

#define AtomFunctors(a)		(a)->functors
#define AtomName(a)			cCharPt((a) + 1)
#define AtomPermanent(a)	(a)->permanent
#define AtomMarked(a)		(a)->marked
#define AtomIsOp(a)			(a)->isOp

void AtomsInit(void) ;
AtomPt LookupAtom(CharPt name) ;
AtomPt LookupTempAtom(CharPt name) ;
Size ForEachAtom(AtomFun p, Pt x) ;
void AtomsClearNotMarked(void) ;
Size Spec(Pt t, FunctorFun p, Pt x) ;
Size AtomsList(void) ;
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
	int arity : 16 ;				/* Functor's arity */
	Bool builtin : 1 ;				/* Functor of builtin predicate */
	Bool meta : 1 ;					/* Functor of builtin meta predicate */
	Bool spy : 1 ;					/* Spy point */
} Functor, *FunctorPt ;

#define	cFunctorPt(f)		((FunctorPt)f)

#define FunctorAtom(f)		(f)->atom
#define FunctorName(f)		AtomName(FunctorAtom(f))
#define FunctorArity(f)		(f)->arity
#define FunctorPreds(f)		(f)->predicates
#define FunctorIsBuiltin(f)	(f)->builtin
#define FunctorIsMeta(f)	(f)->meta
#define FunctorIsSpy(f)		(f)->spy

extern FunctorPt commaFunctor, semicolonFunctor, neckFunctor,
		commandFunctor, slashFunctor, formatFunctor, listFunctor,
		cutFunctor, eqFunctor, barFunctor, hifenFunctor, varFunctor,
		primitiveFunctor, unitParamFunctor ;

Bool FunctorCheck(VoidPt ref) ;
FunctorPt LookupFunctor(AtomPt atom, int arity) ;
FunctorPt LookupFunctorByName(CharPt name, int arity) ;
CharPt FunctorNameArity(FunctorPt f) ;
Size ForEachFunctor(FunctorFun p, Pt x) ;
Size SpyFunctor(FunctorPt f, Pt x) ;
void NoSpyAll(void) ;
void WriteSpyPoints(void) ;
void FunctorsInit(void) ;

#endif
