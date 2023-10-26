/*
 *   This file is part of the CxProlog system

 *   Atom.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Atom_
#define _Atom_

/* ATOM */

typedef struct Atom
{
	struct Atom *nextHash ;			/* Next atom in the hash chain */
	struct Functor *functors ;		/* Functor list */
	struct Unit *unit ;				/* Unit identified by this atom */
	struct IVar *ivar ;				/* IVar identified by this atom */
	struct Operator *op ;			/* Operator identified by this atom */
	Bool permanent : 1 ;
	Bool gcMarked : 1 ;
/*	Char name[] ;	*/				/* Name string */
} Atom, *AtomPt ;

typedef Size (*AtomFun)(AtomPt) ;
typedef Size (*FunctorFun)(struct Functor *) ;

#define	cAtomPt(a)			((AtomPt)a)

#define AtomFunctors(a)		((a)->functors)
#define AtomToUnit(a)		((a)->unit)
#define AtomToIVar(a)		((a)->ivar)
#define AtomToOperator(a)	((a)->op)
#define AtomName(a)			cCharPt((a) + 1)
#define AtomIsPermanent(a)	((a)->permanent)
#define AtomIsGCMarked(a)	((a)->gcMarked)

void AtomsInit(void) ;
AtomPt LookupAtom(CharPt name) ;
AtomPt LookupTempAtom(CharPt name) ;
Size AtomForEach(AtomFun fun) ;
Bool AtomCheck(VoidPt ref) ;
Size AtomGCUnmark(AtomPt at) ;
void AtomGCMark(AtomPt at) ;
int AtomGCClearNotMarked(void) ;
Size ForEachInSpec(Pt t, FunctorFun fun, Bool allowAtoms) ;
void AtomsInit2(void) ;

extern Pt tNilAtom, tEmptyAtom, tEofAtom, tCutAtom, tTrueAtom, tFalseAtom,
	tFailAtom, tOnAtom, tOffAtom, tMinusAtom, tUnderAtom, tDotAtom,
	tBracketsAtom, tQuestionAtom, tEllispisAtom,
	tLessAtom, tEqualAtom, tGreaterAtom, tErrorAtom, tGoingAtom,
	tKilledAtom, tCompletedAtom, tFailedAtom,
	tTextAtom, tBinaryAtom, tEofCodeAtom, tResetAtom,
	tUserAtom, tVoidAtom, tNullAtom, tBadTermAtom ;


/* FUNCTOR */

typedef struct Functor
{
	struct Functor *nextArity ;		/* Next functor of different arity */
	AtomPt atom ;					/* Functor's atom */
	struct Predicate *predicates ;	/* Predicates for this functor */
	int arity : 16 ;				/* Functor's arity */
	Bool builtin : 1 ;				/* Functor of builtin predicate */
	Bool meta : 1 ;					/* Functor of builtin meta predicate */
	Bool xinline : 1 ;				/* Functor of builtin meta predicate that is compiled inline */
	Bool spy : 1 ;					/* Spy point */
} Functor, *FunctorPt, **FunctorHdl ;

#define	cFunctorPt(f)		((FunctorPt)f)
#define	cFunctorHdl(f)		((FunctorHdl)f)

#define FunctorAtom(f)		(f)->atom
#define FunctorName(f)		AtomName(FunctorAtom(f))
#define FunctorArity(f)		(f)->arity
#define FunctorPreds(f)		(f)->predicates
#define FunctorIsBuiltin(f)	(f)->builtin
#define FunctorIsMeta(f)	(f)->meta
#define FunctorIsInLine(f)	(f)->xinline
#define FunctorIsSpy(f)		(f)->spy

extern FunctorPt commaFunctor, semicolonFunctor, neckFunctor,
		commandFunctor, slashFunctor, colonFunctor, listFunctor, stringFunctor,
		metaCutFunctor, eqFunctor, barFunctor, hifenFunctor,
		bracketsFunctor, parFunctor, varFunctor, primitiveFunctor,
		unitParamFunctor, emptyFunctor, ctxPushFunctor, ctxSwitchFunctor,
		ctxHEnterFunctor, ctxHExitFunctor ;

Bool FunctorCheck(VoidPt ref) ;
FunctorPt LookupFunctor(AtomPt atom, int arity) ;
FunctorPt LookupFunctorByName(CharPt name, int arity) ;
CharPt FunctorNameArity(FunctorPt f) ;
Size ForEachFunctor(FunctorFun fun) ;
Size SpyOff(FunctorPt f) ;
Size SpyOn(FunctorPt f) ;
void NoSpyAll(void) ;
void WriteSpyPoints(void) ;
void FunctorsInit(void) ;

#endif
