/*
 *   This file is part of the CxProlog system

 *   Predicate.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Predicates_
#define _Predicates_

typedef struct Predicate
{
	Pt startInst ;					/* Starting segment of code */
	Pt args[4] ;					/* Args 0, 1, 2 and 3	*/		
	ClausePt clauses ;				/* Clause list (it's also the argument 4) */
	FunctorPt functor ;				/* Predicate functor (it's also the argument 5) */
	Hdl index ;						/* Predicate index. nil if no index */

	struct Predicate *nextF ;		/* Next predicate in functor list */

	struct Unit *unit ;				/* Predicate unit */	
	struct Predicate *nextU ;		/* Next predicate in unit list */
	
	Bool indexed : 1,				/* Predicate is to be indexed */
		indexMade : 1,				/* Index already created */
		isNeverBuiltin : 1,			/* Can never be made builtin */
		isBuiltin : 1,				/* Can be redefined only in a boot file */
		isVisible : 1,				/* Predicate is visible in its unit */
		isImported : 1,				/* Predicate is imported from another unit */
		isTraceable : 1 ;			/* Predicate is traceable */
} Predicate, *PredicatePt ;

#define cPredicatePt(x)				((PredicatePt)(x))

#define PredStartInst(pr)			(pr)->startInst
#define PredStartInstArgs(pr)		(pr)->args
#define PredCode(pr)				cHdl(&PredStartInst(pr))
#define PredIndex(pr)				(pr)->index

#define PredNextF(pr)				(pr)->nextF
#define PredNextU(pr)				(pr)->nextU
#define PredUnit(pr)				(pr)->unit
#define PredFunctor(pr)				(pr)->functor
#define PredAtom(pr)				FunctorAtom(PredFunctor(pr))
#define PredArity(pr)				FunctorArity(PredFunctor(pr))
#define PredName(pr)				FunctorName(PredFunctor(pr))
#define PredClauses(pr)				(pr)->clauses
#define PredHasClauses(pr)			( PredClauses(pr) != nil )
#define PredHasNoClauses(pr)		( PredClauses(pr) == nil )

#define PredIsIndexed(pr)			(pr)->indexed
#define PredIndexMade(pr)			(pr)->indexMade
#define PredIsBuiltin(pr)			(pr)->isBuiltin
#define PredIsNeverBuiltin(pr)		(pr)->isNeverBuiltin
#define PredIsVisible(pr)			(pr)->isVisible
#define PredIsImported(pr)			(pr)->isImported
#define PredIsTraceable(pr)			(pr)->isTraceable

#define PredIsC(pr)					(PredStartInstArgs(pr)[0] == Proceed)
#define PredIsUndefined(pr)			(PredStartInst(pr) == Undef)
#define GetImportTerm(pr)			PredStartInstArgs(pr)[0]

void BindPredicate(PredicatePt pr, Pt inst, Pt arg0) ;
void BindPredicateMoreArgs(PredicatePt pr, Pt arg1, Pt arg2) ;
PredicatePt FindPredicate(FunctorPt f) ;
PredicatePt FindPredicateByName(CharPt n, int a) ;
void MakeVisible(PredicatePt pr, Bool err) ;
Bool CanBeVisible(FunctorPt f) ;
PredicatePt FindVisiblePredicate(FunctorPt f) ;
PredicatePt LookupPredicate(FunctorPt f, Bool change) ;
Size NumberOfClauses(PredicatePt pr) ;
CharPt PredNameArity(PredicatePt pr) ;
CharPt UPredNameArity(PredicatePt pr) ;
void MarkCoreBuiltins(void) ;
void MarkBuiltins(void) ;
void PredicateUpdateFlags(void) ;

PredicatePt InstallCBuiltinPred(CharPt name, int arity, Proc cProc) ;
PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, Proc cProc) ;
CharPt CPredNameArity(Pt p) ;
void PredicatesInit(void) ;

#endif