/*
 *   This file is part of the CxProlog system

 *   Predicate.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Predicates_
#define _Predicates_

typedef struct Predicate
{
	Pt startInst ;					/* Starting segment of code */
	Pt args[3] ;					/* Args 0, 1 and 2	*/		
	ClausePt clauses ;				/* Clause list (it's also the argument 3) */
	FunctorPt functor ;				/* Predicate functor (it's also the argument 4 */

	struct Predicate *nextF ;		/* Next predicate in functor list */

	struct Unit *unit ;				/* Predicate unit */	
	struct Predicate *nextU ;		/* Next predicate in unit list */
	
	Bool notIndexed : 1,			/* Predicate is not to be indexed */
		hasIndex : 1,				/* Currently, predicate has no index */
		isBuiltin : 1,				/* Cannot be locally redefined */
		isVisible : 1,				/* Predicate is visible in its unit */
		isImported : 1 ;			/* Predicate is imported from another unit */
} Predicate, *PredicatePt ;

#define cPredicatePt(x)				((PredicatePt)(x))

#define PredStartInst(pr)			(pr)->startInst
#define PredStartInstArgs(pr)		(pr)->args
#define PredCode(pr)				cHdl(&PredStartInst(pr))

#define PredNextF(pr)				(pr)->nextF
#define PredNextU(pr)				(pr)->nextU
#define PredUnit(pr)				(pr)->unit
#define PredFunctor(pr)				(pr)->functor
#define PredAtom(pr)				FunctorAtom(PredFunctor(pr))
#define PredArity(pr)				FunctorArity(PredFunctor(pr))
#define PredName(pr)				FunctorName(PredFunctor(pr))
#define PredFirstClause(pr)			(pr)->clauses
#define PredHasClauses(pr)			( PredFirstClause(pr) != nil )
#define PredHasNoClauses(pr)		( PredFirstClause(pr) == nil )

#define PredIsNotIndexed(pr)		(pr)->notIndexed
#define PredHasIndex(pr)			(pr)->hasIndex
#define PredIsBuiltin(pr)			(pr)->isBuiltin
#define PredIsVisible(pr)			(pr)->isVisible
#define PredIsImported(pr)			(pr)->isImported

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
CharPt PredNameArity(PredicatePt pr) ;
void DeletePredicate(PredicatePt pr) ;
void EnterUserMode(void) ;
void SetForceVisibility(Bool b) ;
Bool GetForceVisibility(void) ;

void ImportPredicate(PredicatePt pr, Pt term) ;
void CheckImports(void) ;

PredicatePt InstallCBuiltinPred(CharPt name, int arity, Proc cProc) ;
PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, Proc cProc) ;
void ListCPredicates(void) ;
CharPt CPredNameArity(Pt p) ;

#endif
