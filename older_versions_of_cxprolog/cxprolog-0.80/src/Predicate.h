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
	
	Bool indexable : 1 ;			/* Predicate is indexable */
	Bool indexMade : 1 ;			/* Index already created */
	Bool visible : 1 ;				/* Predicate is visible in its unit */
	Bool permanent : 1 ;			/* Predicate is permanent */
	Bool traceable : 1 ;			/* Predicate is traceable */
	Bool noCurrUnit : 1 ;			/* No current unit when created */
	Bool mutableBuiltin : 1 ;		/* Predicate is a mutable builtin */
} Predicate, *PredicatePt ;

#define cPredicatePt(x)				( (PredicatePt)(x) )

#define PredStartInst(pr)			( (pr)->startInst )
#define PredStartInstArgs(pr)		( (pr)->args )
#define PredCode(pr)				( cHdl(&PredStartInst(pr)) )
#define PredIndex(pr)				( (pr)->index )

#define PredNextF(pr)				( (pr)->nextF )
#define PredNextU(pr)				( (pr)->nextU )
#define PredUnit(pr)				( (pr)->unit )
#define PredFunctor(pr)				( (pr)->functor )
#define PredAtom(pr)				( FunctorAtom(PredFunctor(pr)) )
#define PredArity(pr)				( FunctorArity(PredFunctor(pr)) )
#define PredName(pr)				( FunctorName(PredFunctor(pr)) )
#define PredIsBuiltin(pr)			( FunctorIsBuiltin(PredFunctor(pr)) )
#define PredIsSpy(pr)				( FunctorIsSpy(PredFunctor(pr)) )
#define PredIsMeta(pr)				( FunctorIsMeta(PredFunctor(pr)) )
#define PredClauses(pr)				( (pr)->clauses )
#define PredHasClauses(pr)			( PredClauses(pr) != nil )

#define PredIsIndexable(pr)			( (pr)->indexable )
#define PredIndexMade(pr)			( (pr)->indexMade )
#define PredIsVisible(pr)			( (pr)->visible )
#define PredIsPermanent(pr)			( (pr)->permanent )
#define PredIsNoCurrUnit(pr)		( (pr)->noCurrUnit )
#define PredIsMutableBuiltin(pr)	( (pr)->mutableBuiltin )

#define PredIsImported(pr)			( PredStartInst(pr) == AllocSwitchCtxCall )
#define GetImportTerm(pr)			( PredStartInstArgs(pr)[0] )
#define PredIsUndefined(pr)			( PredStartInst(pr) == Undef )
#define PredIsC(pr)					( PredStartInstArgs(pr)[0] == Proceed )
#define PredIsTraceable(pr)			( (pr)->traceable )

void BindPredicate(PredicatePt pr, Pt inst, Pt arg0) ;
void BindPredicateMoreArgs(PredicatePt pr, Pt arg1, Pt arg2) ;
PredicatePt FindPredicate(FunctorPt f) ;
PredicatePt FindPredicateByName(CharPt n, int a) ;
PredicatePt LookupPredicate(FunctorPt f) ;
PredicatePt LookupPredicateByName(CharPt n, int a) ;
PredicatePt CheckPredicate(FunctorPt f) ;
PredicatePt CheckPredicateByName(CharPt n, int a) ;
Size NumberOfClauses(PredicatePt pr) ;
CharPt PredNameArity(PredicatePt pr) ;
CharPt UPredNameArity(PredicatePt pr) ;
void MarkBuiltinsPermanent(void) ;
void AbolishPredicate(FunctorPt f) ;
Bool PredicateUpdateFlags(int newValue) ;

PredicatePt InstallCBuiltinPred(CharPt name, int arity, Proc cProc) ;
PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, Proc cProc) ;
CharPt CPredNameArity(Pt p) ;
void PredicatesInit(void) ;

#endif
