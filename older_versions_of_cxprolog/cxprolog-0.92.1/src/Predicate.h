/*
 *   This file is part of the CxProlog system

 *   Predicate.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Predicates_
#define _Predicates_

typedef struct Predicate
{
	Inst startInst ;				/* Starting segment of code */
	Pt args[4] ;					/* Args 0, 1, 2 and 3	*/		
	ClausePt clauses ;				/* Clause list (it's also the argument 4) */
	FunctorPt functor ;				/* Predicate functor (it's also the argument 5) */
	ClausePt lastClause ;			/* Speeds up assertz/1 */
	Hdl index ;						/* Predicate index. nil if no index */

	struct Predicate *nextF ;		/* Next predicate in functor list */

	struct Unit *unit ;				/* Predicate unit */	
	struct Predicate *nextU ;		/* Next predicate in unit */
	AtomPt consultFile ;			/* Predicate consult filename */

	Word16 consultGen : 16 ;		/* Predicate consult generation */
	Bool indexable : 1 ;			/* Predicate is indexable */
	Bool dynamic : 1 ;				/* Predicate is dynamic */
	Bool multifile : 1 ;			/* Predicate is multifile  */
	Bool logical : 1 ;				/* Logical or immediate update semantic view */
	Bool visible : 1 ;				/* Predicate is visible in its unit */
	Bool keepSource : 1 ;			/* Keep the source for its clauses  */
	Bool permanent : 1 ;			/* Predicate is permanent (static builtin) */
	Bool traceable : 1 ;			/* Predicate is traceable */
	Bool abolished : 1 ;			/* Predicate has been abolished */
} Predicate, *PredicatePt ;

#define cPredicatePt(x)				( (PredicatePt)(x) )

#define PredStartInst(pr)			( (pr)->startInst )
#define PredStartInstArgs(pr)		( (pr)->args )
#define PredCode(pr)				( cHdl(&PredStartInst(pr)) )
#define PredIndex(pr)				( (pr)->index )

#define PredNextF(pr)				( (pr)->nextF )
#define PredUnit(pr)				( (pr)->unit )
#define PredNextU(pr)				( (pr)->nextU )
#define PredConsultFile(pr)			( (pr)->consultFile )
#define PredFunctor(pr)				( (pr)->functor )
#define PredAtom(pr)				( FunctorAtom(PredFunctor(pr)) )
#define PredArity(pr)				( FunctorArity(PredFunctor(pr)) )
#define PredName(pr)				( FunctorName(PredFunctor(pr)) )
#define PredIsBuiltin(pr)			( FunctorIsBuiltin(PredFunctor(pr)) )
#define PredIsSpy(pr)				( FunctorIsSpy(PredFunctor(pr)) )
#define PredIsMeta(pr)				( FunctorIsMeta(PredFunctor(pr)) )
#define PredClauses(pr)				( (pr)->clauses )
#define PredHasClauses(pr)			( PredClauses(pr) != nil )
#define PredLastClause(pr)			( (pr)->lastClause )

#define PredConsultGen(pr)			( (pr)->consultGen )
#define PredIsIndexable(pr)			( (pr)->indexable )
#define PredIsDynamic(pr)			( (pr)->dynamic )
#define PredIsMultifile(pr)			( (pr)->multifile )
#define PredIsLogical(pr)			( (pr)->logical )
#define PredIsVisible(pr)			( (pr)->visible )
#define PredKeepSource(pr)			( (pr)->keepSource )
#define PredIsPermanent(pr)			( (pr)->permanent )
#define PredIsTraceable(pr)			( (pr)->traceable )
#define PredWasAbolished(pr)		( (pr)->abolished )

#define PredIsMutableBuiltin(pr)	( PredIsBuiltin(pr) && PredIsDynamic(pr) )

/* There are 5 kinds of predicates:
	Local		- have clauses or is visible or dynamic
	Imported	- no clauses (may be visible)
	C determ	- no clauses (always visible)
	C nondeterm	- have 2 clauses (always visible)
	Undefined	- no clauses, not visible, not dynamic
*/
#define PredIsImported(pr)          ( PredStartInst(pr) == Import )
#define GetImportTerm(pr)			( PredStartInstArgs(pr)[0] )
#define PredIsC(pr)					( PredStartInstArgs(pr)[0] == Proceed )
#define PredIsCNonDeterm(pr)		( PredStartInst(pr) == PutNil )
#define PredIsUndefined(pr)			( PredStartInst(pr) == UndefPred )

#define PredCNonDetermAMem(pr)		( PredStartInstArgs(pr)[2][2] )

extern PredicatePt failPred ;

void BindPredicate(PredicatePt pr, Inst inst, Pt a0) ;
void BindPredicateFull(PredicatePt pr, Inst inst, Pt a0, Pt a1, Pt a2, Pt a3) ;
PredicatePt FindPredicateInCurrUnit(FunctorPt f) ;
PredicatePt FindPredicate(FunctorPt f) ;
PredicatePt LookupPredicate(FunctorPt f) ;
PredicatePt FindPredicateByName(CharPt n, int a) ;
PredicatePt LookupPredicateByName(CharPt n, int a) ;
PredicatePt CheckPredicate(FunctorPt f) ;
PredicatePt CheckPredicateByName(CharPt n, int a) ;
Size NumberOfClauses(PredicatePt pr) ;
CharPt PredNameArity(PredicatePt pr) ;
Pt PredNameArityTerm(PredicatePt pr) ;
Bool PredHiddenInGeneration(PredicatePt pr) ;
void MarkStaticBuiltinsAsPermanent(void) ;
void AbolishPredicate(PredicatePt pr, Bool force) ;
void CompatibleIfThenUpdateFlags(int newValue) ;
PredicatePt PredNextUPlusBuiltins(PredicatePt pr) ;
ClausePt SetNewClause(Pt source, Bool end, Bool consulting) ;
void SetDynamic(PredicatePt pr, Bool logical) ;
void SetMultifile(PredicatePt pr) ;
void SetVisible(PredicatePt pr) ;
void SetImport(PredicatePt pr, Pt unit) ;
void SetClauseList(Pt list, Bool end, Bool visible) ;

PredicatePt InstallCBuiltinPred(CharPt name, int arity, VFun cProc) ;
PredicatePt InstallGNDeterCBuiltinPred(CharPt name, int arity, int extraA, VFun cProc) ;
PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, VFun cProc) ;
PredicatePt FindCPredByInst(Inst inst) ;
PredicatePt CurrCPred(void) ;
CharPt CurrCPredNameArity(void) ;
void PredicatesInit(void) ;

#endif
