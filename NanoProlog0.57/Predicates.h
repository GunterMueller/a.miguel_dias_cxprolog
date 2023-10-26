/*
 *   This file is part of the NanoProlog system

 *   Predicates.h
 *   by A.Miguel Dias - 89/11/14
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 940225: new function: EnterUserMode()
 931117: release of version 0.5

*/

#ifndef _Predicates_
#define _Predicates_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif


/* PREDICATES */

typedef struct Predicate
{
	Hdl predCode ;					/* Address of the start of code */
	Hdl idxHConst, idxHStruct ;		/* Code index hash tables */
	Pt idxInst[2] ;					/* Index instruction */
	Proc cProc ;					/* Code of a C predicate */
	struct Clause *clauses ;		/* Prolog clause chain */
	Int nClauses ;					/* Number of predicate clauses */

	struct Module *module ;			/* Predicate module */	
	struct Predicate *nextF ;		/* Next predicate in functor list */
	struct Predicate *nextM ;		/* Next predicate in module list */
	FunctorPt functor ;				/* Predicate functor */

	Bool noIndex : 1 ;				/* Predicate is not to be indexed */
	Bool haveIndex : 1 ;			/* Predicate currently have no index */
} Predicate, *PredicatePt ;

extern PredicatePt predicates ;

#define cPredicatePt(x)				((PredicatePt)(x))

#define PredNextF(p)				(p)->nextF
#define PredNextM(p)				(p)->nextM
#define PredModule(p)				(p)->module
#define PredFunctor(p)				(p)->functor
#define PredAtom(p)					FunctorAtom(PredFunctor(p))
#define PredArity(p)				FunctorArity(PredFunctor(p))
#define PredName(p)					FunctorName(PredFunctor(p))
#define PredNClauses(p)				(p)->nClauses
#define PredFirstClause(p)			(p)->clauses
#define PredProc(p)					(p)->cProc
#define PredNoIndex(p)				(p)->noIndex
#define PredHaveIndex(p)			(p)->haveIndex
#define PredMeta(p)					(p)->meta
#define PredCode(p)					(p)->predCode
#define PredIdxInst(p)				(p)->idxInst
#define PredHConst(p)				(p)->idxHConst
#define PredHStruct(p)				(p)->idxHStruct
#define PredIsC(p)					FunctorIsC(PredFunctor(p))
#define PredIsSys(p)				FunctorIsSys(PredFunctor(p))
#define PredIsMeta(p)				FunctorIsMeta(PredFunctor(p))

#define LookupPredicate2(n, a)	LookupPredicate(LookupFunctor2(n, a))

PredicatePt LookupPredicate(FunctorPt f) ;
PredicatePt LookupPredicateInModule(FunctorPt f, struct Module *m) ;
CharPt PredNameArity(PredicatePt pr) ;
void EnterUserMode(void) ;

Predicate *InstallCSysPred(CharPt name, int arity, Proc cProc) ;
void FinishInstallingCSysPreds(void) ;
void ListCPredicates(void) ;
CharPt ProcToName(Proc p) ;


/* CLAUSES */

typedef struct Clause
{
	Predicate *owner ;				/* Clause predicate */
	struct Clause *next ;			/* Next clause */
	struct Clause *prev ;			/* Previous clause */
	Pt idxArg ;						/* First argument info */
	Pt term ;						/* Clause's term */
/*	Pt code[] ;	*/					/* The code */
} Clause, *ClausePt ;

#define ClausePt(x)					((ClausePt)(x))

#define ClauseNext(c)				(c)->next
#define ClausePrev(c)				(c)->prev
#define ClauseIdxArg(c)				(c)->idxArg
#define ClauseTerm(c)				(c)->term
#define ClauseCode(c)				cHdl((c) + 1)
#define ClauseArity(c)				PredArity((c)->owner)

ClausePt InstallClauseHead(PredicatePt pr, Hdl code, int codeLen, Pt term) ;
ClausePt InstallClauseTail(PredicatePt pr, Hdl code, int codeLen, Pt term) ;
void CompileClause(Pt term, Bool end) ;
void DeleteClause(ClausePt cl) ;

void ListCSysPreds(void) ;
void InstallCSysPreds(void) ;
void InstallExtraCSysPreds(void) ;
void InstallMoreCSysPreds(void) ;

#endif
