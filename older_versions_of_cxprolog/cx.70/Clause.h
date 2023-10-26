/*
 *   This file is part of the CxProlog system

 *   Clause.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Clause_
#define _Clause_

typedef struct Clause
{
	struct Predicate *owner ;		/* Clause predicate */
	Pt idxArg ;						/* First argument info */
	Pt term ;						/* Clause's term */
/* The code. Must be at the end of the clause */
	Pt tryMeInst ;					/* TryMeElse, RetryMeElse, TrustMe */
	struct Clause *next ;			/* Next clause */
	Int arity ;						/* Clause arity */
} Clause, *ClausePt ;

#define cClausePt(x)				((ClausePt)(x))

#define ClauseOwner(c)				(c)->owner
#define ClauseIdxArg(c)				(c)->idxArg
#define ClauseTerm(c)				(c)->term
#define ClauseInst(c)				(c)->tryMeInst
#define ClauseNext(c)				(c)->next
#define ClauseArity(c)				(c)->arity

#define ClauseCode(c)				(&ClauseInst(c))
#define ClauseCodeSkipHeader(c)		(cHdl(ClauseCode(c)) + 3) /* skip TryMeElse l n */

#define PredHasOneClause(p)			( PredHasClauses(p) && ClauseNext(PredFirstClause(p)) == nil )

void CompileClause(Pt term, Bool end) ;
void DeleteClause(ClausePt cl) ;

void ListCBuiltinPreds(void) ;
void InstallCBuiltinPreds(void) ;
void InstallExtraCBuiltinPreds(void) ;
void InstallMoreCBuiltinPreds(void) ;

#endif