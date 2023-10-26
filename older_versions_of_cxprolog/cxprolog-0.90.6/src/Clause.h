/*
 *   This file is part of the CxProlog system

 *   Clause.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Clause_
#define _Clause_

typedef struct Clause
{
	struct Predicate *owner ;		/* Clause predicate */
	Pt idxArgs[3] ;					/* First three arguments info */
	Pt source ;						/* Clause's source */
/* The code. Must be at the end of the clause */
	Pt tryMeInst ;					/* TryMeElse, RetryMeElse, TrustMe */
	struct Clause *next ;			/* Next clause */
	Word arity ;					/* Clause arity */
} Clause, *ClausePt ;

#define cClausePt(x)				((ClausePt)(x))

#define ClauseOwner(c)				(c)->owner
#define ClauseIdxArg(c,n)			(((c)->idxArgs)[n])
#define ClauseSource(c)				(c)->source
#define ClauseInst(c)				(c)->tryMeInst
#define ClauseNext(c)				(c)->next
#define ClauseArity(c)				(c)->arity

#define ClauseCode(c)				(&ClauseInst(c))
#define ClauseCodeSkipHeader(c)		(cHdl(ClauseCode(c)) + 3) /* skip TryMeElse l n */
#define IsClauseLoop(cl)			( ClauseNext(cl) == cl )

#define ClauseFromAllocate(p)		(cClausePt(p) - 1)

#define PredHasOneClause(p)			( PredHasClauses(p) && ClauseNext(PredClauses(p)) == nil )
#define PredHasMultipleClauses(p)	( PredHasClauses(p) && ClauseNext(PredClauses(p)) != nil )

ClausePt InstallClause(struct Predicate *pr, Hdl code, Size codeLen, Pt head, Pt source, Bool end) ;
ClausePt CompileClause(Pt source, Bool end) ;
void CompileClauseList(Pt source, Bool end, Bool visible) ;
void DeleteClause(ClausePt cl) ;

#endif
