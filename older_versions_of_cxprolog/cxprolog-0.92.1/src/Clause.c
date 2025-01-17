/*
 *   This file is part of the CxProlog system

 *   Clause.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTıICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

/*
Static predicates - Kinds of segments of code:
	Static Clause: start with TryMeElse, RetryMeElse, TrustMe
	Dynamic Clause: start with DynamicElse or DynamicIUElse
	Index: start with SwitchOnAtomic, SwitchOnStructure, Try
	Bind Segm created by BindPredicate:
				start with LocalJump, MakeIndex, UndefPred, EmptyPred,
				SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
				C-Pred, PutNil, Import, DynamicEnter, DynamicIUEnter
*/

ClausePt failClause ;

static void SetIndexInfo(ClausePt cl, register Hdl args, register int n)
{ /* pre: n > 0 */
	ClauseIdxInfo(cl, 0) = 
		  IsStruct(*args) ? TagStruct(XStructFunctor(*args))
		: IsList(*args) ? TagList(nil)
		: *args ;
	if( n == 1 ) return ;
	args++ ;
	ClauseIdxInfo(cl, 1) = 
		  IsStruct(*args) ? TagStruct(XStructFunctor(*args))
		: IsList(*args) ? TagList(nil)
		: *args ;
	if( n == 2 ) return ;
	args++ ;
	ClauseIdxInfo(cl, 1) = 
		  IsStruct(*args) ? TagStruct(XStructFunctor(*args))
		: IsList(*args) ? TagList(nil)
		: *args ;
}

static ClausePt NewClause(PredicatePt pr, Hdl code, Size codeLen, Pt head, Pt source)
{
	register ClausePt cl = TempAllocate(WordsOf(Clause) + codeLen) ;
	ClauseOwner(cl) = pr ;
	ClauseSource(cl) = source ;
	ClauseConsultFile(cl) = nil ;
	ClauseNext(cl) = nil ;
	ClauseArity(cl) = PredArity(pr) ;
	CopyWords(ClauseCodeSkipHeader(cl), code, codeLen) ;
	if( PredIsIndexable(pr) )
		SetIndexInfo(cl, XStructArgs(head), PredArity(pr)) ;
	return cl ;
}

static ClausePt PreviousClause(ClausePt cl) /* pre: cl not first */
{
	register ClausePt prev ;
	for( prev = PredClauses(ClauseOwner(cl)) ;
			ClauseNext(prev) != cl ;
			prev = ClauseNext(prev) ) ;
	return prev ;
}

static void BindDynamicPredicate(PredicatePt pr)
{							/* pre: pr is dynamic or multifile */
	if( PredHasClauses(pr) ) {
		if( PredIsIndexable(pr) )
			BindPredicateFull(pr,
				PredIsLogical(pr) ? DynamicEnterIndexed : DynamicIUEnterIndexed,
				nil, nil, nil, cPt(PredClauses(pr))) ;
		else
			BindPredicateFull(pr,
				PredIsLogical(pr) ? DynamicEnter : DynamicIUEnter,
				cPt(PredClauses(pr)), cPt(PredArity(pr)), nil, nil) ;
	}
	else
		BindPredicate(pr, EmptyPred, cPt(PredFunctor(pr))) ;
}

static void BindStaticPredicate(PredicatePt pr)
{							/* pre: pr is not dynamic or multifile */
	if( PredHasClauses(pr) )
		BindPredicate(pr, MakeIndex, cPt(pr)) ;
	elif( PredIsVisible(pr) )
		BindPredicate(pr, EmptyPred, cPt(PredFunctor(pr))) ;
	else
		BindPredicate(pr, UndefPred, cPt(PredFunctor(pr))) ;
}

static void LogicalSetLife(ClausePt cl, Bool assert)
{					/* pre: PredIsLogical(ClauseOwner(cl)) */
	if( assert ) {
		ClauseBirth(cl) = GlobalClock ; /* Active call doesn't see the clause yet */
		ClauseSetAlive(cl) ;
		GlobalClock = IncIntPt(GlobalClock) ;
	}
	elif( ClauseIsAlive(cl) ) {
		ClauseDeath(cl) = GlobalClock ; /* Active call still sees the clause */
		GlobalClock = IncIntPt(GlobalClock) ;
	}
	if( IsEndOfTime(GlobalClock) )
		FatalError("End-of-time reached for dynamic logical predicates") ;
}

ClausePt InstallClause(PredicatePt pr, Hdl code, Size codeLen,
										Pt head, Pt source, Bool end)
{	/* pre: clause instalation already validated */
	ClausePt cl = NewClause(pr, code, codeLen, head, source) ;

	if( PredIsDynamic(pr) || PredIsMultifile(pr) ) {
		if( !PredHasClauses(pr) ) {	/* first added */
			PredClauses(pr) = PredLastClause(pr) = cl ;
		}
		elif( end ) {
			ClauseNext(PredLastClause(pr)) = cl ;
			PredLastClause(pr) = cl ;
		}
		else {
			ClauseNext(cl) = PredClauses(pr) ;
			PredClauses(pr) = cl ;
		}
		if( PredIsLogical(pr) ) {
			ClauseInst(cl) =
					PredIsIndexable(pr) ? DynamicElseIndexed : DynamicElse ;
			LogicalSetLife(cl, true) ;
		}
		else
			ClauseInst(cl) =
					PredIsIndexable(pr) ? DynamicIUElseIndexed : DynamicIUElse ;
		BindDynamicPredicate(pr) ;
	}
	else {
		if( !PredHasClauses(pr) ) {	/* first added */
			PredClauses(pr) = PredLastClause(pr) = cl ;
			ClauseInst(cl) = TryMeElse ;
		}
		elif( end ) {
			ClauseNext(PredLastClause(pr)) = cl ;
			if( PredLastClause(pr) != PredClauses(pr) )
				ClauseInst(PredLastClause(pr)) = RetryMeElse ;
			PredLastClause(pr) = cl ;
			ClauseInst(cl) = TrustMe ;
		}
		else {
			ClausePt sec = ClauseNext(cl) = PredClauses(pr) ;
			PredClauses(pr) = cl ;
			ClauseInst(cl) = TryMeElse ;
			if( ClauseNext(sec) != nil )
				ClauseInst(sec) = RetryMeElse ;
			else
				ClauseInst(sec) = TrustMe ;
		}
		BindStaticPredicate(pr) ;
	}
	
	return cl ;
}

static void DeinstallClause(ClausePt cl)
{				/* ClauseNext(cl) is not changed here */
	PredicatePt pr = ClauseOwner(cl) ;
	if( PredIsDynamic(pr) || PredIsMultifile(pr) ) {
		if( PredClauses(pr) == cl ) {	/* first is removed */
			PredClauses(pr) = ClauseNext(cl) ;
			if( !PredHasClauses(pr) )
				PredLastClause(pr) = nil ;
		}
		else {
			ClausePt prev = PreviousClause(cl) ; ;
			ClauseNext(prev) = ClauseNext(cl) ;
			if( ClauseNext(prev) == nil )
				PredLastClause(pr) = prev ;
		}
	}
	else {
		if( PredClauses(pr) == cl ) {	/* first is removed */
			PredClauses(pr) = ClauseNext(cl) ;
			if( !PredHasClauses(pr) )
				PredLastClause(pr) = nil ;
			else
				ClauseInst(PredClauses(pr)) = TryMeElse ;
		}
		else {
			ClausePt prev = PreviousClause(cl) ; ;
			ClauseNext(prev) = ClauseNext(cl) ;
			if( ClauseNext(prev) == nil )
				PredLastClause(pr) = prev ;
			if( prev != PredClauses(pr) ) {
				if( ClauseNext(prev) == nil )
					ClauseInst(prev) = TrustMe ;
				else
					ClauseInst(prev) = RetryMeElse ;
			}
		}
	}
}

void ReinstallClausesAsStatic(PredicatePt pr)
{
	register ClausePt cl ;
	if( !PredHasClauses(pr) ) return ;
	doseq(cl, PredClauses(pr), ClauseNext(cl)) {
		ClauseInst(cl) = RetryMeElse ;
		if( !PredKeepSource(pr) ) {
			ReleaseTerm(ClauseSource(cl)) ;
			ClauseSource(cl) = nil ;
		}
	}
	ClauseInst(PredLastClause(pr)) = TrustMe ;
	ClauseInst(PredClauses(pr)) = TryMeElse ;
	BindStaticPredicate(pr) ;
}



/* CLAUSE AND INDEX DELETION */

/* The clauses and indexes deleted by retract/1, retractall/1
   and abolish/1 are stored in the following variables until
   they are garbage collected */

static ClausePt deletedClauses = nil ;
static Hdl deletedIndexes = nil ;

void DeleteClause(ClausePt cl)
{
	PredicatePt pr = ClauseOwner(cl) ;
	if( PredIsPermanent(pr) )
		DatabaseError("Cannot modify '%s' because it is a static built-in",
													PredNameArity(pr)) ;
	if( PredIsDynamic(pr) || PredIsMultifile(pr) ) {
		if( PredIsLogical(pr)) {			/* dynamic or multifile */
			LogicalSetLife(cl, false) ;
				/* First clause must always be alive */
			cl = PredClauses(pr) ;
			while( cl != nil && !ClauseIsAlive(cl) ) {
				DeinstallClause(cl) ;
				ClauseNextDeleted(cl) = deletedClauses ;
				deletedClauses = cl ;
				cl = PredClauses(pr) ;	
			}
		}
		else {								/* dynamic_iu */
			DeinstallClause(cl) ;
				/* Retracting clauses first-to-last may leave clauses
					dead in active chains. So this is needed. */
			ClauseCodeSkipHeader(cl)[0] = Fail ; /* DiscardAndFail ??? */
			ReleaseTerm(ClauseSource(cl)) ;
			ClauseSource(cl) = nil ;	/* clause/2 and retract/1 use this */
			ClauseNextDeleted(cl) = deletedClauses ;
			deletedClauses = cl ;
		}
		BindDynamicPredicate(pr) ;
	}
	else {									/* static */
		DeinstallClause(cl) ;
		ReleaseTerm(ClauseSource(cl)) ;
		ClauseSource(cl) = nil ;
		ClauseNextDeleted(cl) = deletedClauses ;
		deletedClauses = cl ;
		BindStaticPredicate(pr) ;
	}
}

void DeleteClausesAndIndex(PredicatePt pr)
{
	ClausePt cl, next ;
	Hdl idx ;
/* Delete clauses */
	doseq(cl, PredClauses(pr), next) {
		next = ClauseNext(cl) ;
		DeleteClause(cl) ;
	}
/* invariant: PredClauses(pr) == nil */
/* Delete index */
	if( (idx = PredIndex(pr)) != nil ) {
		idx[0] = cPt(deletedIndexes) ;
		deletedIndexes = idx ;
		PredIndex(pr) = nil ;
	}
}

void ClauseGC(Bool blind)
{
	if( blind ) {
		while( deletedClauses != nil ) {
			ClausePt cl = deletedClauses ;
			deletedClauses = ClauseNextDeleted(deletedClauses) ;
			if( ClauseSource(cl) != nil ) /* for dynamic */
				ReleaseTerm(ClauseSource(cl)) ;
			Release(cl) ;
		}
		while( deletedIndexes != nil ) {
			Hdl idx = deletedIndexes ;
			deletedIndexes = cHdl(deletedIndexes[0]) ;
			Release(idx) ;
		}
		if( true ) { /* Handle dynamic logical predicates */
			register UnitPt u ;
			register PredicatePt pr ;
			register ClausePt cl ;
			doseq(u, unitList, UnitNext(u))
				doseq(pr, UnitPreds(u), PredNextU(pr))
					if( PredIsLogical(pr) ) {
						doseq(cl, PredClauses(pr), ClauseNext(cl))
							if( ClauseIsAlive(cl) )
								ClauseBirth(cl) = zeroIntPt ;
							else {
								DeinstallClause(cl) ;
								ReleaseTerm(ClauseSource(cl)) ;
								Release(cl) ;
							}
					}
			GlobalClock = oneIntPt ;
		}
	/* @@ MemoryWarning("Garbage collector recovered n bytes from m clauses") ; */
	}
	else
		Error("ClauseGC(false) not yet implemented") ;
}



/* CXPROLOG C'BUILTINS */

static void ShowPred(Hdl cp)
{
	register Hdl h ;
	ClausePt cl = nil ;
	for( h = cp ; *h != TryMeElse && *h != RetryMeElse && *h != TrustMe ; h-- ) ;
	cl = cClausePt(h - Df(&ClauseInst(cl), cl)) ;
	Mesg(PredNameArity(ClauseOwner(cl))) ;
}

/*
{question/2.}
{try/1.}
{$$_lux1/0.}
{once/1.}
{$$_run_thread/1.}
*/

static void PActiveClauses(void)
{
	register EnvironmentPt e = E ;
	register ChoicePointPt b = B ;

	ShowPred(CP) ;
	for( ; Lt(b, stacksEnd) ; b = b->B ) {
		for( ; Lt(0,b) ; e = e[-1].E )
			ShowPred(e[-1].CP) ;
		ShowPred(b->CP) ;
		e = b->E ;
	}
	JumpNext() ;
}

void ClausesInit()
{
	GlobalClock = zeroIntPt ;
	
	if( failPred == nil ) InternalError("ClausesInit") ;
	UseScratch() ;
	Gen0(Fail) ;
	Gen0(Proceed) ;
	failClause = NewClause(failPred, ScratchStart(), 2, tNilAtom, tNilAtom) ;
	ClauseIdxInfo(failClause, 0) =  TagList(nil) ;
	ClauseDeath(failClause) = zeroIntPt ;
	FreeScratch() ;

	InstallCBuiltinPred("__act_clauses", 0, PActiveClauses) ;
}
