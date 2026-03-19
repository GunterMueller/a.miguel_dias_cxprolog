/*
 *   This file is part of the NanoProlog system

 *   Predicates.c
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
 940225: some changes related to PredIsSys(.)
 931117: release of version 0.5

*/

#include "NanoProlog.h"


/* PREDICATES */

static Predicate *NewPredicate(Functor *f)
{
	Predicate *pr = HeapAlloc(WordsOf(Predicate), true) ;

	PredFunctor(pr) = f ;
	PredNextF(pr) = FunctorPreds(f) ;
	FunctorPreds(f) = pr ;
	PredNClauses(pr) = 0 ;
	PredFirstClause(pr) = nil ;
	PredIdxInst(pr)[0] = MakeIndex ;
	PredIdxInst(pr)[1] = cPt(pr) ;
	PredHConst(pr) = nil ;
	PredHStruct(pr) = nil ;
	PredNextM(pr) = ModulePredicates(SourceModule()) ;
	PredIsSys(pr) = SourceModule() == sysModule ;
	ModulePredicates(SourceModule()) = pr ;
	PredModule(pr) = SourceModule() ;
	PredNoIndex(pr) = false ;
	PredHaveIndex(pr) = false ;
	PredCode(pr) = &PFailAddr ;
	return( pr ) ;
}

PredicatePt FindPredicate(FunctorPt f)
{
	return( nil ) ;
}

Predicate *LookupPredicate(Functor *f)
{
	register PredicatePt pr ;

	if( FunctorIsSys(f) )	/* Sys predicates belong to all the modules */
		return( FunctorPreds(f) ) ;
	foreach(pr, FunctorPreds(f), PredNextF(pr))
		if( PredModule(pr) == SourceModule() )
			return( pr ) ;
	return( NewPredicate(f) ) ;
}

PredicatePt LookupPredicateInModule(FunctorPt f, struct Module *m)
{
	register PredicatePt pr ;

	if( FunctorIsSys(f) )	/* Sys predicates belong to all the modules */
		return( FunctorPreds(f) ) ;
	foreach(pr, FunctorPreds(f), PredNextF(pr))
		if( PredModule(pr) == m )
			return( pr ) ;
	return( NewPredicate(f) ) ;
}

char *PredNameArity(Predicate *pr)
{
	static char s[120] ;
	
	if( PredModule(pr) != SourceModule() && PredModule(pr) != sysModule )
		sprintf(s, "%s%s%s/%d", ModuleName(PredModule(pr)),
			FunctorName(switchFunctor), PredName(pr), PredArity(pr)) ;
	else sprintf(s, "%s/%d", PredName(pr), PredArity(pr)) ;
	return(s) ;
}

void EnterUserMode()
{
	register PredicatePt *pr, p ;

	if( SourceModule() != sysModule )
		FatalError("EnterUserMode() cannot be called twice") ;

	SetSourceModule(LookupModule(userAtom)) ;
	
/* Predicates refered in the Boot file that without clauses are considered user predicates */
	for( pr = &ModulePredicates(sysModule) ; *pr != nil ; )
		if( PredNClauses(*pr) == 0 && not PredIsC(*pr) )
		{
			p = *pr ;
			*pr = PredNextM(*pr) ;
			PredNextM(p) = ModulePredicates(SourceModule()) ;
			ModulePredicates(SourceModule()) = p ;
			PredIsSys(p) = false ;
			PredModule(p) = SourceModule() ;
		}
		else pr = &PredNextM(*pr) ;
}

/* C PREDICATES */

static PredicatePt cPredicates = nil ;

Predicate *InstallCPredicate(char *name, int arity, Proc cPr)
{
	Predicate *pr ;
	FunctorPt f = LookupFunctor2(name, arity) ;

	if( SourceModule() != sysModule )
		Error("All C predicates must be installed on sysModule") ;
	if( cPredicates != nil )
		 Error("Installing C predicates already stopped") ;
	pr = LookupPredicate(f) ;
	if( PredIsC(pr) )
		 Error("C predicate already installed") ;
	PredIsC(pr) = true ;
	PredProc(pr) = cProc(Z(cPr)) ;
	return( pr ) ;
}

void StopInstallCPredicates()
{
	cPredicates = ModulePredicates(sysModule) ;
}

char *ProcToName(Proc p)
{
	Predicate *pr ;
	
	foreach(pr, cPredicates, PredNextM(pr))
		if( PredProc(pr) == p )
			return( PredNameArity(pr) ) ;
	Default("ProcToName") ;
}

void ListCPredicates()
{
	Predicate *pr ;
	
	foreach(pr, cPredicates, PredNextM(pr))
		printf("%s\n", PredNameArity(pr)) ;
}



/* CLAUSES */

/* Returns first clause argument. If first argument is a structure its
  functor is returned with the struct tag. If clause head is an atom
  nil is returned. */
static Pt CodifyIdxArg(Pt t)
{
	Pt head = ClauseHead(t) ;
	Pt idx ;

	if( IsStruct(head) ) idx = XStructArg(head, 0) ;
	elif( IsList(head) ) idx = XListHead(head) ;
	else return( nil ) ;
	
	if( IsStruct(idx) ) idx = TagStruct(XStructFunctor(idx)) ;
	elif( IsList(idx) ) idx = TagList(nil) ;
	return( idx ) ;	
}

static Clause *NewClause(Predicate *pred, Hdl code, int codeLen, Pt term)
{
	Clause *cl = HeapAlloc(WordsOf(Clause) + codeLen, false) ;
		
	cl->owner = pred ;
	ClauseNext(cl) = nil ;
	ClausePrev(cl) = nil ;
	if( PredArity(pred) > 0 ) ClauseIdxArg(cl) = CodifyIdxArg(term) ;
	CopyWords(ClauseCode(cl), code, codeLen) ;
	ClauseTerm(cl) = term ;
	return( cl ) ;
}

static PredicatePt LookupPredicateByTerm(register Pt term)
{
	FunctorPt f ;
	
	VarValue(term) ;
	if( IsThisStruct(term, neckFunctor) )
		term = Drf(XStructArg(term, 0)) ;

	if( IsVar(term) ) Error("Clause head is a var") ;
	elif( IsNumber(term) ) Error("Clause head is a number") ;
	elif( IsAtom(term) ) f = LookupFunctor(XAtom(term), 0) ;
	elif( IsList(term) ) f = listFunctor ;
	else f = XStructFunctor(term) ;

	return( LookupPredicate(f) ) ;
}

void CompileClause(Pt clause, Bool end)
{
	PredicatePt pr = LookupPredicateByTerm(clause) ;
	Hdl code ;
	long size ;

	if( SourceModule() == sysModule )
	{
		clause = CopyTermToHeap(clause) ;
		Compiler(clause, end, &code, &size) ;
		ClauseTerm(InstallClause(pr, code, size, clause, end)) = nil ;	
		FreeHeapTerm(clause) ;
	}
	else
	{
		if( PredIsSys(pr) )
			Error("Atempt to change system predicate '%s'", PredNameArity(pr)) ;
		clause = CopyTermToHeap(clause) ;
		Compiler(clause, end, &code, &size) ;
		InstallClause(pr, code, size, clause, end) ;
	}	
}

Clause *InstallClauseHead(PredicatePt pr, Hdl code, int codeLen, Pt term)
{
	Clause **cl, *first ;
	
	cl = &PredFirstClause(pr) ;
	first = *cl ;
	*cl = NewClause(pr, code, codeLen, term) ;
	(**cl).next = first ;
	if( first != nil ) ClausePrev(first) = *cl ;
	PredNClauses(pr)++ ;
	InvalidIndex(pr) ;
	return( *cl ) ;
}

Clause *InstallClauseTail(PredicatePt pr, Hdl code, int codeLen, Pt term)
{
	Clause **cl, *save ;
	
	for( cl = &PredFirstClause(pr), save = nil ; *cl != nil ;
								save = *cl, cl = &(**cl).next ) ;
	*cl = NewClause(pr, code, codeLen, term) ;
	(**cl).prev = save ;
	PredNClauses(pr)++ ;
	InvalidIndex(pr) ;
	return( *cl ) ;
}

void DeleteClause(Clause *cl)
{
	Predicate *pr = cl->owner ;
	Clause *prev = ClausePrev(cl),
		   *next = ClauseNext(cl) ;
	
	if( prev == nil )	/* first is to be removed */
	{
		PredFirstClause(pr) = next ;
		if( next != nil )
			ClausePrev(next) = nil ;
	}
	elif( next == nil )	/* last is to be removed */
	{
		ClauseNext(prev) = nil ;
	}
	else
	{
		ClauseNext(prev) = next ;
		ClausePrev(next) = prev ;
	}

	if( ClauseTerm(cl) != nil ) FreeHeapTerm(ClauseTerm(cl)) ;
	HeapFree(cl) ;
	PredNClauses(pr)-- ;
	InvalidIndex(pr) ;
}
