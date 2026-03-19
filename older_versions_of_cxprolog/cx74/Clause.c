/*
 *   This file is part of the CxProlog system

 *   Clause.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"

static ClausePt NewClause(PredicatePt pred, Hdl code, int codeLen, Pt head, Pt term)
{
	register ClausePt cl = TemporaryAllocate(WordsOf(Clause) + codeLen) ;
		
	ClauseOwner(cl) = pred ;
	if( not PredIsNotIndexed(pred) ) {
		/* Save information for building the index, later. */
		register Hdl args = XStructArgs(head) ;
		register int n = XStructArity(head) ;
		if( n > nIndexedArgs ) n = nIndexedArgs ;
		while( n-- )
			ClauseIdxArg(cl, n) = 
				  IsStruct(args[n]) ? TagStruct(XStructFunctor(args[n]))
				: IsList(args[n]) ? TagList(nil)
				: args[n] ;
	}
	ClauseTerm(cl) = term ;
	ClauseNext(cl) = nil ;
	ClauseArity(cl) = PredArity(pred) ;
	CopyWords(ClauseCodeSkipHeader(cl), code, codeLen) ;
	return cl ;
}

ClausePt InstallClause(PredicatePt pr, Hdl code, int codeLen, Pt head, Pt term, Bool end)
{
	ClausePt cl = NewClause(pr, code, codeLen, head, term) ;

	if( PredClauses(pr) == nil ) {	/* first is to be added */
		PredClauses(pr) = cl ;
		ClauseInst(cl) = TryMeElse ;
	}
	elif( end ) {
		register ClausePt c ;
		for( c = PredClauses(pr) ; ClauseNext(c) != nil ; c = ClauseNext(c) ) ;
		ClauseNext(c) = cl ;
		if( c != PredClauses(pr) )
			ClauseInst(c) = RetryMeElse ;
		ClauseInst(cl) = TrustMe ;
	}
	else {
		ClausePt sec = ClauseNext(cl) = PredClauses(pr) ;
		PredClauses(pr) = cl ;
		ClauseInst(cl) = TryMeElse ;
		if( ClauseNext(sec) == nil )
			ClauseInst(sec) = TrustMe ;
		else
			ClauseInst(sec) = RetryMeElse ;
	}
	InvalidIndex(pr) ;
	return cl ;
}

void CompileClause(register Pt clauseTerm, Bool end)
{
	PredicatePt pr ;
	ClausePt cl ;
	Hdl code ;
	long size ;
	Pt head, body ;

	clauseTerm = AllocateTermForAssert(clauseTerm) ;
	if( IsThisStruct(clauseTerm, neckFunctor) ) {
		head = XStructArg(clauseTerm,0) ;
		body = XStructArg(clauseTerm,1) ;
	}
	else {
		head = clauseTerm ;
		body = tTrueAtom ;
	}

	pr = LookupPredicate(XTestFunctor(head), false) ;
	if( PredIsBuiltin(pr) ) {
		ReleaseTerm(clauseTerm) ;
		Error("Attempt to add a clause to builtin predicate '%s'", PredNameArity(pr)) ;
	}
	if( PredIsImported(pr) ) {
		ReleaseTerm(clauseTerm) ;
		Error("Attempt to add a clause to imported predicate '%s'", PredNameArity(pr)) ;
	}

	Compiler(head, body, &code, &size) ;
	cl = InstallClause(pr, code, size, head, clauseTerm, end) ;
	if( CurrUnit() == builtinUnit || not keepSource_flag ) {
		ReleaseTerm(ClauseTerm(cl)) ;
		ClauseTerm(cl) = nil ;
	}
}

void DeleteClause(ClausePt cl)
{
	PredicatePt pr = ClauseOwner(cl) ;
	ClausePt next ;

	if( PredIsBuiltin(pr) )
		Error("Atempt to delete clause from builtin predicate '%s'", PredNameArity(pr)) ;

	if( PredClauses(pr) == cl ) {	/* first is to be removed */
		PredClauses(pr) = ClauseNext(cl) ;
		if( PredClauses(pr) != nil )
			ClauseInst(PredClauses(pr)) = TryMeElse ;
	}
	else {
		register ClausePt c ;
		for( c = PredClauses(pr) ; ClauseNext(c) != cl ; c = ClauseNext(c) ) ;
		next = ClauseNext(c) = ClauseNext(cl) ;
		if( c == PredClauses(pr) )
			ClauseInst(c) = TryMeElse ;
		elif( next != nil )
			ClauseInst(c) = RetryMeElse ;
		else
			ClauseInst(c) = TrustMe ;
	}
	if( ClauseTerm(cl) != nil ) ReleaseTerm(ClauseTerm(cl)) ;
	Release(cl) ;
	InvalidIndex(pr) ;
}
