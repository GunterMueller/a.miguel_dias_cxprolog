/*
 *   This file is part of the CxProlog system

 *   Clause.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define DEBUG 0

static ClausePt NewClause(PredicatePt pred, Hdl code, Size codeLen, Pt head, Pt source)
{
	register ClausePt cl = TempBlockAllocate(WordsOf(Clause) + codeLen) ;
		
	ClauseOwner(cl) = pred ;
	if( PredIsIndexed(pred) ) {
		/* Save information for building the index, later. */
		register Hdl args = XStructArgs(head) ;
		register int n = XStructArity(head) ;
		if( n > 3 ) n = 3 ;
		while( n-- )
			ClauseIdxArg(cl, n) = 
				  IsStruct(args[n]) ? TagStruct(XStructFunctor(args[n]))
				: IsList(args[n]) ? TagList(nil)
				: args[n] ;
	}
	ClauseSource(cl) = source ;
	ClauseNext(cl) = nil ;
	ClauseArity(cl) = PredArity(pred) ;
	CopyWords(ClauseCodeSkipHeader(cl), code, codeLen) ;
	return cl ;
}

ClausePt InstallClause(PredicatePt pr, Hdl code, Size codeLen, Pt head, Pt source, Bool end)
{
	ClausePt cl = NewClause(pr, code, codeLen, head, source) ;

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

void CompileClause(Pt source, Bool end)
{
	PredicatePt pr ;
	ClausePt cl ;
	Hdl code ;
	Size size ;
	Pt head, body ;

#if DEBUG
	Write("%s\n", TermAsStr(source)) ;
#endif
	source = AllocateTermForAssert(source) ;
	if( IsThisStruct(source, neckFunctor) ) {
		head = XStructArg(source,0) ;
		body = XStructArg(source,1) ;
	}
	else {
		head = source ;
		body = tTrueAtom ;
	}

	pr = LookupPredicate(XTestFunctor(head), false) ;
	if( PredIsBuiltin(pr) ) {
		ReleaseTerm(source) ;
		Error("Attempt to add a clause to builtin predicate '%s'", UPredNameArity(pr)) ;
	}
	if( PredIsImported(pr) ) {
		ReleaseTerm(source) ;
		Error("Attempt to add a clause to imported predicate '%s'", UPredNameArity(pr)) ;
	}

	Compiler(head, body, &code, &size) ;
	cl = InstallClause(pr, code, size, head, source, end) ;
	if( CurrUnit() == builtinUnit || not keepSource_flag ) {
		ReleaseTerm(ClauseSource(cl)) ;
		ClauseSource(cl) = nil ;
	}
#if DEBUG
	Disassemble(ClauseCode(cl)) ;
	getchar() ;
#endif
}

void DeleteClause(ClausePt cl)
{
	PredicatePt pr = ClauseOwner(cl) ;
	ClausePt next ;

	if( PredIsBuiltin(pr) )
		Error("Atempt to delete clause from builtin predicate '%s'", UPredNameArity(pr)) ;

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
	if( ClauseSource(cl) != nil ) ReleaseTerm(ClauseSource(cl)) ;
	BlockRelease(cl) ;
	InvalidIndex(pr) ;
}
