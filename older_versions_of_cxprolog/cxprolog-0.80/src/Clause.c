/*
 *   This file is part of the CxProlog system

 *   Clause.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

#define DEBUG 0

static ClausePt NewClause(PredicatePt pred, Hdl code, Size codeLen, Pt head, Pt source)
{
	register ClausePt cl = TempBlockAllocate(WordsOf(Clause) + codeLen) ;
		
	ClauseOwner(cl) = pred ;
	if( PredIsIndexable(pred) ) {
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

#if DEBUG > 0
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

	pr = LookupPredicate(XTestFunctor(head)) ;
	if( PredIsPermanent(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("Attempt to add a clause to permanent predicate '%s'",
													UPredNameArity(pr)) ;
	}
	if( PredIsImported(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("Attempt to add a clause to imported predicate '%s'",
													UPredNameArity(pr)) ;
	}
	if( PredIsNoCurrUnit(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("No current unit when creating new predicate '%s'",
													PredNameArity(pr)) ;
	}
	
	Compiler(head, body, &code, &size) ;
	cl = InstallClause(pr, code, size, head, source, end) ;
	if( Booting() && CurrUnit() == builtinUnit || not keepSource_flag ) {
		ReleaseTerm(ClauseSource(cl)) ;
		ClauseSource(cl) = nil ;
	}
#if DEBUG > 1
	Disassemble(ClauseCode(cl)) ;
	getchar() ;
#endif
}

void DeleteClause(ClausePt cl)
{
	PredicatePt pr = ClauseOwner(cl) ;
	ClausePt next ;

	if( PredIsPermanent(pr) )
		DatabaseError("Attempt to delete clause from permanent predicate '%s'",
													UPredNameArity(pr)) ;

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
