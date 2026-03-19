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
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

/* Kinds of segments of code:
	Clause: start with TryMeElse, RetryMeElse, TrustMe
	Index: start with SwitchOnAtomic, SwitchOnStructure, Try
	Bind Segm created by BindPredicate:
				start with LocalJump, MakeIndex, Undef,
				SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
				C-Pred, PutNil, Import 
*/

static ClausePt NewClause(PredicatePt pred, Hdl code, Size codeLen, Pt head, Pt source)
{
	register ClausePt cl = TempAllocate(WordsOf(Clause) + codeLen) ;
	
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
	InvalidIndex(pr) ;
	return cl ;
}

ClausePt CompileClause(Pt source, Bool end)
{
	register PredicatePt pr ;
	register ClausePt cl ;
	Hdl code ;
	Size size ;
	Pt head, body ;

#if 0
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

	if( reconsulting_flag > 0 && !PredReconsulted(pr) ) {
		AbolishPredicate(pr) ;
		PredReconsulted(pr) = true ;
	}

	if( PredIsPermanent(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("Attempt to add a clause to permanent predicate '%s'",
													PredNameArity(pr)) ;
	}
	if( PredIsImported(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("Attempt to add a clause to imported predicate '%s'",
													PredNameArity(pr)) ;
	}
	if( PredIsNoCurrUnit(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("No current unit when creating new predicate '%s'",
													PredNameArity(pr)) ;
	}
	Compiler(head, body, &code, &size) ;
	cl = InstallClause(pr, code, size, head, source, end) ;
	if( (Booting() && CurrUnit() == builtinUnit) || !keepSource_flag ) {
		ReleaseTerm(ClauseSource(cl)) ;
		ClauseSource(cl) = nil ;
	}
#if 0
	Write("%d\n", size) ;
#endif
#if 0
	Disassemble(ClauseCode(cl)) ;
	getchar() ;
#endif
	return cl ;
}

void CompileClauseList(register Pt list, Bool end, Bool visible)
{
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) ) {
		ClausePt cl = CompileClause(XListHead(list), end) ;
		PredIsVisible(ClauseOwner(cl)) = visible ;
	}
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
}

static void ReleaseClause(ClausePt cl)
{
	PredicatePt pr = ClauseOwner(cl) ;

	if( PredClauses(pr) == cl ) {	/* first is to be removed */
		PredClauses(pr) = ClauseNext(cl) ;
		if( PredClauses(pr) != nil )
			ClauseInst(PredClauses(pr)) = TryMeElse ;
	}
	else {
		register ClausePt c ;
		for( c = PredClauses(pr) ; ClauseNext(c) != cl ; c = ClauseNext(c) ) ;
		ClauseNext(c) = ClauseNext(cl) ;
		if( ClauseNext(c) == nil )
			PredLastClause(pr) = c ;
		if( c != PredClauses(pr) ) {
			if( ClauseNext(c) != nil )
				ClauseInst(c) = RetryMeElse ;
			else
				ClauseInst(c) = TrustMe ;
		}
	}
#if 0
	if( ClauseSource(cl) != nil )
		Mesg("RELEASED CLAUSE: %s", TermAsStr(ClauseSource(cl))) ;
#endif
	ReleaseTerm(ClauseSource(cl)) ;
	Release(cl) ;
	InvalidIndex(pr) ;
}

void DeleteClause(ClausePt cl)
{
	PredicatePt pr = ClauseOwner(cl) ;

	if( PredIsPermanent(pr) )
		DatabaseError("Attempt to delete clause from permanent predicate '%s'",
													PredNameArity(pr)) ;
	ClauseCodeSkipHeader(cl)[0] = DeletedClause ;
	PredHasDeletedClauses(ClauseOwner(cl)) = true ;
	ReleaseTerm(ClauseSource(cl)) ;
	ClauseSource(cl) = nil ;
	ClauseIdxArg(cl, 0) =
		ClauseIdxArg(cl, 1) =
		ClauseIdxArg(cl, 2) = minusOneIntPt ;
	InvalidIndex(pr) ;
}


/* CXPROLOG C'BUILTINS */

static void PClauseGC(void)
{
	UnitPt u ;
	register PredicatePt pr ;
	register ClausePt cl, next ;
	doseq(u, unitList, UnitNext(u))
		doseq(pr, UnitPreds(u), PredNextU(pr))
			if( PredHasDeletedClauses(pr) ) {
				PredHasDeletedClauses(pr) = false ;
				doseq(cl, PredClauses(pr), next) {
					next = ClauseNext(cl) ;
					if( IsDeletedClause(cl) )
						ReleaseClause(cl) ;
				}
			}
	JumpNext() ;
}

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
	InstallCBuiltinPred("$$_clause_gc", 0, PClauseGC) ;
	InstallCBuiltinPred("act_clauses", 0, PActiveClauses) ;
}
