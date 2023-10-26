/*
 *   This file is part of the CxProlog system

 *   Predicate.c
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

static PredicatePt NewPredicate(FunctorPt f)
{
	register PredicatePt pr ;

	if( UnitIsReserved(CurrUnit()) )
		if( C == tNilAtom )
		  Error("No current unit when attempting to create new predicate '%s'",
													FunctorNameArity(f)) ;
		else
			Error("Cannot create the new predicate '%s' in the builtin unit",
													FunctorNameArity(f)) ;

	pr = PermanentAllocate(WordsOf(Predicate)) ;
	PredClauses(pr) = nil ;

	PredFunctor(pr) = f ;
	PredNextF(pr) = FunctorPreds(f) ;
	FunctorPreds(f) = pr ;

	PredUnit(pr) = CurrUnit() ;
	PredNextU(pr) = UnitPredicates(CurrUnit()) ;
	UnitPredicates(CurrUnit()) = pr ;

	PredIndex(pr) = nil ;
	PredIsNotIndexed(pr) = FunctorArity(f) == 0 || f == listFunctor ;
	PredIsBuiltin(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsImported(pr) = false ;

	InvalidIndex(pr) ;
	return pr ;
}

void BindPredicate(PredicatePt pr, Pt inst, Pt arg0)
{
	PredStartInst(pr) = inst ;
	PredStartInstArgs(pr)[0] = arg0 ;
}

void BindPredicateMoreArgs(PredicatePt pr, Pt arg1, Pt arg2)
{
	PredStartInstArgs(pr)[1] = arg1 ;
	PredStartInstArgs(pr)[2] = arg2 ;
}

PredicatePt FindPredicate(FunctorPt f)
{
	register PredicatePt pr ;
	register UnitPt u = CurrUnit() ;

	dolist(pr, FunctorPreds(f), PredNextF(pr))
		if( PredUnit(pr) == builtinUnit || PredUnit(pr) == u )
			return pr ;
	return nil ;
}

PredicatePt FindPredicateByName(CharPt n, int a)
{
	return FindPredicate(LookupFunctorByName(n, a)) ;
}

void MakeVisible(PredicatePt pr, Bool err)
{
	if( PredName(pr)[0] == '$' ) {
		if( err )
			Error("A predicate which name starts with a \'$\' cannot be \
							made visible: '%s'", PredNameArity(pr)) ;
	}
	else PredIsVisible(pr) = true ;
}

Bool CanBeVisible(FunctorPt f)
{
	return FunctorName(f)[0] != '$' ;
}

PredicatePt FindVisiblePredicate(FunctorPt f) /* pre: CanBeVisible(f) == true */
{
	register PredicatePt pr ;
	register UnitPt u = CurrUnit() ;

	dolist(pr, FunctorPreds(f), PredNextF(pr))
		if( PredUnit(pr) == builtinUnit || PredUnit(pr) == u )
			if( PredIsVisible(pr) || forceVisibility_flag )
				return pr ;
	return nil ;
}

PredicatePt LookupPredicate(FunctorPt f, Bool change)
{
	register PredicatePt pr ;

	if( (pr = FindPredicate(f)) != nil ) {
		if( change && PredIsBuiltin(pr) )
		  Error("Attempt to change builtin predicate '%s'", PredNameArity(pr)) ;
		return pr ;
	}
	return NewPredicate(f) ;
}

CharPt PredNameArity(PredicatePt pr)
{
	sprintf(retBuffer, "%s:%s/%d",
		UnitName(PredUnit(pr)), PredName(pr), PredArity(pr)) ;
	return retBuffer ;
}

void DeletePredicate(PredicatePt pr)
{
	ClausePt cl, next ;
	FunctorPt f = PredFunctor(pr) ;
	
	if( PredIsBuiltin(pr) )
		Error("Builtin predicate '%s' cannot be deleted", PredNameArity(pr)) ;

	dolist(cl, PredClauses(pr), next) {
		next = ClauseNext(cl) ;
		DeleteClause(cl) ;
	}
	if( PredHasIndex(pr) ) {
		Release(PredIndex(pr)) ;
		PredIndex(pr) = nil ;
	}

	PredIsNotIndexed(pr) = FunctorArity(f) == 0 || f == listFunctor ;
	PredIsBuiltin(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsImported(pr) = false ;
	InvalidIndex(pr) ;
}

static void ListUndefPredicates()
{
	register UnitPt u ;
	register PredicatePt pr ;
	
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredIsUndefined(pr) )
			Warning("Predicate '%s' used but undefined in unit '%s'",
				PredNameArity(pr), FunctorNameArity(UnitFunctor(builtinUnit))) ;

	dolist(u, FirstUnit(), NextUnit(u))
		dolist(pr, UnitPredicates(u), PredNextU(pr))
			if( PredIsUndefined(pr) )
				Warning("Predicate '%s' used but undefined in unit '%s'",
					PredNameArity(pr), FunctorNameArity(UnitFunctor(u))) ;
}

static void EnterUserMode()
{
	register PredicatePt pr ;
	register OperatorPt op ;
	register ImperativeVarPt iv ;

	if( UnitIsReserved(builtinUnit) )
		FatalError("EnterUserMode() cannot be called twice") ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr)) {
		PredIsBuiltin(pr) = not PredIsUndefined(pr) ;
		MakeVisible(pr, false) ;
	}
	for( op = FirstOperator() ; op != nil ; op = NextOperator(op) )
		OperatorIsBuiltin(op) = true ;
	for( iv = FirstImperativeVar() ; iv != nil ; iv = NextImperativeVar(iv) )
		IVarIsBuiltin(iv) = true ;	
	UnitIsReserved(builtinUnit) = true ;
}


/* IMPORTED PREDICATES */

void ImportPredicate(PredicatePt pr, Pt term)
{
	if( PredIsImported(pr) ) {
		if( XTestFunctor(GetImportTerm(pr)) != XTestFunctor(term) )
			Error("Predicate '%s' was already imported from another unit",
							PredNameArity(pr)) ;
	}
	elif( not PredIsUndefined(pr) )
		Error("Predicate '%s' cannot be imported because it was already \
						defined in this unit", PredNameArity(pr)) ;

	BindPredicate(pr, AllocSwitchCtxCall, AllocateTermForAssert(term)) ;
	BindPredicateMoreArgs(pr, cPt(WordsOf(Environment)), DeallocProceed) ;
	PredIsImported(pr) = true ;
}

static void CheckImports()
{
	register UnitPt u, u2 ;
	register PredicatePt pr, pr2 ;
	FunctorPt f2 ;
	Bool found ;
	
	dolist(u, FirstUnit(), NextUnit(u))
		dolist(pr, UnitPredicates(u), PredNextU(pr))
			if( PredIsImported(pr) ) {
				f2 = XTestFunctor(GetImportTerm(pr)) ;
				u2 = FindUnit(FunctorAtom(f2)) ;
				if( u2 == nil || UnitArity(u2) != FunctorArity(f2) )
					Warning("Predicate '%s' is imported from nonexistent \
unit '%s'", PredNameArity(pr), FunctorNameArity(f2)) ;
				
				found = false ;
				dolist(pr2, FunctorPreds(PredFunctor(pr)), PredNextF(pr2))
					if( PredUnit(pr2) == u2 ) {
						found = true ;
						break ;
					}			
				if( found && not PredIsVisible(pr2) || not found )
					Warning("Imported predicate '%s' is not visible in \
unit '%s'", PredNameArity(pr), FunctorNameArity(f2)) ;
			}
}


/* BUILTIN C PREDICATES */

PredicatePt InstallCBuiltinPred(CharPt name, int arity, Proc cProc)
{
	PredicatePt pr ;
	FunctorPt f = LookupFunctorByName(name, arity) ;

	if( CurrUnit() != builtinUnit )
		Error("All C predicates must be installed in the 'builtin' unit (%s/%d)",
				name, arity) ;

	pr = LookupPredicate(f, false) ;
	if( not PredIsUndefined(pr) )
		FatalError("Cannot install C predicate. Another one with the same \
					name and arity is already installed (%s/%d)", name, arity) ;
	BindPredicate(pr, cPt(Z(cProc)), Proceed) ;
	return pr ;
}

PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, Proc cProc)
{
	PredicatePt pr ;
	Hdl h ;
	ClausePt cl1, cl2 ;
	char buff[1024] ;
	FunctorPt f = LookupFunctorByName(name, arity) ;

	strcpy(buff, "@@@@_") ;
	strcat(buff, name) ;
	InstallCBuiltinPred(buff, arity, cProc) ;
	pr = InstallCBuiltinPred(name, arity, cProc) ;

	PredIsNotIndexed(pr) = true ;

	h = PermanentAllocate(2) ;
	h[0] = cPt(cProc) ;
	h[1] = Proceed ; 

	cl1 = InstallClause(pr, h, 2, nil, nil, true) ;
	ClauseArity(cl1)++ ;
	cl2 = InstallClause(pr, h, 2, nil, nil, true) ;
	ClauseInst(cl2) = RetryMeElse ;
	ClauseNext(cl2) = cl2 ;
	ClauseArity(cl2)++ ;

	BindPredicate(pr, PutNil, cPt(OutTemp(arity))) ;
	BindPredicateMoreArgs(pr, LocalJump, cPt(ClauseCode(cl1))) ;
	return pr ;
}

CharPt CPredNameArity(Pt p)
{
	register PredicatePt pr ;
	
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredStartInst(pr) == p )
			return PredNameArity(pr) ;
	if( p == CallVar || p == ExecuteVar )
		return "call/1" ;
	return nil ;
}

static void ListCBuiltinPreds()
{
	register PredicatePt pr ;
	
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredIsC(pr) )
			WriteStd("%s\n", PredNameArity(pr)) ;
}


/* CXPROLOG C'BUILTINS */

static void PNDRepeat()
{
	JumpNext()
}

static void PNDClause()
{
	ClausePt cl ;
	Bool b ;
	Hdl saveTrail, h ;
	Pt head, body, clTerm ;

	if( A(2) == tNilAtom ) {
		PredicatePt pr = FindPredicate(XTestFunctor(X0)) ;
		if( pr == nil || PredIsUndefined(pr) )
			Jump(DiscardAndFail)
		else
			cl = PredClauses(pr) ;
	}
	else cl = cClausePt(A(2)) ;
	
	for(;;) {
		if( cl == nil ) Jump(DiscardAndFail) ;
			/* for pached predicates */
		if( cl == ClauseNext(cl) ) Jump(DiscardAndFail) ;
		if( (clTerm = ClauseTerm(cl)) == nil )
			clTerm = MakeBinStruct(
						neckFunctor,
						MakeCleanTerm(PredFunctor(ClauseOwner(cl))),
						MakeAtom("SOURCE_UNAVAILABLE")) ;
		SplitClauseTerm(clTerm, &head, &body) ;
		saveTrail = TR ;
		b = Unify(X0, head) && Unify(X1, body) ;
		RestoreTrail(saveTrail, h) ;
		if( b ) break ;
		cl = ClauseNext(cl) ;
	}

	A(2) = cPt(ClauseNext(cl)) ;
	SplitClauseTerm(PushTerm(clTerm), &head, &body) ;
	if( Unify(X0, head) && Unify(X1, body) ) JumpNext()
	Error("Internal error in PClause") ;	
}

static void PAsserta()
{
	CompileClause(X0, false) ;
	JumpNext()
}

static void PAssertz()
{
	CompileClause(X0, true) ;
	JumpNext()
}

static void PAbolish()
{
	PredicatePt pr ;
	if( (pr = FindPredicate(XTestFunctor2(X0,X1))) != nil )
		DeletePredicate(pr) ;
	JumpNext()
}

static void PNDRetract()
{
	PredicatePt pr ;
	ClausePt cl ;
	Bool b ;
	Hdl saveTrail, h ;
	Pt head, body ;

	if( A(1) == tNilAtom ) {
		SplitClauseTerm(X0, &head, &body) ;
		pr = FindPredicate(XTestFunctor(head)) ;
		if( pr != nil && PredIsBuiltin(pr) )
			Error("Atempt to retract clause from builtin predicate '%s'", PredNameArity(pr)) ;
		if( pr == nil || PredIsUndefined(pr) )
			Jump(DiscardAndFail) ;
		cl = PredClauses(pr) ;
	}
	else cl = cClausePt(A(1)) ;
	
	for(;;) {
		if( cl == nil || ClauseTerm(cl) == nil ) Jump(DiscardAndFail) ;
		saveTrail = TR ;
		b = Unify(X0, ClauseTerm(cl)) ;
		RestoreTrail(saveTrail, h) ;
		if( b ) break ;
		cl = ClauseNext(cl) ;
	}

	A(1) = cPt(ClauseNext(cl)) ;
	if( Unify(X0, PushTerm(ClauseTerm(cl))) ) {
		DeleteClause(cl) ;
		JumpNext()
	}
	Error("Internal error in DoRetract") ;	
}

static void PNDCurrentPredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom ? UnitPredicates(CurrUnit()) : PredNextU(cPredicatePt(A(1))) ;
	dolist(pr, pr, PredNextU(pr))
		if( PredHasClauses(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanTerm(PredFunctor(pr))) ) JumpNext()
	DoFail()
}

static void PNDVisiblePredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom ? UnitPredicates(CurrUnit()) : PredNextU(cPredicatePt(A(1))) ;
	dolist(pr, pr, PredNextU(pr))
		if( PredIsVisible(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanTerm(PredFunctor(pr))) ) JumpNext()
	DoFail()
}

static void PNDImportedPredicate()
{
	PredicatePt pr =
		A(2) == tNilAtom ? UnitPredicates(CurrUnit()) : PredNextU(cPredicatePt(A(2))) ;
	dolist(pr, pr, PredNextU(pr))
		if( PredIsImported(pr) ) break ;
	A(2) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanTerm(PredFunctor(pr))) &&
		Unify(X1, GetImportTerm(pr)) ) JumpNext()
	DoFail()
}

static void PNDBuiltinPredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom ? UnitPredicates(builtinUnit) : PredNextU(cPredicatePt(A(1))) ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanTerm(PredFunctor(pr))) ) JumpNext()
	DoFail()
}

static void PCode1()
{
	ListAtomCode(XTestAtom(X0)) ;
	JumpNext()
}

static void PCode2()
{
	PredicatePt pr ;
	if( (pr = FindPredicate(XTestFunctor2(X0,X1))) != nil )
		ListPredicateCode(pr) ;
	JumpNext()
}

static void PListCBuiltinPreds()
{
	ListCBuiltinPreds() ;
	JumpNext()
}

static void PPrintAtomTable()
{
	PrintAtomTable() ;
	JumpNext()
}


static void PVisible()
{
	MakeVisible(LookupPredicate(XTestFunctor2(X0, X1), true), true) ;
	JumpNext()
}

static void PImport()
{
	ImportPredicate(LookupPredicate(XTestFunctor2(X0, X1), true), X2) ;
	JumpNext()
}

static void PCheckImports()
{
	CheckImports() ;
	JumpNext()
}

static void PUndef()
{
	ListUndefPredicates() ;
	JumpNext()
}

static void PUserMode()
{
	EnterUserMode() ;
	JumpNext()
}

void InitPredicates()
{
	InstallNDeterCBuiltinPred("repeat", 0, PNDRepeat) ;
	InstallNDeterCBuiltinPred("clause", 2, PNDClause) ;
	InstallCBuiltinPred("asserta", 1, PAsserta) ;
	InstallCBuiltinPred("assertz", 1, PAssertz) ;
	InstallCBuiltinPred("assert", 1, PAssertz) ;
	InstallCBuiltinPred("abolish", 2, PAbolish) ;
	InstallNDeterCBuiltinPred("retract", 1, PNDRetract) ;
	InstallNDeterCBuiltinPred("current_predicate", 1, PNDCurrentPredicate) ;
	InstallNDeterCBuiltinPred("visible_predicate", 1, PNDVisiblePredicate) ;
	InstallNDeterCBuiltinPred("imported_predicate", 2, PNDImportedPredicate) ;
	InstallNDeterCBuiltinPred("builtin_predicate", 1, PNDBuiltinPredicate) ;
	InstallCBuiltinPred("@@_code", 1, PCode1) ;
	InstallCBuiltinPred("@@_code", 2, PCode2) ;
	InstallCBuiltinPred("clist", 0, PListCBuiltinPreds) ;
	InstallCBuiltinPred("atlist", 0, PPrintAtomTable) ;
	InstallCBuiltinPred("visible", 2, PVisible) ;
	InstallCBuiltinPred("import", 3, PImport) ;
	InstallCBuiltinPred("check_imports", 0, PCheckImports) ;
	InstallCBuiltinPred("undef", 0, PUndef) ;
	InstallCBuiltinPred("@@_user_mode", 0, PUserMode) ;
}
