/*
 *   This file is part of the CxProlog system

 *   Predicate.c
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

	pr = PermBlockAllocate(WordsOf(Predicate)) ;
	PredClauses(pr) = nil ;

	PredFunctor(pr) = f ;
	PredNextF(pr) = FunctorPreds(f) ;
	FunctorPreds(f) = pr ;

	PredUnit(pr) = CurrUnit() ;
	PredNextU(pr) = UnitPredicates(CurrUnit()) ;
	UnitPredicates(CurrUnit()) = pr ;

	PredIndex(pr) = nil ;
	PredIsIndexed(pr) = FunctorArity(f) != 0 && f != listFunctor ;
	PredIndexMade(pr) = false ;
	PredIsBuiltin(pr) = false ;
	PredIsNeverBuiltin(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsImported(pr) = false ;
	PredIsTraceable(pr)	= true ;

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
			Error("%s %s: '%s",
					"A predicate which name starts with a \'$\'",
					"cannot be made visible",
					UPredNameArity(pr)) ;
	}
	else PredIsVisible(pr) = true ;
}

static void MakeNeverBuiltin(PredicatePt pr, Bool err)
{
	if( PredHasClauses(pr) ) {
		if( err )
			Error("%s %s: '%s'",
					"Every 'never_builtin' declaration must precede",
					"the corresponding predicate definition",
					UPredNameArity(pr)) ;
	}
	else PredIsNeverBuiltin(pr) = true ;
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
		  Error("Attempt to change builtin predicate '%s'", UPredNameArity(pr)) ;
		return pr ;
	}
	return NewPredicate(f) ;
}

Size NumberOfClauses(PredicatePt pr)
{
	register ClausePt cl ;
	register Size n = 0 ;
	dolist(cl, PredClauses(pr), ClauseNext(cl))
		n++ ;
	return n ;
}

CharPt PredNameArity(PredicatePt pr)
{
	sprintf(retBuffer, "%s/%d", PredName(pr), PredArity(pr)) ;
	return retBuffer ;
}

CharPt UPredNameArity(PredicatePt pr)
{
	sprintf(retBuffer, "%s:%s/%d",
		UnitName(PredUnit(pr)), PredName(pr), PredArity(pr)) ;
	return retBuffer ;
}

static void AbolishPredicate(FunctorPt f)
{
	PredicatePt pr ;
	register ClausePt cl, next ;
	
	if( (pr = FindPredicate(f)) == nil )
		return ;

	if( PredIsBuiltin(pr) )
		Error("Builtin predicate '%s' cannot be deleted", FunctorNameArity(f)) ;

	dolist(cl, PredClauses(pr), next) {
		next = ClauseNext(cl) ;
		DeleteClause(cl) ;
	}
	if( PredIndex(pr) != nil ) {
		BlockRelease(PredIndex(pr)) ;
		PredIndex(pr) = nil ;
	}

	PredIsIndexed(pr) = FunctorArity(f) != 0 && f != listFunctor ;
	PredIndexMade(pr) = false ;
	PredIsBuiltin(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsImported(pr) = false ;
	InvalidIndex(pr) ;
}

static void AbolishBuiltin(FunctorPt f)
{
	PredicatePt pr ;
	if( (pr = FindPredicate(f)) == nil )
		Error("Predicate %s is not defined", FunctorNameArity(f)) ;
	if( FunctorIsMeta(f) )
		Error("The core builtin predicate %s cannot be abolished", FunctorNameArity(f)) ;
	if( not PredIsBuiltin(pr) )
		Error("Predicate %s is not builtin", FunctorNameArity(f)) ;
	PredIsBuiltin(pr) = false ;
	AbolishPredicate(f) ;
}

static void ImportPredicate(PredicatePt pr, Pt term)
{
	if( PredIsImported(pr) ) {
		if( XTestFunctor(GetImportTerm(pr)) != XTestFunctor(term) )
			Error("Predicate '%s' was already imported from another unit",
							UPredNameArity(pr)) ;
	}
	elif( not PredIsUndefined(pr) )
		Error("Predicate '%s' cannot be imported because it was already \
						defined in this unit", UPredNameArity(pr)) ;

	BindPredicate(pr, AllocSwitchCtxCall, AllocateTermForAssert(term)) ;
	BindPredicateMoreArgs(pr, cPt(WordsOf(Environment)), DeallocProceed) ;
	PredIsImported(pr) = true ;
}

void MarkCoreBuiltins()
{
	MarkBuiltins() ;
}

void MarkBuiltins()
{
	register PredicatePt pr ;
	PredicateUpdateFlags() ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr)) {
		if( not PredIsBuiltin(pr) ) {
			PredIsBuiltin(pr) = not PredIsUndefined(pr)
								&& not PredIsNeverBuiltin(pr) ;
			MakeVisible(pr, false) ;
		}
	}

/*
	register OpPt op ;
	register IVarPt iv ;
	for( op = FirstOp() ; op != nil ; op = NextOp(op) )
		OpIsBuiltin(op) = true ;

	for( iv = FirstIVar() ; iv != nil ; iv = NextIVar(iv) )
		IVarIsBuiltin(iv) = true ;
*/	
}

void PredicateUpdateFlags()
{
	register PredicatePt pr ;
	if( (pr = FindPredicateByName("->", 2)) == nil || NumberOfClauses(pr) != 2 )
		return ;
	if( compatibleIfThen_flag )
		BindPredicate(pr, LocalJump, cPt(ClauseCodeSkipHeader(PredClauses(pr)))) ;
	else
		BindPredicate(pr, LocalJump, cPt(ClauseCode(PredClauses(pr)))) ;
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
	BindPredicate(pr, InstEncode(cProc), Proceed) ;
	return pr ;
}

PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, Proc cProc)
{
	PredicatePt pr ;
	Pt h[2] ;
	ClausePt cl1, cl2 ;
	Char buff[1 K] ;

	strcpy(buff, "$$$$_") ;
	strcat(buff, name) ;
	InstallCBuiltinPred(buff, arity, cProc) ;
	pr = InstallCBuiltinPred(name, arity, cProc) ;

	PredIsIndexed(pr) = false ;

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
			return UPredNameArity(pr) ;
	if( p == CallVar || p == ExecuteVar )
		return "call/1" ;
	return nil ;
}

static void ListCBuiltinPreds()
{
	register PredicatePt pr ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredIsC(pr) )
			Write("%s\n", UPredNameArity(pr)) ;
}


/* CXPROLOG C'BUILTINS */

static Pt SourceUnavailableTerm(FunctorPt f)
{
	static Pt tSUT = nil ;
	static FunctorPt *fSUT ;
	if( tSUT == nil ) {
		HSave() ;
		tSUT = AllocateTermForAssign(
				MakeBinStruct(
					neckFunctor,
					MakeCleanStruct(LookupFunctorByName("u",maxArity)),
					MakeAtom("SOURCE_UNAVAILABLE"))) ;
		fSUT = (FunctorPt *)(XPt(tSUT) + 3) ;
		HRestore() ;
	}
	*fSUT = f ;
	return tSUT ;
}

static void PNDRepeat()
{
	JumpNext()
}

static void PNDClause()
{
	ClausePt cl ;
	Bool b ;
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
		if( (clTerm = ClauseSource(cl)) == nil )
			clTerm = SourceUnavailableTerm(PredFunctor(ClauseOwner(cl))) ;
		SplitClauseTerm(clTerm, &head, &body) ;
		TrailSave() ;
		b = Unify(X0, head) && Unify(X1, body) ;
		TrailRestore() ;
		if( b ) break ;
		cl = ClauseNext(cl) ;
	}

	A(2) = cPt(ClauseNext(cl)) ;
	SplitClauseTerm(ZPushTerm(clTerm), &head, &body) ;
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
	AbolishPredicate(XTestFunctor2(X0,X1)) ;
	JumpNext()
}

static void PAbolishBuiltin()
{
	AbolishBuiltin(XTestFunctor2(X0,X1)) ;
	JumpNext()
}

static void PNDRetract()
{
	PredicatePt pr ;
	ClausePt cl ;
	Bool b ;
	Pt head, body, t ;

	if( A(1) == tNilAtom ) {
		SplitClauseTerm(X0, &head, &body) ;
		pr = FindPredicate(XTestFunctor(head)) ;
		if( pr != nil && PredIsBuiltin(pr) )
			Error("Atempt to retract clause from builtin predicate '%s'", UPredNameArity(pr)) ;
		if( pr == nil || PredIsUndefined(pr) )
			Jump(DiscardAndFail) ;
		cl = PredClauses(pr) ;
	}
	else cl = cClausePt(A(1)) ;
	
	for(;;) {
		if( cl == nil || ClauseSource(cl) == nil ) Jump(DiscardAndFail) ;
		TrailSave() ;
		b = Unify(X0, ClauseSource(cl)) ;
		TrailRestore() ;
		if( b ) break ;
		cl = ClauseNext(cl) ;
	}

	A(1) = cPt(ClauseNext(cl)) ;
	t = ZPushTerm(ClauseSource(cl)) ;
	if( Unify(X0, t) ) {
		DeleteClause(cl) ;
		JumpNext()
	}
	Error("Internal error in DoRetract") ;	
}

static void PNDCurrentPredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom
			? UnitPredicates(CurrUnit())
			: PredNextU(cPredicatePt(A(1))) ;
	dolist(pr, pr, PredNextU(pr))
		if( PredHasClauses(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanStruct(PredFunctor(pr))) ) JumpNext()
	DoFail()
}

static void PNDVisiblePredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom
			? UnitPredicates(CurrUnit())
			: PredNextU(cPredicatePt(A(1))) ;
	dolist(pr, pr, PredNextU(pr))
		if( PredIsVisible(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanStruct(PredFunctor(pr))) ) JumpNext()
	DoFail()
}

static void PNDImportedPredicate()
{
	PredicatePt pr =
		A(2) == tNilAtom
			? UnitPredicates(CurrUnit())
			: PredNextU(cPredicatePt(A(2))) ;
	dolist(pr, pr, PredNextU(pr))
		if( PredIsImported(pr) ) break ;
	A(2) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanStruct(PredFunctor(pr))) ) {
		Pt t = ZPushTerm(GetImportTerm(pr)) ;
		if( Unify(X1, t) ) JumpNext()
	}
	DoFail()
}

static void PNDBuiltinPredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom
			? UnitPredicates(builtinUnit)
			: PredNextU(cPredicatePt(A(1))) ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanStruct(PredFunctor(pr))) ) JumpNext()
	DoFail()
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

static void PNeverBuiltin()
{
	MakeNeverBuiltin(LookupPredicate(XTestFunctor2(X0, X1), true), true) ;
	JumpNext()
}

static void PCheckCurrentUnitImports()
{
	register PredicatePt pr, pr2 ;
	FunctorPt f2 ;
	register UnitPt u = CurrUnit(), u2 ;
	Bool found ;

	dolist(pr, UnitPredicates(u), PredNextU(pr))
		if( PredIsImported(pr) ) {
			f2 = XTestFunctor(GetImportTerm(pr)) ;
			u2 = FindUnit(FunctorAtom(f2)) ;
			if( u2 == nil || UnitArity(u2) != FunctorArity(f2) )
				Warning("Predicate '%s' is imported from nonexistent unit '%s'",
							UPredNameArity(pr), FunctorNameArity(f2)) ;
			
			found = false ;
			dolist(pr2, FunctorPreds(PredFunctor(pr)), PredNextF(pr2))
				if( PredUnit(pr2) == u2 ) {
					found = true ;
					break ;
				}			
			if( (found && not PredIsVisible(pr2)) || not found )
				Warning("Imported predicate '%s' is not visible in unit '%s'",
							UPredNameArity(pr), FunctorNameArity(f2)) ;
		}
	JumpNext()
}

static void PCheckMissing()
{
	register UnitPt u ;
	register PredicatePt pr ;
	
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredIsUndefined(pr) )
			Warning("Predicate '%s' used but undefined in unit '%s'",
				UPredNameArity(pr), FunctorNameArity(UnitFunctor(builtinUnit))) ;

	dolist(u, FirstUnit(), NextUnit(u))
		dolist(pr, UnitPredicates(u), PredNextU(pr))
			if( PredIsUndefined(pr) )
				Warning("Predicate '%s' used but missing in unit '%s'",
					UPredNameArity(pr), FunctorNameArity(UnitFunctor(u))) ;
	JumpNext()
}

static void PEnterUserMode()
{
	if( UnitIsReserved(builtinUnit) )
		FatalError("Cannot be called twice") ;
	AbolishBuiltin(LookupFunctorByName("abolish_builtin", 2)) ;
	MarkBuiltins() ;
	UserModeInstructions() ;
	UnitIsReserved(builtinUnit) = true ;
	JumpNext()
}

static void PBuiltins()
{
	PredicatePt pr ;
	Size i = 0 ;
	VersionShow() ;
	Write("Builtins:\n") ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredName(pr)[0] != '$' ) {
			if( i % 3 != 2 ) {
				CharPt str = PredNameArity(pr) ;
				Size len = strlen(str) ;
				Write("  %s", str) ;
				for( ; len < 26 ; len++ )
					Write(" ") ;
			}
			else 
				Write("%s\n", PredNameArity(pr)) ;
			i++ ;
		}
	JumpNext()
}

void PredicatesInit()
{
	InstallNDeterCBuiltinPred("repeat", 0, PNDRepeat) ;
	InstallNDeterCBuiltinPred("clause", 2, PNDClause) ;
	InstallCBuiltinPred("asserta", 1, PAsserta) ;
	InstallCBuiltinPred("assertz", 1, PAssertz) ;
	InstallCBuiltinPred("assert", 1, PAssertz) ;
	InstallCBuiltinPred("abolish", 2, PAbolish) ;
	InstallCBuiltinPred("abolish_builtin", 2, PAbolishBuiltin) ;
	InstallNDeterCBuiltinPred("retract", 1, PNDRetract) ;
	InstallNDeterCBuiltinPred("current_predicate", 1, PNDCurrentPredicate) ;
	InstallNDeterCBuiltinPred("visible_predicate", 1, PNDVisiblePredicate) ;
	InstallNDeterCBuiltinPred("imported_predicate", 2, PNDImportedPredicate) ;
	InstallNDeterCBuiltinPred("builtin_predicate", 1, PNDBuiltinPredicate) ;
	InstallNDeterCBuiltinPred("system_predicate", 1, PNDBuiltinPredicate) ;
	InstallCBuiltinPred("clist", 0, PListCBuiltinPreds) ;
	InstallCBuiltinPred("atlist", 0, PPrintAtomTable) ;
	InstallCBuiltinPred("visible", 2, PVisible) ;
	InstallCBuiltinPred("import", 3, PImport) ;
	InstallCBuiltinPred("never_builtin", 2, PNeverBuiltin) ;
	InstallCBuiltinPred("$$_check_curr_unit_imports", 0, PCheckCurrentUnitImports) ;
	InstallCBuiltinPred("check_missing", 0, PCheckMissing) ;
	InstallCBuiltinPred("$$_enter_user_mode", 0, PEnterUserMode) ;
	InstallCBuiltinPred("builtins", 0, PBuiltins) ;
}
