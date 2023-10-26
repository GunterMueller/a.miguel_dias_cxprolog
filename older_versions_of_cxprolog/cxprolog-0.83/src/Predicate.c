/*
 *   This file is part of the CxProlog system

 *   Predicate.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

/* The builtinUnit contains:
	1 - Builtin preds. All of them are visible.
    2 - Empty non-visible aux preds, required by the debugger. */ 

#define PredIsBuiltinAux(pr)	(PredName(pr)[0] == '$')

static PredicatePt NewPredicate(FunctorPt f)
{
	register PredicatePt pr ;

	if( FunctorArity(f) > maxPredArity )
		DatabaseError("Highest predicate arity (%d) exceeded on predicate '%s/%d'",
							maxPredArity, FunctorName(f), FunctorArity(f)) ;

	pr = PermBlockAllocate(WordsOf(Predicate)) ;
	PredClauses(pr) = nil ;

	PredFunctor(pr) = f ;
	PredNextF(pr) = FunctorPreds(f) ;
	FunctorPreds(f) = pr ;

	PredUnit(pr) = CurrUnit() ;
	PredNextU(pr) = UnitPredicates(CurrUnit()) ;
	UnitPredicates(CurrUnit()) = pr ;

	PredIndex(pr) = nil ;
	PredIsIndexable(pr) = FunctorArity(f) != 0 && f != listFunctor ;
	PredIndexMade(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsPermanent(pr) = false ;
	PredIsTraceable(pr)	= true ;
	PredIsNoCurrUnit(pr) = false ;
	PredIsMutableBuiltin(pr) = false ;
	PredReconsulted(pr) = reconsulting_flag > 0 ;
	
	if( CurrUnit() == builtinUnit )
		if( Booting() ) {
			PredIsBuiltin(pr) = true ;
			PredIsVisible(pr) = true ; /* All builtins are visible */
		}
		else /* Cannot generate error yet because the debugger requires
				the creation of an empty predicate, here. */
			PredIsNoCurrUnit(pr) = true ;

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
	if( FunctorIsBuiltin(f) )
		return FunctorPreds(f) ;
	else {
		register PredicatePt pr ;
		register UnitPt u = CurrUnit() ;
		dolist(pr, FunctorPreds(f), PredNextF(pr))
			if( PredUnit(pr) == u )
				return pr ;
		return nil ;
	}
}

PredicatePt FindPredicateByName(CharPt n, int a)
{
	return FindPredicate(LookupFunctorByName(n, a)) ;
}

PredicatePt LookupPredicate(FunctorPt f)
{
	PredicatePt pr ;
	if( (pr = FindPredicate(f)) != nil )
		return pr ;
	return NewPredicate(f) ;
}

PredicatePt LookupPredicateByName(CharPt n, int a)
{
	return LookupPredicate(LookupFunctorByName(n, a)) ;
}

PredicatePt CheckPredicate(FunctorPt f)
{
	PredicatePt pr ;
	if( (pr = FindPredicate(f)) == nil || PredIsUndefined(pr) ) 
		FatalError("Missing core predicate '%s'", FunctorNameArity(f)) ;
	return pr ;
}

PredicatePt CheckPredicateByName(CharPt n, int a)
{
	return CheckPredicate(LookupFunctorByName(n, a)) ;
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
	if( PredUnit(pr) == builtinUnit )
		return PredNameArity(pr) ;
	sprintf(retBuffer, "%s:%s/%d",
		UnitName(PredUnit(pr)), PredName(pr), PredArity(pr)) ;
	return retBuffer ;
}

void MarkBuiltinsPermanent()
{
	register PredicatePt pr ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( not PredIsMutableBuiltin(pr) )
			PredIsPermanent(pr) = true ;
}

void AbolishPredicate(PredicatePt pr)
{
	register ClausePt cl, next ;
	
	if( PredIsPermanent(pr) )
		DatabaseError("Permanent predicate '%s' cannot be deleted",
											PredNameArity(pr)) ;

	dolist(cl, PredClauses(pr), next) {
		next = ClauseNext(cl) ;
		DeleteClause(cl) ;
	}
	if( PredIndex(pr) != nil ) {
		BlockRelease(PredIndex(pr)) ;
		PredIndex(pr) = nil ;
	}

	PredIsIndexable(pr) = PredArity(pr) != 0
							&& PredFunctor(pr) != listFunctor ;
	PredIndexMade(pr) = false ;
	PredIsBuiltin(pr) = PredIsMutableBuiltin(pr) ;
	PredIsVisible(pr) = PredIsMutableBuiltin(pr) ;
	InvalidIndex(pr) ;
}

void CompatibleIfThenUpdateFlags(int newValue)
{
	register PredicatePt pr ;
	compatibleIfThen_flag = newValue ;
	if( (pr = FindPredicateByName("->", 2)) == nil
						|| NumberOfClauses(pr) != 2 ) return ;
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
		DatabaseError("All C predicates must be installed in the 'builtin unit' (%s/%d)",
				name, arity) ;

	pr = LookupPredicate(f) ;
	if( not PredIsUndefined(pr) )
		FatalError("Cannot install C predicate. Another one with the same \
					name and arity was already installed (%s/%d)", name, arity) ;
	BindPredicate(pr, InstEncode(cProc), Proceed) ;
	return pr ;
}

PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, Proc cProc)
{
	PredicatePt pr ;
	Pt h[2] ;
	ClausePt cl1, cl2 ;
	Str1000 buff ;

	sprintf(buff, "$$$$_%s", name) ;
/* Only registers the "$$$$_..." name as a C predicate name */
	InstallCBuiltinPred(buff, arity, cProc) ;
	pr = InstallCBuiltinPred(name, arity, cProc) ;

	PredIsIndexable(pr) = false ;

	h[0] = cPt(cProc) ;
	h[1] = Proceed ; 
	cl1 = InstallClause(pr, h, 2, nil, nil, true) ;
	ClauseArity(cl1) += 1 ;
	cl2 = InstallClause(pr, h, 2, nil, nil, true) ;
	ClauseArity(cl2) += 1 ;
	ClauseInst(cl2) = RetryMeElse ;
	ClauseNext(cl2) = cl2 ;

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
	return nil ;
}


/* CXPROLOG C'BUILTINS */

static Pt SourceUnavailableTerm(FunctorPt f)
{
	static Pt tSUT = nil ;
	static Pt aSUT = nil ;
	if( tSUT == nil ) {
		HSave() ;
		tSUT = AllocateTermForAssign(
				MakeBinStruct(
					neckFunctor,
					MakeCleanStruct(LookupFunctorByName("u",maxArity)),
					MakeAtom("$$_source_unavailable"))) ;
		aSUT = AllocateTermForAssign(
				MakeBinStruct(
					neckFunctor,
					tNilAtom,
					MakeAtom("$$_source_unavailable"))) ;
		HRestore() ;
	}
	if( FunctorArity(f) > 0 ) {
		XStructFunctor((XStructArg(tSUT,0))) = f ;
		return tSUT ;
	}
	else {
		XStructArg(aSUT,0) = TagAtom(FunctorAtom(f)) ;
		return aSUT ;
	}
}

static void PNDRepeat0()
{
	JumpNext()
}

static void PNDRepeat1()
{
	if( A(1) == tNilAtom ) {
		X0 = MakeInt(XTestNat(X0)) ;
		A(1) = MakeInt(0) ;
	}
	if( X0 == A(1) ) Jump(DiscardAndFail)
	A(1) = MakeInt(XInt(A(1))+1) ;
	JumpNext()
}

static void PNDRepeat2()
{
	if( A(2) == tNilAtom ) {
		X0 = MakeInt(XTestNat(X0)) ;
		XTestVar(X1) ;
		A(2) = MakeInt(0) ;
	}
	if( X0 == A(2) ) Jump(DiscardAndFail)
	A(2) = MakeInt(XInt(A(2))+1) ;
	if( UnifyWithNumber(X1, A(2)) ) JumpNext()
	InternalError("PNDRepeat2") ;
}

static void PNDClause()
{
	ClausePt cl ;
	Pt clTerm, clParts[2] ;

	if( A(2) == tNilAtom ) {
		PredicatePt pr = FindPredicate(XTestFunctor(X0)) ;
		if( pr == nil || PredIsUndefined(pr) )
			Jump(DiscardAndFail)
		else
			cl = PredClauses(pr) ;
	}
	else cl = cClausePt(A(2)) ;
	
	for(;;) {
		if( cl == nil || IsClauseLoop(cl) )
			Jump(DiscardAndFail) ;
		if( (clTerm = ClauseSource(cl)) == nil )
			clTerm = SourceUnavailableTerm(PredFunctor(ClauseOwner(cl))) ;
		SplitNeckTerm(clTerm, clParts) ;
		if( UnifiableN(&X0, clParts, 2) )
			break ;
		cl = ClauseNext(cl) ;
	}

	A(2) = cPt(ClauseNext(cl)) ;
	SplitNeckTerm(ZPushTerm(clTerm), clParts) ;
	if( UnifyN(&X0, clParts, 2) ) JumpNext()
	InternalError("PNDClause") ;	
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
		AbolishPredicate(pr) ;
	JumpNext()
}

static void PAbolishBuiltin()
{
	FunctorPt f = XTestFunctor2(X0,X1) ;
	PredicatePt pr ;
	if( not Booting() )
		DatabaseError("Only works at boot time") ;
	if( (pr = FindPredicate(f)) == nil )
		DatabaseError("Predicate '%s' not defined", FunctorNameArity(f)) ;
	if( PredIsMeta(pr) )
		DatabaseError("The core builtin predicate %s cannot be abolished",
											FunctorNameArity(f)) ;
	if( not PredIsBuiltin(pr) )
		DatabaseError("Predicate '%s' not a builtin", FunctorNameArity(f)) ;
	PredIsPermanent(pr) = false ; /* Make it not permanent */
	AbolishPredicate(pr) ;
	PredIsBuiltin(pr) = true ;
	PredIsVisible(pr) = true ;
	JumpNext()
}

static void PMutableBuiltin()
{
	PredicatePt pr = LookupPredicate(XTestFunctor2(X0, X1)) ;
	if( not Booting() )
		DatabaseError("Only works at boot time") ;
	if( not PredIsUndefined(pr) )
		Error("%s %s: '%s'",
				"A 'mutable_builtin/2' command must precede",
				"the corresponding predicate definition",
				UPredNameArity(pr)) ;
	PredIsMutableBuiltin(pr) = true ;
	JumpNext()
}

static void PNDRetract()
{
	PredicatePt pr ;
	ClausePt cl ;
	Pt clParts[2], t ;

	if( A(1) == tNilAtom ) {
		SplitNeckTerm(X0, clParts) ;
		pr = FindPredicate(XTestFunctor(clParts[0])) ;
		if( pr != nil && PredIsPermanent(pr) )
			DatabaseError("Attempt to retract clause from permanent predicate '%s'",
											UPredNameArity(pr)) ;
		if( pr == nil || PredIsUndefined(pr) )
			Jump(DiscardAndFail) ;
		cl = PredClauses(pr) ;
	}
	else cl = cClausePt(A(1)) ;
	
	for(;;) {
		if( cl == nil || ClauseSource(cl) == nil )
			Jump(DiscardAndFail) ;
		if( Unifiable(X0, ClauseSource(cl)) )
			break ;
		cl = ClauseNext(cl) ;
	}

	A(1) = cPt(ClauseNext(cl)) ;
	t = ZPushTerm(ClauseSource(cl)) ;
	if( Unify(X0, t) ) {
		DeleteClause(cl) ;
		JumpNext()
	}
	InternalError("PNDRetract") ;	
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
	dolist(pr, pr, PredNextU(pr))
		if( PredIsBuiltin(pr) && not PredIsBuiltinAux(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanStruct(PredFunctor(pr))) ) JumpNext()
	DoFail()
}

static void PListCBuiltinPreds()
{
	register PredicatePt pr ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredIsC(pr) )
			Write("%s\n", UPredNameArity(pr)) ;
	JumpNext()
}

static void PPrintAtomTable()
{
	PrintAtomTable() ;
	JumpNext()
}

static Size PVisibleAux(FunctorPt f, Pt x)
{
	PredicatePt pr = LookupPredicate(f) ;
	if( PredName(pr)[0] == '$' )
		DatabaseError("%s %s: '%s",
				"A predicate which name starts with a \'$\'",
				"cannot be made visible", UPredNameArity(pr)) ;
	if( reconsulting_flag > 0 && not PredReconsulted(pr) ) {
		AbolishPredicate(pr) ;
		PredReconsulted(pr) = true ;
	}
	PredIsVisible(pr) = true ;
	return 0 ;
}
static void PVisible()
{
	Spec(X0, PVisibleAux, tNilAtom) ;
	JumpNext()
}

static Size PImportAux(FunctorPt f, Pt from)
{
	PredicatePt pr = LookupPredicate(f) ;

	if( reconsulting_flag > 0 && not PredReconsulted(pr) ) {
		AbolishPredicate(pr) ;
		PredReconsulted(pr) = true ;
	}

	if( PredIsBuiltin(pr) )
		DatabaseError("Predicate '%s' cannot be imported because is a builtin",
								PredNameArity(pr)) ;
        
	if( not PredIsUndefined(pr) )
		if( PredIsImported(pr) )
			DatabaseError("Predicate '%s' cannot be imported twice into the same unit",
								UPredNameArity(pr)) ;
		else
			DatabaseError("Predicate '%s' cannot be imported because %s",
								UPredNameArity(pr), "is already defined in the unit") ;

	if( PredIsNoCurrUnit(pr) )
		DatabaseError("No current unit when importing predicate '%s'",
							PredNameArity(pr)) ;

	BindPredicate(pr, AllocSwitchCtxCall, AllocateTermForAssert(from)) ;
	BindPredicateMoreArgs(pr, cPt(WordsOf(Environment)), DeallocProceed) ;
	return 0 ;
}
static void PImport()
{
	Spec(X0, PImportAux, X1) ;
	JumpNext()
}

static void PCheckPredicate()
{
	CheckPredicate(XTestFunctor2(X0, X1)) ;
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
			if( f2 == unitParamFunctor ) continue ; /* Is a var import */
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
	if( not Booting() )
		FatalError("Cannot be activated twice") ;
	CompatibleIfThenUpdateFlags(compatibleIfThen_flag) ;
	MarkBuiltinsPermanent() ;
	UserModeInstructions() ;
	Booting() = false ;
	JumpNext()
}

static void PEnterReconsulting()
{
	if( reconsulting_flag++ == 0 )
		NoReconsultAll() ;
	JumpNext()
}

static void PExitReconsulting()
{
	if( reconsulting_flag-- == 0 )
		Error("Non closing '$$_exit_reconsulting'") ;
	JumpNext()
}

static void PIsReconsulting()
{
	if( reconsulting_flag > 0 ) JumpNext()
	DoFail()
}

static void PBuiltins()
{
	PredicatePt pr ;
	Size i = 0 ;
	VersionShow() ;
	Write("Builtins:\n") ;
	dolist(pr, UnitPredicates(builtinUnit), PredNextU(pr))
		if( PredIsBuiltin(pr) && not PredIsBuiltinAux(pr) ) {
			if( i % 3 != 2 ) {
				CharPt str = PredNameArity(pr) ;
				Size len = strlen(str) ;
				Write("  %s", str) ;
				for( ; len < 25 ; len++ )
					Write(" ") ;
			}
			else 
				Write("%s\n", PredNameArity(pr)) ;
			i++ ;
		}
	if( i % 3 != 0 ) Write("\n") ;
	JumpNext()
}

void PredicatesInit()
{
	PredIsMutableBuiltin(LookupPredicateByName("$$_top_call", 1)) = true ;

	InstallNDeterCBuiltinPred("repeat", 0, PNDRepeat0) ;
	InstallNDeterCBuiltinPred("repeat", 1, PNDRepeat1) ;
	InstallNDeterCBuiltinPred("repeat", 2, PNDRepeat2) ;
	InstallNDeterCBuiltinPred("clause", 2, PNDClause) ;
	InstallCBuiltinPred("asserta", 1, PAsserta) ;
	InstallCBuiltinPred("assertz", 1, PAssertz) ;
	InstallCBuiltinPred("assert", 1, PAssertz) ;
	InstallCBuiltinPred("abolish", 2, PAbolish) ;
	InstallCBuiltinPred("abolish_builtin", 2, PAbolishBuiltin) ;
	InstallCBuiltinPred("mutable_builtin", 2, PMutableBuiltin) ;
	InstallNDeterCBuiltinPred("retract", 1, PNDRetract) ;
	InstallNDeterCBuiltinPred("current_predicate", 1, PNDCurrentPredicate) ;
	InstallNDeterCBuiltinPred("visible_predicate", 1, PNDVisiblePredicate) ;
	InstallNDeterCBuiltinPred("imported_predicate", 2, PNDImportedPredicate) ;
	InstallNDeterCBuiltinPred("builtin_predicate", 1, PNDBuiltinPredicate) ;
	InstallNDeterCBuiltinPred("system_predicate", 1, PNDBuiltinPredicate) ;

	InstallCBuiltinPred("$$_visible", 1, PVisible) ;
	InstallCBuiltinPred("$$_import", 2, PImport) ;
	InstallCBuiltinPred("$$_check_predicate", 2, PCheckPredicate) ;
	InstallCBuiltinPred("$$_check_curr_unit_imports", 0, PCheckCurrentUnitImports) ;
	InstallCBuiltinPred("check_missing", 0, PCheckMissing) ;
	InstallCBuiltinPred("$$_enter_user_mode", 0, PEnterUserMode) ;
	InstallCBuiltinPred("$$_enter_reconsulting", 0, PEnterReconsulting) ;
	InstallCBuiltinPred("$$_exit_reconsulting", 0, PExitReconsulting) ;
	InstallCBuiltinPred("$$_is_reconsulting", 0, PIsReconsulting) ;
	InstallCBuiltinPred("builtins", 0, PBuiltins) ;
	InstallCBuiltinPred("clist", 0, PListCBuiltinPreds) ;
	InstallCBuiltinPred("atlist", 0, PPrintAtomTable) ;
}
