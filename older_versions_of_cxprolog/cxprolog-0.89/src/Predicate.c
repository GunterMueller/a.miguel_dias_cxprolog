/*
 *   This file is part of the CxProlog system

 *   Predicate.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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
    2 - Empty non-visible aux preds, required by the debugger: PredIsNoCurrUnit(pr) */ 

#define PredIsBuiltinAux(pr)	(PredName(pr)[0] == '$')

static PredicatePt NewPredicate(FunctorPt f)
{
	register PredicatePt pr ;

	if( FunctorArity(f) > maxPredArity )
		DatabaseError("Highest predicate arity (%d) exceeded on predicate '%s/%d'",
							maxPredArity, FunctorName(f), FunctorArity(f)) ;

	pr = Allocate(WordsOf(Predicate)) ;
	PredClauses(pr) = nil ;

	PredFunctor(pr) = f ;
	PredNextF(pr) = FunctorPreds(f) ;
	FunctorPreds(f) = pr ;

	PredUnit(pr) = CurrUnit() ;
	PredNextU(pr) = UnitPreds(CurrUnit()) ;
	UnitPreds(CurrUnit()) = pr ;

	PredIndex(pr) = nil ;
	PredIsIndexable(pr) = FunctorArity(f) != 0 && f != listFunctor ;
	PredIndexMade(pr) = false ;
	PredIsVisible(pr) = false ;
	PredIsPermanent(pr) = false ;
	PredIsTraceable(pr)	= true ;
	PredIsNoCurrUnit(pr) = false ;
	PredIsMutableBuiltin(pr) = false ;
	PredReconsulted(pr) = reconsulting_flag > 0 ;
	
	if( CurrUnit() == builtinUnit ) {
		if( Booting() ) {
			PredIsBuiltin(pr) = true ;
			PredIsVisible(pr) = true ; /* All builtins are visible */
		}
		else /* Cannot generate error yet because the debugger requires
				the creation of an empty predicate, here. */
			PredIsNoCurrUnit(pr) = true ;
	}

	InvalidIndex(pr) ;
	return pr ;
}

void BindPredicate(PredicatePt pr, Pt inst, Pt arg0)
{
	PredStartInst(pr) = inst ;
	PredStartInstArgs(pr)[0] = arg0 ;
}

void BindPredicate2(PredicatePt pr, Pt inst, Pt arg0, Pt arg1, Pt arg2)
{
	PredStartInst(pr) = inst ;
	PredStartInstArgs(pr)[0] = arg0 ;
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
		doseq(pr, FunctorPreds(f), PredNextF(pr))
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
	doseq(cl, PredClauses(pr), ClauseNext(cl))
		n++ ;
	return n ;
}

CharPt PredNameArity(PredicatePt pr)
{
	if( PredUnit(pr) == builtinUnit )
		sprintf(retString, "%s/%d", PredName(pr), PredArity(pr)) ;
	else
		sprintf(retString, "%s:%s/%d", UnitName(PredUnit(pr)),
										PredName(pr), PredArity(pr)) ;
	return retString ;
}

Pt MakePredSlashTerm(PredicatePt pr)
{
	return MakeBinStruct(slashFunctor,
					TagAtom(PredAtom(pr)),
					MakeInt(PredArity(pr))) ;
}

void MarkBuiltinsPermanent()
{
	register PredicatePt pr ;
	doseq(pr, UnitPreds(builtinUnit), PredNextU(pr))
		if( !PredIsMutableBuiltin(pr) )
			PredIsPermanent(pr) = true ;
}

void AbolishPredicate(PredicatePt pr)
{
	register ClausePt cl, next ;
	
	if( PredIsPermanent(pr) )
		DatabaseError("Permanent predicate '%s' cannot be deleted",
											PredNameArity(pr)) ;

	doseq(cl, PredClauses(pr), next) {
		next = ClauseNext(cl) ;
		DeleteClause(cl) ;
	}
	if( PredIndex(pr) != nil ) {
		Release(PredIndex(pr)) ;
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
	if( !PredIsUndefined(pr) )
		FatalError("C predicate already installed (%s/%d)", name, arity) ;
	BindPredicate(pr, InstEncode(cProc), Proceed) ;
	return pr ;
}

PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, Proc cProc)
{
	PredicatePt pr ;
	Pt h[2] ;
	ClausePt cl1, cl2 ;
	Str1K buff ;

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

	BindPredicate2(pr, PutNil, cPt(OutTemp(arity)), LocalJump, cPt(ClauseCode(cl1))) ;
	return pr ;
}

PredicatePt FindCPredByInst(Pt p)
{
    register PredicatePt pr ;
    doseq(pr, UnitPreds(builtinUnit), PredNextU(pr))
        if( PredStartInst(pr) == p )
            return pr ;
    return nil ;
}

PredicatePt CurrCPred()
{
	if( (P[-1] == CallVar || P[-1] == ExecuteVar) )
		return LookupPredicateByName("call", 1) ;
	else
		return FindCPredByInst(P[-1]) ;
}

CharPt CurrCPredNameArity()
{
    register PredicatePt pr = CurrCPred() ;
	return pr == nil ? cCharPt("") : PredNameArity(pr) ;
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
	SplitNeckTerm(ZPushTerm(clTerm), clParts) ; /* stacks may grow */
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
	if( !Booting() )
		DatabaseError("This predicate only works at boot time") ;
	if( (pr = FindPredicate(f)) == nil )
		DatabaseError("Predicate '%s' not defined", FunctorNameArity(f)) ;
	if( PredIsMeta(pr) )
		DatabaseError("The core builtin predicate %s cannot be abolished",
											FunctorNameArity(f)) ;
	if( !PredIsBuiltin(pr) )
		DatabaseError("Predicate '%s' not a builtin", FunctorNameArity(f)) ;
	PredIsPermanent(pr) = false ; /* Make it not permanent */
	AbolishPredicate(pr) ;
	PredIsBuiltin(pr) = true ;
	PredIsVisible(pr) = true ;
	JumpNext()
}

static void PRenameBuiltin()
{
	FunctorPt f = XTestFunctor2(X0,X1) ;
	FunctorPt newf = XTestFunctor2(X2,X1) ; 
	PredicatePt pr, pr2 ;
	if( !Booting() )
		DatabaseError("This predicate only works at boot time") ;
	if( (pr = FindPredicate(f)) == nil )
		DatabaseError("Predicate '%s' not defined", FunctorNameArity(f)) ;
	if( PredIsMeta(pr) )
		DatabaseError("The core builtin predicate %s cannot be renamed",
											FunctorNameArity(f)) ;
	if( !PredIsBuiltin(pr) )
		DatabaseError("Predicate '%s' not a builtin", FunctorNameArity(f)) ;

/* unlink */
	if( FunctorPreds(f) == pr )
		FunctorPreds(f) = PredNextF(pr) ;
	else
		doseq(pr2, FunctorPreds(f), PredNextF(pr2))
			if( PredNextF(pr2) == pr ) {
				PredNextF(pr2) = PredNextF(pr) ;
				break ;
			}

/* change & link */

	PredFunctor(pr) = newf ;
	PredNextF(pr) = FunctorPreds(newf) ;
	FunctorPreds(newf) = pr ;
	PredIsBuiltin(pr) = true ;	
}

static void PMutableBuiltin()
{
	PredicatePt pr = LookupPredicate(XTestFunctor2(X0, X1)) ;
	if( !Booting() )
		DatabaseError("Only works at boot time") ;
	if( !PredIsUndefined(pr) )
		Error("%s %s: '%s'",
				"A 'mutable_builtin/2' command must precede",
				"the corresponding predicate definition",
				PredNameArity(pr)) ;
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
											PredNameArity(pr)) ;
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
	t = ZPushTerm(ClauseSource(cl)) ; /* stacks may grow */
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
			? UnitPreds(CurrUnit())
			: PredNextU(cPredicatePt(A(1))) ;
	doseq(pr, pr, PredNextU(pr))
		if( PredHasClauses(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	MustBe( Unify(X0, MakeCleanStruct(PredFunctor(pr))) )
}

static void PNDVisiblePredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom
			? UnitPreds(CurrUnit())
			: PredNextU(cPredicatePt(A(1))) ;
	doseq(pr, pr, PredNextU(pr))
		if( PredIsVisible(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	MustBe( Unify(X0, MakeCleanStruct(PredFunctor(pr))) )
}

static void PNDImportedPredicate()
{
	Pt t ;
	PredicatePt pr =
		A(2) == tNilAtom
			? UnitPreds(CurrUnit())
			: PredNextU(cPredicatePt(A(2))) ;
	doseq(pr, pr, PredNextU(pr))
		if( PredIsImported(pr) ) break ;
	A(2) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	Ensure( Unify(X0, MakeCleanStruct(PredFunctor(pr))) )
	t = ZPushTerm(GetImportTerm(pr)) ; /* stacks may grow */
	MustBe( Unify(X1, t) )
}

static void PNDBuiltinPredicate()
{
	PredicatePt pr =
		A(1) == tNilAtom
			? UnitPreds(builtinUnit)
			: PredNextU(cPredicatePt(A(1))) ;
	doseq(pr, pr, PredNextU(pr))
		if( PredIsBuiltin(pr) && !PredIsBuiltinAux(pr) ) break ;
	A(1) = cPt(pr) ;
	if( pr == nil ) Jump(DiscardAndFail) ;
	MustBe( Unify(X0, MakeCleanStruct(PredFunctor(pr))) )
}

static void PPrintAtomTable()
{
	PrintAtomTable() ;
	JumpNext()
}

static Size PVisibleAux(FunctorPt f)
{
	PredicatePt pr = LookupPredicate(f) ;
	if( PredName(pr)[0] == '$' )
		DatabaseError("%s %s: '%s",
				"A predicate which name starts with a \'$\'",
				"cannot be made visible", PredNameArity(pr)) ;
	if( reconsulting_flag > 0 && !PredReconsulted(pr) ) {
		AbolishPredicate(pr) ;
		PredReconsulted(pr) = true ;
	}
	PredIsVisible(pr) = true ;
	return 0 ;
}
static void PVisible()
{
	ForEachInSpec(X0, PVisibleAux) ;
	JumpNext()
}

static Pt importUnitAux ;
static Size PImportAux(FunctorPt f) // Import unit in importUnitAux
{
	PredicatePt pr = LookupPredicate(f) ;

	if( reconsulting_flag > 0 && !PredReconsulted(pr) ) {
		AbolishPredicate(pr) ;
		PredReconsulted(pr) = true ;
	}

	if( PredIsBuiltin(pr) )
		DatabaseError("Predicate '%s' cannot be imported because is a builtin",
								PredNameArity(pr)) ;
        
	if( !PredIsUndefined(pr) ) {
		if( PredIsImported(pr) )
			DatabaseError("Predicate '%s' cannot be imported twice into the same unit",
								PredNameArity(pr)) ;
		else
			DatabaseError("Predicate '%s' cannot be imported because %s",
								PredNameArity(pr), "is already defined in the unit") ;
	}

	if( PredIsNoCurrUnit(pr) )
		DatabaseError("No current unit when importing predicate '%s'",
							PredNameArity(pr)) ;

	BindPredicate(pr, Import, importUnitAux) ;
	return 0 ;
}
static void PImport()
{
	importUnitAux = AllocateTermForAssert(X1) ;
	ForEachInSpec(X0, PImportAux) ;
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

	doseq(pr, UnitPreds(u), PredNextU(pr))
		if( PredIsImported(pr) ) {
			f2 = XTestFunctor(GetImportTerm(pr)) ;
			if( f2 == unitParamFunctor ) continue ; /* Is a var import */
			u2 = AtomToUnit(FunctorAtom(f2)) ;
			if( u2 == nil || UnitArity(u2) != FunctorArity(f2) )
				Warning("Predicate '%s' is imported from nonexistent unit '%s'",
							PredNameArity(pr), FunctorNameArity(f2)) ;
			
			found = false ;
			doseq(pr2, FunctorPreds(PredFunctor(pr)), PredNextF(pr2))
				if( PredUnit(pr2) == u2 ) {
					found = true ;
					break ;
				}			
			if( (found && !PredIsVisible(pr2)) || !found )
				Warning("Imported predicate '%s' is not visible in unit '%s'",
							PredNameArity(pr), FunctorNameArity(f2)) ;
		}
	JumpNext()
}

static void PCheckMissing()
{
	register UnitPt u ;
	register PredicatePt pr ;
	doseq(u, unitList, UnitNext(u))
		doseq(pr, UnitPreds(u), PredNextU(pr))
			if( PredIsUndefined(pr) )
				Warning("Predicate '%s' used but missing in unit '%s'",
					PredNameArity(pr), FunctorNameArity(UnitFunctor(u))) ;
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
	MustBe( reconsulting_flag > 0 )
}

static void PBuiltins()
{
	PredicatePt pr ;
	Size i = 0 ;
	ShowVersion() ;
	Write("Builtins:\n") ;
	doseq(pr, UnitPreds(builtinUnit), PredNextU(pr))
		if( PredIsBuiltin(pr) && !PredIsBuiltinAux(pr) ) {
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

static void PCBuiltins()
{
	PredicatePt pr ;
	Size i = 0 ;
	ShowVersion() ;
	Write("CBuiltins:\n") ;
	doseq(pr, UnitPreds(builtinUnit), PredNextU(pr))
		if( PredIsC(pr) && !PredIsBuiltinAux(pr) ) {
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
	InstallCBuiltinPred("rename_builtin", 3, PRenameBuiltin) ;
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
	InstallCBuiltinPred("$$_enter_reconsulting", 0, PEnterReconsulting) ;
	InstallCBuiltinPred("$$_exit_reconsulting", 0, PExitReconsulting) ;
	InstallCBuiltinPred("$$_is_reconsulting", 0, PIsReconsulting) ;
	InstallCBuiltinPred("builtins", 0, PBuiltins) ;
	InstallCBuiltinPred("cbuiltins", 0, PCBuiltins) ;
	InstallCBuiltinPred("atlist", 0, PPrintAtomTable) ;
}
