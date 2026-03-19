/*
 *   This file is part of the CxProlog system

 *   Predicate.c
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

/* The systemUnit contains:
	1 - Builtin preds. All of them are visible.
    2 - Empty private predicates, required by the debugger. */ 

PredicatePt failPred ;

static void ResetPredicate(register PredicatePt pr)
{ /* invariant: Booting => (CurrUnit() == systemUnit) */
	PredIsDynamic(pr) = false ;
	PredIsLogical(pr) = false ;
	PredKeepSource(pr) = false ;
	PredIsMultifile(pr) = false ;
	PredIsBuiltin(pr) = Booting() ;
	PredIsVisible(pr) = Booting() ;
	PredIsPermanent(pr) = false ;
	PredIsTraceable(pr)	= true ;
	PredWasAbolished(pr) = false ;

	PredConsultFile(pr) = nil ;
	PredConsultGen(pr) = ConsultGen() ;

	PredClauses(pr) = nil ;
	PredIndex(pr) = nil ;
	PredIsIndexable(pr) = PredArity(pr) != 0
						&& PredFunctor(pr) != listFunctor
						&& indexParams_flag != 0 ;

	BindPredicate(pr, UndefPred, cPt(PredFunctor(pr))) ;
}

static PredicatePt NewPredicate(FunctorPt f)
{
	register PredicatePt pr ;
	if( FunctorArity(f) > maxPredArity )
		DatabaseError("Highest predicate arity (%d) exceeded on predicate '%s/%d'",
							maxPredArity, FunctorName(f), FunctorArity(f)) ;
	pr = PermAllocate(WordsOf(Predicate)) ;

	PredFunctor(pr) = f ;
	PredNextF(pr) = FunctorPreds(f) ;
	FunctorPreds(f) = pr ;

	PredUnit(pr) = CurrUnit() ;
	PredNextU(pr) = UnitPreds(CurrUnit()) ;
	UnitPreds(CurrUnit()) = pr ;

	ResetPredicate(pr) ;
	return pr ;
}

static void SetDynamicLogical(PredicatePt pr, Bool logical)
{
	if( !PredIsDynamic(pr) ) {
		PredIsDynamic(pr) = true ;
		PredIsLogical(pr) =
			allDynamic_flag > 0 ? (allDynamic_flag == 1) : logical ;
		PredKeepSource(pr) = true ;
	}
}

static void ResetConsultFile(PredicatePt pr)
{
	ClausePt cl ;
	PredConsultFile(pr) = nil ;
	doseq(cl, PredClauses(pr), ClauseNext(cl)) {
		ClauseConsultFile(cl) = nil ;
		if( IsClauseLoop(cl) ) break ;
	}
}

void BindPredicate(PredicatePt pr, Inst inst, Pt a0)
{
	PredStartInst(pr) = inst ;
	PredStartInstArgs(pr)[0] = a0 ;
}

void BindPredicateFull(PredicatePt pr, Inst inst, Pt a0, Pt a1, Pt a2, Pt a3)
{
	PredStartInst(pr) = inst ;
	PredStartInstArgs(pr)[0] = a0 ;
	PredStartInstArgs(pr)[1] = a1 ;
	PredStartInstArgs(pr)[2] = a2 ;
	PredStartInstArgs(pr)[3] = a3 ;
}

PredicatePt FindPredicateInCurrUnit(FunctorPt f)
{
	register PredicatePt pr ;
	register UnitPt u = CurrUnit() ;
	doseq(pr, FunctorPreds(f), PredNextF(pr))
		if( PredUnit(pr) == u )
			return pr ;
	return nil ;
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

PredicatePt LookupPredicate(FunctorPt f)
{
	if( FunctorIsBuiltin(f) )
		return FunctorPreds(f) ;
	else {
		register PredicatePt pr ;
		register UnitPt u = CurrUnit() ;
		doseq(pr, FunctorPreds(f), PredNextF(pr))
			if( PredUnit(pr) == u )
				return pr ;
		return NewPredicate(f) ;
	}
}

PredicatePt FindPredicateByName(CharPt n, int a)
{
	return FindPredicate(LookupFunctorByName(n, a)) ;
}

PredicatePt LookupPredicateByName(CharPt n, int a)
{
	return LookupPredicate(LookupFunctorByName(n, a)) ;
}

PredicatePt CheckPredicate(FunctorPt f)
{
	PredicatePt pr ;
	if( (pr = FindPredicate(f)) == nil || !PredHasClauses(pr) )
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
	doseq(cl, PredClauses(pr), ClauseNext(cl)) {
		n++ ;
		if( IsClauseLoop(cl) ) break ;
	}
	return n ;
}

CharPt PredNameArity(PredicatePt pr)
{
	if( UnitIsHidden(PredUnit(pr)) )
		return GStrFormat("%s/%d", PredName(pr), PredArity(pr)) ;
	else
		return GStrFormat("%s:%s/%d", UnitName(PredUnit(pr)),
										PredName(pr), PredArity(pr)) ;
}

Pt PredNameArityTerm(PredicatePt pr)
{
	if( UnitIsHidden(PredUnit(pr)) )
		return MakeSlashTerm(PredFunctor(pr)) ;
	else
		return MakeBinStruct(colonFunctor,
						MakeAtom(UnitName(PredUnit(pr))),
						MakeSlashTerm(PredFunctor(pr))) ;
}

Bool PredHiddenInGeneration(PredicatePt pr)
{
	return PredIsUndefined(pr)
		|| (PredIsBuiltin(pr) && PredName(pr)[0] == '$') ;
}

void MarkStaticBuiltinsAsPermanent()
{
	register PredicatePt pr ;
	doseq(pr, UnitPreds(systemUnit), PredNextU(pr)) {
		PredIsPermanent(pr) = !PredIsDynamic(pr) ;	
		if( PredIsUndefined(pr) ) /* Because is visible */
			BindPredicate(pr, EmptyPred, cPt(PredFunctor(pr))) ;
	/* Must be reset after loading built-ins via consult/1 */ 
		ResetConsultFile(pr) ;
	}
}

void AbolishPredicate(PredicatePt pr, Bool force)
{
	if( PredIsBuiltin(pr) ) {
		if( force ) { /* For loading built-ins using consult/1 */
			if( PredIsMeta(pr) )
				DatabaseError("Cannot abolish '%s' because it is a core builtin",
														PredNameArity(pr)) ;
			elif( PredIsPermanent(pr) )
				DatabaseError("Cannot abolish '%s' because it is a static builtin",
														PredNameArity(pr)) ;
			DeleteClausesAndIndex(pr) ;
			ResetPredicate(pr) ;
			PredWasAbolished(pr) = true ;													
		}
		elif( PredIsDynamic(pr) ) { /* A dynamic builtin is a special case */
			DeleteClausesAndIndex(pr) ;
			BindPredicate(pr, EmptyPred, cPt(PredFunctor(pr))) ;
		}
		else
			DatabaseError("Cannot abolish '%s' because it is a static builtin",
														PredNameArity(pr)) ;
	}
	else {
		DeleteClausesAndIndex(pr) ;
		ResetPredicate(pr) ;
		PredWasAbolished(pr) = true ;
	}
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

PredicatePt PredNextUPlusBuiltins(PredicatePt pr) /* pre: pr != nil */
{
	if( pr->nextU == nil && PredUnit(pr) != systemUnit )
		return UnitPreds(systemUnit) ;
	else return pr->nextU ;
}

ClausePt SetNewClause(Pt source, Bool end, Bool consulting)
{
	register PredicatePt pr ;
	ClausePt cl ;
	Hdl code ;
	Size size ;
	Pt head, body ;

	source = AllocateTermForAssert(source) ;
#if 0
	Write("%s\n", TermAsStr(source)) ;
#endif

	if( IsThisStruct(source, neckFunctor) ) {
		head = XStructArg(source,0) ;
		body = XStructArg(source,1) ;
	}
	else {
		head = source ;
		body = tTrueAtom ;
	}

	pr = LookupPredicate(XTestFunctor(head)) ;

	if( UnitIsPermanent(CurrUnit()) && !PredIsMutableBuiltin(pr) ) {
		ReleaseTerm(source) ;
		ChangingCurrUnit("asserting clause for", pr) ; /* Issue error */
	}

	if( PredIsPermanent(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("Cannot modify static built-in predicate '%s'",
													PredNameArity(pr)) ;
	}
	elif( consulting && PredHandleConsult(pr, false) )
		/* nothing */ ;
	elif( PredIsImported(pr) ) {
		ReleaseTerm(source) ;
		DatabaseError("Cannot modify import link '%s'", PredNameArity(pr)) ;
	}

	if( consulting ) {
		if( !PredHasClauses(pr) ) {	/* First clause about to be added */
			if( allDynamic_flag > 0 && !Booting() )
				SetDynamicLogical(pr, true) ;
			elif( keepSource_flag && !UnitIsHidden(CurrUnit()) )
				PredKeepSource(pr) = true ;
		}
	}
	else {	/* asserting */
		if( !PredHasClauses(pr) ) /* First clause about to be added to dynamic */
			SetDynamicLogical(pr, true) ;
		elif( !PredIsDynamic(pr) ) {		
			ReleaseTerm(source) ;
			DatabaseError("Cannot modify static predicate '%s'",
														PredNameArity(pr)) ;
		}
	}

	Compiler(head, body, &code, &size) ;
	cl = InstallClause(pr, code, size, head, source, end) ;
	if( consulting )
		ClauseConsultFile(cl) = ConsultFile() ;

	if( !PredKeepSource(pr) ) {
		ReleaseTerm(ClauseSource(cl)) ; /* release source */
		ClauseSource(cl) = nil ;
	}
	PredWasAbolished(pr) = false ;

	return cl ;
}

void SetDynamic(PredicatePt pr, Bool logical)
{
	ChangingCurrUnit("declaring dynamic", pr) ;

	if( PredIsPermanent(pr) ) /* Because this is allowed at boot time */
		DatabaseError("Cannot modify '%s' because it is a built-in",
													PredNameArity(pr)) ;
	elif( PredHandleConsult(pr, !PredIsDynamic(pr) || PredIsLogical(pr) != logical) )
		/* nothing */ ;
	elif( PredIsImported(pr)  )
		DatabaseError("Cannot set '%s' dynamic because it is a import link",
												PredNameArity(pr)) ;
	elif( !PredIsDynamic(pr) && PredHasClauses(pr) )
		DatabaseError("Cannot make '%s' dynamic because it is static",
												PredNameArity(pr)) ;
	elif( PredIsDynamic(pr) && PredIsLogical(pr) != logical )
		DatabaseError("Cannot change semantic view of dynamic predicate '%s'",
												PredNameArity(pr)) ;
	SetDynamicLogical(pr, logical) ;
	if( Booting() )
		PredIsMultifile(pr) = true ;
	if( PredIsUndefined(pr) ) {
		BindPredicate(pr, EmptyPred, cPt(PredFunctor(pr))) ;
		PredWasAbolished(pr) = false ;
	}
}

void SetMultifile(PredicatePt pr)
{
	ChangingCurrUnit("declaring multifile", pr) ;

	if( PredIsPermanent(pr) ) /* Because this is allowed at boot time */
		DatabaseError("Cannot set '%s' multifile because it is a builtin",
												PredNameArity(pr)) ;
	elif( PredHandleConsult(pr, !PredIsMultifile(pr)) )
		/* nothing */ ;
	elif( PredIsImported(pr)  )
		DatabaseError("Cannot set '%s' multifile because it is a import link",
												PredNameArity(pr)) ;
	elif( !PredIsMultifile(pr) && PredHasClauses(pr) )
		DatabaseError("Cannot make '%s' multifile because it already has clauses",
												PredNameArity(pr)) ;
	PredIsMultifile(pr) = true ;
	if( !PredIsDynamic(pr) ) {
		PredIsLogical(pr) = true ;
		PredKeepSource(pr) = true ;
	}
	if( PredIsUndefined(pr) ) {
		BindPredicate(pr, EmptyPred, cPt(PredFunctor(pr))) ;
		PredWasAbolished(pr) = false ;
	}
}

void SetVisible(PredicatePt pr)
{
	if( PredName(pr)[0] == '$' )
		DatabaseError("Cannot make '%s' visible because the name "
						"starts with \'$\'", PredNameArity(pr)) ;

	ChangingCurrUnit("declaring visible", pr) ;

	if( PredIsBuiltin(pr) )
		DatabaseError("Cannot set '%s' visible because it is a builtin",
												PredNameArity(pr)) ;
	elif( PredHandleConsult(pr, !PredIsVisible(pr)) )
		/* nothing */ ;
	elif( !PredIsVisible(pr) && PredHasClauses(pr) )
		DatabaseError("Cannot set '%s' visible because it already has clauses",
												PredNameArity(pr)) ;
	PredIsVisible(pr) = true ;
	if( PredIsUndefined(pr) ) {
		BindPredicate(pr, EmptyPred, cPt(PredFunctor(pr))) ;
		PredWasAbolished(pr) = false ;
	}
}

void SetImport(PredicatePt pr, Pt unit) /* pre: !HasFreeVars(unit) */
{
	ChangingCurrUnit("importing", pr) ;

	if( PredIsBuiltin(pr) )
		DatabaseError("Cannot import '%s' because it is a builtin",
												PredNameArity(pr)) ;
	elif( PredHandleConsult(pr, !PredIsImported(pr)) )
		/* nothing */ ;
	elif( PredIsDynamic(pr) )
		DatabaseError("Cannot import '%s' because it is already dynamic",
												PredNameArity(pr)) ;
	elif( PredIsMultifile(pr) )
		DatabaseError("Cannot import '%s' because it is already multifile",
												PredNameArity(pr)) ;
	elif( PredIsImported(pr) && Compare(GetImportTerm(pr), unit) )
		DatabaseError("Cannot import '%s' from two different unit specifiers",
												PredNameArity(pr)) ;
	elif( PredHasClauses(pr) )
		DatabaseError("Cannot import '%s' because it already has clauses",
												PredNameArity(pr)) ;
	PredIsIndexable(pr) = false ;
	BindPredicate(pr, Import, unit) ;
	PredWasAbolished(pr) = false ;
}

void SetNoIndex(PredicatePt pr)
{
	ChangingCurrUnit("making non-indexable", pr) ;

	if( PredIsBuiltin(pr) )
		DatabaseError("Cannot set '%s' no_index because it is a builtin",
												PredNameArity(pr)) ;
	elif( PredHandleConsult(pr, PredIsIndexable(pr)) )
		/* nothing */ ;
	elif( PredHasClauses(pr) )
		DatabaseError("Cannot set '%s' no_index because it already has clauses",
												PredNameArity(pr)) ;
	PredIsIndexable(pr) = false ;
}

#if unused
static void SetStatic(PredicatePt pr)	/* pre: pr is inactive */
{
	if( PredIsMultifile(pr) )
		DatabaseError("Not available for multifile predicate '%s'",
												PredNameArity(pr)) ;
	elif( PredIsDynamic(pr) ) {
		PredIsDynamic(pr) = false ;
		PredIsLogical(pr) = false ;
		PredKeepSource(pr) = keepSource_flag && !UnitIsHidden(CurrUnit()) ;
		ReinstallClausesAsStatic(pr) ;	
	}
}
#endif

void SetClauseList(register Pt list, Bool end, Bool visible)
{
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) ) {
		ClausePt cl = SetNewClause(XListHead(list), end, false) ;
		PredIsVisible(ClauseOwner(cl)) = visible ;
	}
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
}



/* BUILTIN C PREDICATES */

PredicatePt InstallCBuiltinPred(CharPt name, int arity, VFun cProc)
{
	PredicatePt pr ;
	FunctorPt f = LookupFunctorByName(name, arity) ;

	if( CurrUnit() != systemUnit )
		DatabaseError("Attempt to install C predicate '%s/%d' outside "
						"the system unit",  name, arity) ;

	pr = LookupPredicate(f) ;
	if( !PredIsUndefined(pr) )
		FatalError("C predicate already installed (%s/%d)", name, arity) ;
	PredIsPermanent(pr) = true ; /* Essencial for safety when booting */
	PredIsIndexable(pr) = false ;
	BindPredicate(pr, InstEncode(cProc), Proceed) ;
	return pr ;
}

PredicatePt InstallGNDeterCBuiltinPred(CharPt name, int arity, int extraA, VFun cProc)
{
	PredicatePt pr ;
	Pt h[2] ;
	ClausePt cl1, cl2 ;
	CharPt newName ;

	newName = GStrFormat("$$$$_%s", name) ;
/* Registers the "$$$$_..." name as a C predicate name */
	InstallCBuiltinPred(newName, arity, cProc) ;
	pr = InstallCBuiltinPred(name, arity, cProc) ; /* Take advantage of stuff */

	PredIsIndexable(pr) = false ;

	h[0] = cPt(cProc) ;
	h[1] = Proceed ; 
	cl1 = InstallClause(pr, h, 2, nil, nil, true) ;
	ClauseArity(cl1) += extraA ;
	cl2 = InstallClause(pr, h, 2, nil, nil, true) ;
	ClauseArity(cl2) += extraA ;
	ClauseInst(cl2) = RetryMeElse ;
	ClauseNext(cl2) = cl2 ;

	if( extraA == 0 )
		BindPredicate(pr, LocalJump, cPt(ClauseCode(cl1))) ;
	else
		BindPredicateFull(pr, PutNil, cPt(OutTemp(arity)),
							LocalJump, cPt(ClauseCode(cl1)), nil) ;
	return pr ;
}

PredicatePt InstallNDeterCBuiltinPred(CharPt name, int arity, VFun cProc)
{
	return InstallGNDeterCBuiltinPred(name, arity, 1, cProc) ;
}

PredicatePt FindCPredByInst(Inst inst)
{
	register PredicatePt pr ;
	doseq(pr, UnitPreds(systemUnit), PredNextU(pr)) {
        if( PredStartInst(pr) == inst )
            return pr ;
	}
    return nil ;
}

PredicatePt CurrCPred()
{
	if( (P[-1] == CallVar || P[-1] == ExecuteVar) ) {
		static PredicatePt callPred = nil ;
		if( callPred == nil )
			callPred = LookupPredicateByName("call", 1) ;
		return callPred ;
	}
	else
		return FindCPredByInst(P[-1]) ;
}

CharPt CurrCPredNameArity()
{
    register PredicatePt pr = CurrCPred() ;
	return pr == nil ? cCharPt("") : PredNameArity(pr) ;
}


/* CXPROLOG C'BUILTINS */

static void PTrue()
{
	JumpNext() ;
}

static void PFail()
{
	DoFail() ;
}

static void PNDRepeat0()
{
	JumpNext() ;
}

static void PNDRepeat1()
{
	if( A(1) == tNilAtom ) {
		A(0) = Drf(A(0)) ;
		if( XTestNat(A(0)) <= 0 ) Jump(DiscardAndFail) ;
		A(1) = zeroIntPt ;
	}
	A(1) = IncIntPt(A(1)) ;
	if( A(0) == A(1) ) Discard() ;
	JumpNext() ;
}

static void PNDRepeat2()
{
	if( A(2) == tNilAtom ) {
		A(0) = MakeInt(XTestNat(A(0))) ;
		XTestVar(X1) ;
		A(2) = zeroIntPt ;
	}
	if( A(0) == A(2) ) Jump(DiscardAndFail) ;
	A(2) = IncIntPt(A(2)) ;
	if( UnifyWithNumber(X1, A(2)) ) JumpNext() ;
	InternalError("PNDRepeat2") ;
}

static void PNDClause()
{
	ClausePt cl ;
	Pt clParts[2], clTerm ;

	if( A(2) == tNilAtom ) {
		PredicatePt pr = FindPredicate(XTestFunctor(X0)) ;
		if( pr == nil || !PredHasClauses(pr) ) Jump(DiscardAndFail) ;
		else cl = PredClauses(pr) ;
		A(3) = PredIsLogical(pr) ? GlobalClock : tNilAtom ;
	}
	else cl = cClausePt(A(2)) ;

	for(;; cl = ClauseNext(cl) ) {
		if( cl == nil || IsClauseLoop(cl) ) Jump(DiscardAndFail) ;
		if( A(3) != tNilAtom && !ClauseIsAlive2(cl, A(3)) ) continue ;
		if( (clTerm = ClauseSource(cl)) != nil ) {
			SplitNeckTerm(clTerm, clParts) ;
			if( UnifiableN(&X0, clParts, 2) ) break ;
		}
	}

	A(2) = cPt(ClauseNext(cl)) ;
	SplitNeckTerm(ZPushTerm(clTerm), clParts) ; /* stacks may grow */
	if( UnifyN(&X0, clParts, 2) ) JumpNext() ;
	InternalError("PNDClause") ;	
}

static void PNDRetract()
{
	ClausePt cl ;
	Pt X0Parts[2], clParts[2], clTerm ;

	SplitNeckTerm(X0, X0Parts) ;
	if( A(1) == tNilAtom ) {
		PredicatePt pr = FindPredicate(XTestFunctor(X0Parts[0])) ;
		if( pr == nil ) Jump(DiscardAndFail) ;
		if( PredIsPermanent(pr) )
			DatabaseError("Cannot modify static builtin predicate '%s'",
											PredNameArity(pr)) ;
		if( !PredHasClauses(pr) ) Jump(DiscardAndFail) ;
		if( !PredIsDynamic(pr) )
			DatabaseError("Cannot modify static predicate '%s'",
											PredNameArity(pr)) ;
		cl = PredClauses(pr) ;
		A(2) = PredIsLogical(pr) ? GlobalClock : tNilAtom ;
	}
	else cl = cClausePt(A(1)) ;
	
	for(;; cl = ClauseNext(cl) ) {
		if( cl == nil || IsClauseLoop(cl) ) Jump(DiscardAndFail) ;
		if( A(2) != tNilAtom && !ClauseIsAlive2(cl, A(2)) ) continue ;
		if( (clTerm = ClauseSource(cl)) != nil ) {
			SplitNeckTerm(clTerm, clParts) ;
			if( UnifiableN(X0Parts, clParts, 2) ) break ;
		}
	}

	A(1) = cPt(ClauseNext(cl)) ;
	SplitNeckTerm(ZPushTerm(clTerm), clParts) ; /* stacks may grow */
	if( UnifyN(X0Parts, clParts, 2) ) {
		DeleteClause(cl) ;
		JumpNext() ;
	}
	InternalError("PNDRetract") ;	
}

static void PAsserta()
{
	SetNewClause(X0, false, false) ;
	JumpNext() ;
}

static void PAssertz()
{
	SetNewClause(X0, true, false) ;
	JumpNext() ;
}

#if unused
static void PMakeStatic()
{
	PredicatePt pr ;
	if( (pr = FindPredicate(XTestSlash(X0))) == nil )
		Error("Couldn't find predicate '%s'", FunctorNameArity(XTestSlash(X0))) ;
	SetStatic(pr) ;
	JumpNext() ;
}
#endif

static void PAbolish()
{
	PredicatePt pr ;
	if( (pr = FindPredicate(XTestSlash(X0))) != nil )
		AbolishPredicate(pr, false) ;
	JumpNext() ;
}

static void PAbolish2()
{
	PredicatePt pr ;
	if( (pr = FindPredicate(XTestFunctor2(X0,X1))) != nil )
		AbolishPredicate(pr, false) ;
	JumpNext() ;
}

static void PAbolishBuiltin()
{
	FunctorPt f = XTestFunctor2(X0,X1) ;
	PredicatePt pr ;
	if( !Booting() )
		DatabaseError("This predicate only works at booting time") ;
	if( (pr = FindPredicate(f)) == nil )
		DatabaseError("Predicate '%s' is not defined", FunctorNameArity(f)) ;
	if( PredIsMeta(pr) )
		DatabaseError("The core builtin predicate '%s' cannot be abolished",
											FunctorNameArity(f)) ;
	if( !PredIsBuiltin(pr) )
		DatabaseError("Predicate '%s' is not a builtin", FunctorNameArity(f)) ;
	PredIsBuiltin(pr) = false ;	/* Make it not builtin */
	AbolishPredicate(pr, false) ;
	JumpNext() ;
}

static void PRenameBuiltin()
{
	FunctorPt f = XTestFunctor2(X0,X1) ;
	FunctorPt newf = XTestFunctor2(X2,X1) ; 
	PredicatePt pr, pr2 ;
	if( !Booting() )
		DatabaseError("This predicate only works at booting time") ;
	if( (pr = FindPredicate(f)) == nil )
		DatabaseError("Predicate '%s' not defined", FunctorNameArity(f)) ;
	if( PredIsMeta(pr) )
		DatabaseError("The core builtin predicate '%' cannot be renamed",
											PredNameArity(pr)) ;
	if( !PredIsBuiltin(pr) )
		DatabaseError("Predicate '%s' is not a builtin", PredNameArity(pr)) ;

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
	PredIsBuiltin(pr) = false ;	/* Required */
	PredFunctor(pr) = newf ;
	PredNextF(pr) = FunctorPreds(newf) ;
	FunctorPreds(newf) = pr ;	/* Put it in front, possibly hidding another */
    PredIsBuiltin(pr) = true ;	/* Required */
}

static Size PDynamicAux(FunctorPt f)
{
	SetDynamic(LookupPredicate(f), true) ;
	return 1 ;
}
static void PDynamic()
{
	ForEachInSpec(X0, PDynamicAux, false) ;
	JumpNext() ;
}

static Size PDynamicIUAux(FunctorPt f)
{
	SetDynamic(LookupPredicate(f), false) ;
	return 1 ;
}
static void PDynamicIU()
{
	ForEachInSpec(X0, PDynamicIUAux, false) ;
	JumpNext() ;
}

static Size PMultifileAux(FunctorPt f)
{
	SetMultifile(LookupPredicate(f)) ;
	return 1 ;
}
static void PMultifile()
{
	ForEachInSpec(X0, PMultifileAux, false) ;
	JumpNext() ;
}

static Size PVisibleAux(FunctorPt f)
{
	SetVisible(LookupPredicate(f)) ;
	return 1 ;
}
static void PVisible()
{
	ForEachInSpec(X0, PVisibleAux, false) ;
	JumpNext() ;
}

static Pt importUnitAux ;
static Size PImportAux(FunctorPt f) // Import unit in importUnitAux
{
	SetImport(LookupPredicate(f), importUnitAux) ;
	return 1 ;
}
static void PImport()
{
	if( HasFreeVars(X1) )
		DatabaseError("Import declaration from non-ground unit specifier '%s'",
												TermAsStr(X1)) ;
	importUnitAux = AllocateTermForAssert(X1) ;
	ForEachInSpec(X0, PImportAux, false) ;
	JumpNext() ;
}

static Size PNoIndexAux(FunctorPt f)
{
	SetNoIndex(LookupPredicate(f)) ;
	return 1 ;
}
static void PNoIndex()
{
	ForEachInSpec(X0, PNoIndexAux, false) ;
	JumpNext() ;
}

static void PBuiltins()
{
	PredicatePt pr ;
	Size i = 0 ;
	ShowVersion() ;
	Write("BUILTINS:\n") ;
	doseq(pr, UnitPreds(systemUnit), PredNextU(pr))
		if( PredIsBuiltin(pr) && !PredHiddenInGeneration(pr) ) {
			CharPt str = PredNameArity(pr) ;
			Size len = CharLen(str) ;
			if( i == 2 && len > 23 )
				Write("\n") ;
			if( i == 2 || len > 23 ) {
				Write("  %s\n", str) ;
				i = 0 ;
			}
			else {
				Write("  %s", str) ;
				for( ; len < 23 ; len++ )
					Write(" ") ;
				i++ ;
			}
		}
	if( i != 0 ) Write("\n") ;
	JumpNext() ;
}

static void PCBuiltins()
{
	PredicatePt pr ;
	Size i = 0 ;
	ShowVersion() ;
	Write("CBuiltins:\n") ;
	doseq(pr, UnitPreds(systemUnit), PredNextU(pr))
		if( PredIsC(pr) && !PredHiddenInGeneration(pr) ) {
			CharPt str = PredNameArity(pr) ;
			Size len = CharLen(str) ;
			if( i == 2 && len > 23 )
				Write("\n") ;
			if( i == 2 || len > 23 ) {
				Write("  %s\n", str) ;
				i = 0 ;
			}
			else {
				Write("  %s", str) ;
				for( ; len < 23 ; len++ )
					Write(" ") ;
				i++ ;
			}
		}
	if( i != 0 ) Write("\n") ;
	JumpNext() ;
}

static void PDummyTrue()
{
	JumpNext() ;
}

static void PDummyFail()
{
	DoFail() ;
}

void PredicatesInit()
{
	InstallCBuiltinPred("true", 0, PTrue) ;
	failPred = InstallCBuiltinPred("fail", 0, PFail) ;
	InstallCBuiltinPred("false", 0, PFail) ;
	InstallNDeterCBuiltinPred("repeat", 0, PNDRepeat0) ;
	InstallNDeterCBuiltinPred("repeat", 1, PNDRepeat1) ;
	InstallNDeterCBuiltinPred("repeat", 2, PNDRepeat2) ;

	InstallGNDeterCBuiltinPred("clause", 2, 2, PNDClause) ;
	InstallGNDeterCBuiltinPred("retract", 1, 2, PNDRetract) ;
	InstallCBuiltinPred("asserta", 1, PAsserta) ;
	InstallCBuiltinPred("assertz", 1, PAssertz) ;
	InstallCBuiltinPred("assert", 1, PAssertz) ;
/*	InstallCBuiltinPred("make_static", 1, PMakeStatic) ;*/
	InstallCBuiltinPred("abolish", 1, PAbolish) ;
	InstallCBuiltinPred("abolish", 2, PAbolish2) ;
	InstallCBuiltinPred("abolish_builtin", 2, PAbolishBuiltin) ;
	InstallCBuiltinPred("rename_builtin", 3, PRenameBuiltin) ;

	InstallCBuiltinPred("dynamic", 1, PDynamic) ;
	InstallCBuiltinPred("dynamic_iu", 1, PDynamicIU) ;
	InstallCBuiltinPred("multifile", 1, PMultifile) ;
	InstallCBuiltinPred("visible", 1, PVisible) ;
	InstallCBuiltinPred("import", 2, PImport) ;
	InstallCBuiltinPred("no_index", 1, PNoIndex) ;

	InstallCBuiltinPred("builtins", 0, PBuiltins) ;
	InstallCBuiltinPred("cbuiltins", 0, PCBuiltins) ;

	InstallCBuiltinPred("discontiguous", 1, PDummyTrue) ;
	InstallCBuiltinPred("set_prolog_flag", 2, PDummyTrue) ;
	InstallCBuiltinPred("current_prolog_flag", 2, PDummyFail) ;
}
