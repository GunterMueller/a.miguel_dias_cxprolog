/*
 *   This file is part of the CxProlog system

 *   Builtins.c
 *   by A.Miguel Dias - 1989/12/03
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


/* TEST TYPE */

static void PVar()
{
	if( IsVar(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PNonVar()
{
	if( IsVar(Drf(X0)) ) DoFail()
	JumpNext()
}

static void PAtom()
{
	if( IsAtom(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PInt()
{
	if( IsInt(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PReal()
{
	if( IsReal(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PNumber()
{
	if( IsNumber(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PExtra()
{
	if( IsExtra(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PAtomic()
{
	if( IsAtomic(Drf(X0)) ) JumpNext()
	DoFail()
}



/* PREDICATES */

static void PNDRepeat()
{
	JumpNext()
}

static void PNDClause()
{
	ClausePt cl ;
	Bool b ;
	Hdl saveTrail, h ;
	Pt head, body ;

	if( A(2) == tNilAtom ) {
		PredicatePt pr = FindPredicate(XTestFunctor(X0)) ;
		if( pr == nil || PredIsUndefined(pr) )
			Jump(DiscardAndFail)
		else
			cl = PredFirstClause(pr) ;
	}
	else cl = cClausePt(A(2)) ;
	
	for(;;) {
		if( cl == nil || ClauseTerm(cl) == nil ) Jump(DiscardAndFail) ;
		SplitClauseTerm(ClauseTerm(cl), &head, &body) ;
		saveTrail = TR ;
		b = Unify(X0, head) && Unify(X1, body) ;
		RestoreTrail(saveTrail, h) ;
		if( b ) break ;
		cl = ClauseNext(cl) ;
	}

	A(2) = cPt(ClauseNext(cl)) ;
	SplitClauseTerm(PushTerm(ClauseTerm(cl)), &head, &body) ;
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
		cl = PredFirstClause(pr) ;
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

static void PUndef()
{
	ListUndefPredicates() ;
	JumpNext()
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



/* TERM MANIPULATION */

static void PFunctor()
{
	Pt t0 = Drf(X0) ;
	if( IsStruct(t0) ) {
		if( UnifyWithAtomic(X1, TagAtom(XStructAtom(t0))) &&
			UnifyWithAtomic(X2, MakeInt(XStructArity(t0))) ) JumpNext()
		DoFail()
	}
	elif( IsList(t0) ) {
		if( UnifyWithAtomic(X1, MakeAtom(".")) &&
			UnifyWithAtomic(X2, MakeInt(2)) ) JumpNext()
		DoFail()
	}
	elif( IsAtomic(t0) || IsExtra(t0) ) {
		if( UnifyWithAtomic(X1, t0) &&
			UnifyWithAtomic(X2, MakeInt(0)) ) JumpNext()
		DoFail()
	}
	elif( IsVar(t0) ) {
		Pt t1 = Drf(X1) ;
		Pt t2 = Drf(X2) ;
		if( not IsAtomic(t1) || not IsNat(t2) ) DoFail()
		if( XInt(t2) == 0 && UnifyWithAtomic(t0, t1) ) JumpNext()
		if( not IsAtom(t1) ) DoFail()
		if( Unify(t0, MakeCleanTerm(LookupFunctor(XAtom(t1), XInt(t2)))) )
										JumpNext()
		DoFail()
	}
	Default("PFunctor") ;
}

static void PArg()
{
	Pt t1 = Drf(X1) ;
	if( IsStruct(t1) ) {
		if( XTestPosInt(X0) > XStructArity(t1) ) Error("Out of range") ;
		if( Unify(X2, XStructArg(t1, XTestPosInt(X0) - 1)) ) JumpNext()
		DoFail()
	}
	elif( IsList(t1) ) {
		if( XTestPosInt(X0) > 2 ) Error("Out of range") ;
		if( Unify(X2, XListArg(t1, XTestPosInt(X0) - 1)) ) JumpNext()
		DoFail()
	}
	else TypeError("structure or list", t1) ;
}

static void PName()
{
	Pt t0 = Drf(X0) ;
	Pt t1 = Drf(X1) ;
	if( IsAtom(t0) ) {
		if( Unify(t1, StringToPString(XAtomName(t0))) ) JumpNext()
		DoFail()
	}
	elif( IsExtra(t0) && XExtraSubTag(t0) == textSubTag ) {
		if( Unify(t1, StringToPString(XExtraName(t0))) ) JumpNext()
		DoFail()
	}
	elif( IsList(t1) || t1 == tNilAtom ) {
		if( UnifyWithAtomic(t0, MakeAtom(PStringToString(t1))) ) JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PConcatS()
{
	if( Unify(X1, StringToPString(PConcatString(X0))) ) JumpNext()
	DoFail()
}



/* CUT */

static void PGetLevel()
{
	if( UnifyWithAtomic(X0, TagAtom(Ef(B0))) ) JumpNext()
	DoFail()
}

static void PCutTo()
{
	Pt t0 = Drf(X0) ;
	if( cChoicePointPt(XPt(t0)) > B ) {
		B = cChoicePointPt(XPt(t0)) ;
		HB = Bf(H) ;
	
	/* TidyTrail
		t0 = cPt(Bf(TR)) ;
		while( cHdl(t0) < TR )
			if( IsToTrailVar(*cHdl(t0)) ) t0++ ;
			else *t0 = cWord(Pop(TR)) ; */
	}
	JumpNext()
}

static void PCut()
{
	Error("Dynamic '!/0' is not supported") ;
	JumpNext()
}



/* EQUALITY */

static void PTrue()
{
	JumpNext()
}

static void PFail()
{
	DoFail()
}

static void PUnify()
{
	if( Unify(X0, X1) ) JumpNext()
	DoFail()
}

static void PNoUnify(void)
{
	Hdl saveTrail = TR, h ;
	if( Unify(X0, X1) ) DoFail()
	RestoreTrail(saveTrail, h) ;
	JumpNext()
}

static void PEqual()
{
	if( Equal(X0, X1) ) JumpNext()
	DoFail()
}

static void PNoEqual()
{
	if( Equal(X0, X1) ) DoFail()
	JumpNext()
 }



/* IMPERATIVE VARIABLES */

static void PCreateImperativeVar()
{
	DefineImperativeVar(XTestAtom(X0)) ;
	JumpNext()
}

static void PImperativeVarSet()
{
	ImperativeVarSet(XTestAtom(X0), X1) ;
	JumpNext()
}

static void PImperativeVarGet()
{
	if( Unify(X1, ImperativeVarGet(XTestAtom(X0))) ) JumpNext()
	DoFail()
}

static void PNDCurrentImperativeVar()
{
	ImperativeVarPt iv =
		A(1) == tNilAtom ? FirstImperativeVar() : NextImperativeVar(cImperativeVarPt(A(1))) ;
	A(1) = cPt(iv) ;
	if( iv == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagAtom(iv->atom)) ) JumpNext()
	DoFail()
}



/* INPUT/OUTPUT */

static void PGet0()
{
	if( UnifyWithAtomic(X0, MakeInt(Get0())) ) JumpNext()
	DoFail()
}

static void PGet()
{
	if( UnifyWithAtomic(X0, MakeInt(Get())) ) JumpNext()
	DoFail()
}

static void PPeek()
{
	if( UnifyWithAtomic(X0, MakeInt(Peek0())) ) JumpNext()
	DoFail()
}

static void PGetS()
{
	if( UnifyWithAtomic(X0, StringToPString(GetLine())) ) JumpNext()
	DoFail()
}

static void PSkip()
{
	Skip(XTestInt(X0)) ;
	JumpNext()
}

static void PRead()
{
	Pt t ;
	if( ( t = ReadTerm(nil) ) != nil && Unify(X0, t) ) JumpNext()
	DoFail()
}

static void PRead2()
{
	Pt t, names ;
	if( ( t = ReadTerm(&names) ) != nil &&
		Unify(X0, t) && Unify(X1, names) ) JumpNext()
	DoFail()
}

static void PPut()
{
	Put(XTestInt(X0)) ;
	JumpNext()
}

static void PPutS()
{
	PutString(PStringToString(X0)) ;
	JumpNext()
}

static void PNl()
{
	Nl() ;
	JumpNext()
}

static void PTab()
{
	Tab(XTestInt(X0)) ;
	JumpNext()
}

static void PWrite()
{
	GWriteTerm(X0, wNormal, false) ;
	JumpNext()
}

static void PWriteQ()
{
	GWriteTerm(X0, quoted, false) ;
	JumpNext()
}

static void PPrint()
{
	GWriteTerm(X0, print, false) ;
	JumpNext()
}

static void PDisplay()
{
	GWriteTerm(X0, display, true) ;
	JumpNext()
}



/* FILESYS */

static void PExists()
{
	if( access(XTestAtomOrTextName(X0), 0) == 0 ) JumpNext()
	DoFail()
}



/* OPERATORS */

static void POp()
{
	DefineOperator(XTestInt(X0), XTestAtomName(X1), X2) ;
	JumpNext()
}

static void PResetOps()
{
	ResetOperators() ;
	JumpNext()
}

static void PNDCurrentOp()
{
	OperatorPt o ;

	o = A(1) == tNilAtom ? FirstOperator() : NextOperator(cOperatorPt(A(1))) ;
	A(1) = cPt(o) ;
	if( o == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagAtom(OperatorAtom(o))) ) JumpNext()
	DoFail()
}

static void PIsOp()
{
	int p, lp, rp ;

	switch( XTestInt(X1) )
	{
		case 0:	lp = p = Prefix(XTestAtom(X0), &rp) ; break ;
		case 1:	p = Infix(XTestAtom(X0), &lp, &rp) ; break ;
		case 2:	rp = p = Postfix(XTestAtom(X0), &lp) ; break ;
		default: Error("Second argument should be 0, 1 ou 2") ;
	}
	if( p != 0 &&
			UnifyWithAtomic(X2, MakeInt(p)) &&
			UnifyWithAtomic(X3, MakeInt(lp)) &&
			UnifyWithAtomic(X4, MakeInt(rp)) ) JumpNext()
	DoFail()
}

static void PLocalOperators()
{
	SetLocalOperators(true) ;
	JumpNext()
}

static void PNoLocalOperators()
{
	SetLocalOperators(false) ;
	JumpNext()
}



/* STREAMS */

static void PPrompt()
{
	WritePrompt() ;
	JumpNext()
}

static void PSee()
{
	See(XTestAtomOrTextName(X0)) ;
	JumpNext()
}

static void PSeeing()
{
	if( UnifyWithAtomic(X0, MakeAtom(Seeing())) ) JumpNext()
	DoFail()
}

static void PSeen()
{
	Seen() ;
	JumpNext()
}

static void PTell()
{
	Tell(XTestAtomOrTextName(X0)) ;
	JumpNext()
}

static void PTelling()
{
	if( UnifyWithAtomic(X0, MakeAtom(Telling())) ) JumpNext()
	DoFail()
}

static void PTold()
{
	Told() ;
	JumpNext()
}



/* ARITMETIC */

static void PIs()
{
	if( UnifyWithAtomic(X0, TermEval(X1)) ) JumpNext()
	DoFail()
}

static void PEq()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 == i1 : i0 == r1 )
			: ( b1 ? r0 == i1 : r0 == r1 ) ) JumpNext()
	DoFail()
}

static void PNe()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 != i1 : i0 != r1 )
			: ( b1 ? r0 != i1 : r0 != r1 ) ) JumpNext()
	DoFail()
}

static void PLt()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 < i1 : i0 < r1 )
			: ( b1 ? r0 < i1 : r0 < r1 ) ) JumpNext()
	DoFail()
}

static void PGt()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 > i1 : i0 > r1 )
			: ( b1 ? r0 > i1 : r0 > r1 ) ) JumpNext()
	DoFail()
}

static void PLe()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 <= i1 : i0 <= r1 )
			: ( b1 ? r0 <= i1 : r0 <= r1 ) ) JumpNext()
	DoFail()
}

static void PGe()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 >= i1 : i0 >= r1 )
			: ( b1 ? r0 >= i1 : r0 >= r1 ) ) JumpNext()
	DoFail()
}

static void PSucc()
{
	Pt t0 = Drf(X0) ;
	if( IsInt(t0) ) {
		Int i = XInt(t0) + 1 ;
		if( i < 1 || not UnifyWithAtomic(X1, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	elif( IsVar(t0) ) {
		Int i = XTestInt(X1) - 1 ;
		if( i < 0 || not UnifyWithAtomic(t0, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	XTestInt(X0) ;
}



/* THREADS */

static void PNewThread()
{
	if( CreateThread(XTestAtom(X0), XTestInt(X1) Kb, X2, X3) == nil ) /* @@@ */
		DoFail()
	else JumpNext()
}

static void PTransferToThread()
{
	XTestVar(X1) ;
	TransferToThread(LookupThread(XTestAtom(X0))) ;
	JumpNext()
}

static void PActualThread()
{
	if( UnifyWithAtomic(X0, TagAtom(GetCurrThreadName())) )
		JumpNext()
	else DoFail()
}

static void PKillThread()
{
	KillThread(LookupThread(XTestAtom(X0))) ;
	JumpNext()
}



/* UNITS / CONTEXTS */

static void PCreateUnit()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) )
		DefineUnit(XAtom(t0), nil, 0) ;
	elif( IsStruct(t0) ) {
		DefineUnit(XStructAtom(t0), XStructArgs(t0), XStructArity(t0)) ;
	}
	else Error("Argument must be an atom or a structure") ;
	JumpNext()
}

static void PGetContext()
{
	if( Unify(X0, C) ) JumpNext()
	DoFail()
}

static void PGetHContext()
{
	if( Unify(X0, CH) ) JumpNext()
	DoFail()
}

static void PNDCurrentUnit()
{
	UnitPt u =
		A(1) == tNilAtom ? FirstUnit() : NextUnit(cUnitPt(A(1))) ;
	A(1) = cPt(u) ;
	if( u == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, MakeCleanTerm(UnitFunctor(u))) ) JumpNext()
	DoFail()
}



/* FLAGS */

static void PTrace()
{
	SetTrace(true) ;
	JumpNext()
}

static void PNoTrace()
{
	SetTrace(false) ;
	JumpNext()
}

static void PSetTrace()
{
	SetTrace(XTestBool(X0)) ;
	JumpNext()
}

static void PCheckTrace()
{
	if( CheckTrace() ) JumpNext()
	DoFail()
}

static void PSetInTopGoal()
{
	SetInTopGoal(true) ;
	JumpNext()
}

static void PCheckInTopGoal()
{
	if( CheckInTopGoal() ) JumpNext()
	DoFail()
}

static void PSetSilent()
{
	SetSilent(XTestBool(X0)) ;
	JumpNext()
}

static void PCheckSilent()
{
	if( CheckSilent() ) JumpNext()
	DoFail()
}

static void PSetForceVisibility()
{
	SetForceVisibility(XTestBool(X0)) ;
	JumpNext()
}

static void PCheckForceVisibility()
{
	if( CheckForceVisibility() ) JumpNext()
	DoFail()
}


/* MISC */

static void PEvent()
{
	PrologEvent(XTestInt(X0)) ;
	JumpNext()
}

static void PFlags()
{
	PrintFlags() ;
	JumpNext()
}

static void PUserMode()
{
	EnterUserMode() ;
	JumpNext()
}

static void PStatistics()
{
	Statistics() ;
	JumpNext()
}



/* */

void InstallCBuiltinPreds()
{
/* TEST TYPE */
	InstallCBuiltinPred("var", 1, PVar) ;
	InstallCBuiltinPred("nonvar", 1, PNonVar) ;
	InstallCBuiltinPred("atom", 1, PAtom) ;
	InstallCBuiltinPred("integer", 1, PInt) ;
	InstallCBuiltinPred("real", 1, PReal) ;
	InstallCBuiltinPred("number", 1, PNumber) ;
	InstallCBuiltinPred("extra", 1, PExtra) ;
	InstallCBuiltinPred("atomic", 1, PAtomic) ;

/* PREDICATES */
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
	InstallCBuiltinPred("undef", 0, PUndef) ;
	InstallCBuiltinPred("@@_code", 1, PCode1) ;
	InstallCBuiltinPred("@@_code", 2, PCode2) ;
	InstallCBuiltinPred("clist", 0, PListCBuiltinPreds) ;
	InstallCBuiltinPred("visible", 2, PVisible) ;
	InstallCBuiltinPred("import", 3, PImport) ;
	InstallCBuiltinPred("check_imports", 0, PCheckImports) ;

/* TERM MANIPULATION */
	InstallCBuiltinPred("functor", 3, PFunctor) ;
	InstallCBuiltinPred("arg", 3, PArg) ;
	InstallCBuiltinPred("name", 2, PName) ;
	InstallCBuiltinPred("concats", 2, PConcatS) ;

/* CUT */
	InstallCBuiltinPred("!", 0, PCut) ;
	InstallCBuiltinPred("get_level", 1, PGetLevel) ;
	InstallCBuiltinPred("cut", 1, PCutTo) ;

/* EQUALITY */
	InstallCBuiltinPred("true", 0, PTrue) ;
	InstallCBuiltinPred("fail", 0, PFail) ;
	InstallCBuiltinPred("=", 2, PUnify) ;
	InstallCBuiltinPred("\\=", 2, PNoUnify) ;
	InstallCBuiltinPred("==", 2, PEqual) ;
	InstallCBuiltinPred("\\==", 2, PNoEqual) ;

/* IMPERATIVE VARIABLES */
	InstallCBuiltinPred("create_ivar", 1, PCreateImperativeVar) ;
	InstallCBuiltinPred(":=", 2, PImperativeVarSet) ;
	InstallCBuiltinPred("=:", 2, PImperativeVarGet) ;
	InstallNDeterCBuiltinPred("current_ivar", 1, PNDCurrentImperativeVar) ;

/* INPUT/OUTPUT */
	InstallCBuiltinPred("get0", 1, PGet0) ;
	InstallCBuiltinPred("get", 1, PGet) ;
	InstallCBuiltinPred("peek", 1, PPeek) ;
	InstallCBuiltinPred("gets", 1, PGetS) ;
	InstallCBuiltinPred("skip", 1, PSkip) ;
	InstallCBuiltinPred("read", 1, PRead) ;
	InstallCBuiltinPred("read", 2, PRead2) ;
	InstallCBuiltinPred("put", 1, PPut) ;
	InstallCBuiltinPred("puts", 1, PPutS) ;
	InstallCBuiltinPred("nl", 0, PNl) ;
	InstallCBuiltinPred("tab", 1, PTab) ;
	InstallCBuiltinPred("write", 1, PWrite) ;
	InstallCBuiltinPred("writeq", 1, PWriteQ) ;
	InstallCBuiltinPred("print", 1, PPrint) ;
	InstallCBuiltinPred("display", 1, PDisplay) ;


/* FILESYS */
	InstallCBuiltinPred("file_exists", 1, PExists) ;

/* OPERATORS */
	InstallCBuiltinPred("op", 3, POp) ;
	InstallCBuiltinPred("reset_ops", 0, PResetOps) ;
	InstallNDeterCBuiltinPred("@@_current_op_aux", 1, PNDCurrentOp) ;
	InstallCBuiltinPred("@@_is_op", 5, PIsOp) ;
	InstallCBuiltinPred("local_operators", 0, PLocalOperators) ;
	InstallCBuiltinPred("nolocal_operators", 0, PNoLocalOperators) ;

/* STREAMS */
	InstallCBuiltinPred("@@_prompt", 0, PPrompt) ;
	InstallCBuiltinPred("see", 1, PSee) ;
	InstallCBuiltinPred("seeing", 1, PSeeing) ;
	InstallCBuiltinPred("seen", 0, PSeen) ;
	InstallCBuiltinPred("tell", 1, PTell) ;
	InstallCBuiltinPred("telling", 1, PTelling) ;
	InstallCBuiltinPred("told", 0, PTold) ;

/* ARITMETIC */
	InstallCBuiltinPred("is", 2, PIs) ;
	InstallCBuiltinPred("=:=", 2, PEq) ;
	InstallCBuiltinPred("=\\=", 2, PNe) ;	/* Use of escape '\' */
	InstallCBuiltinPred("<", 2, PLt) ;
	InstallCBuiltinPred(">", 2, PGt) ;
	InstallCBuiltinPred("=<", 2, PLe) ;
	InstallCBuiltinPred(">=", 2, PGe) ;
	InstallCBuiltinPred("succ", 2, PSucc) ;

/* THREADS */
	InstallCBuiltinPred("new_thread", 4, PNewThread) ;
	InstallCBuiltinPred("transfer_to_thread", 2, PTransferToThread) ;
	InstallCBuiltinPred("actual_thread", 1, PActualThread) ;
	InstallCBuiltinPred("kill_thread", 1, PKillThread) ;
	
/* UNITS / CONTEXTS */
	InstallCBuiltinPred("create_unit", 1, PCreateUnit) ;
	InstallCBuiltinPred("context", 1, PGetContext) ;
	InstallCBuiltinPred("hcontext", 1, PGetHContext) ;
	InstallNDeterCBuiltinPred("current_unit", 1, PNDCurrentUnit) ;

/* FLAGS */
	InstallCBuiltinPred("trace", 0, PTrace) ;
	InstallCBuiltinPred("notrace", 0, PNoTrace) ;
	InstallCBuiltinPred("set_trace", 1, PSetTrace) ;
	InstallCBuiltinPred("check_trace", 0, PCheckTrace) ;
	InstallCBuiltinPred("@@_set_in_top_goal", 1, PSetInTopGoal) ;
	InstallCBuiltinPred("@@_check_in_top_goal", 0, PCheckInTopGoal) ;
	InstallCBuiltinPred("set_silent", 1, PSetSilent) ;
	InstallCBuiltinPred("check_silent", 0, PCheckSilent) ;
	InstallCBuiltinPred("set_force_visibility", 0, PSetForceVisibility) ;
	InstallCBuiltinPred("check_visibility", 0, PCheckForceVisibility) ;

/* MISC */
	InstallCBuiltinPred("@@_event", 1, PEvent) ;
	InstallCBuiltinPred("flags", 0, PFlags) ;
	InstallCBuiltinPred("@@_user_mode", 0, PUserMode) ;
	InstallCBuiltinPred("statistics", 0, PStatistics) ;
}

