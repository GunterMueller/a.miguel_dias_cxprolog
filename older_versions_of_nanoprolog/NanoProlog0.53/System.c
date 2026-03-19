/*
 *   This file is part of the NanoProlog system

 *   System.c
 *   by A.Miguel Dias - 89/12/3
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 940225: new predicate $user_mode
 931117: release of version 0.5

*/

#include "NanoProlog.h"


/* */

#define X0		Xc(0)
#define X1		Xc(1)
#define X2		Xc(2)
#define X3		Xc(3)
#define X4		Xc(4)

#define R0		Drf(X0)
#define R1		Drf(X1)
#define R2		Drf(X2)
#define R3		Drf(X3)
#define R4		Drf(X4)

static Bool b, b0, b1 ;
static Int i, i0, i1 ;
static int n, n0, n1 ;
static Real r, r0, r1 ;
static Pt t0, t1, t2, t3 ;
static Hdl hh, h0, h1 ;
static char *str ;
static Clause *cl ;
static Predicate *pr ;
static Atom *at ;

static void TypeError(char *s)
{
	Error("Type Error: %s expected", s) ;
}

static void TypeCheck(char *types)
{
	Hdl a ;

	for( a = X ; *types != '\0' ; a++ )
	{
		switch(	*types++ )
		{
			case 'a':
					if( not IsAtom(Drf(*a)) ) TypeError("atom") ;
					break ;
			case 'i':
					if( not IsInt(Drf(*a)) ) TypeError("integer") ;
					break ;
			case 'p':
					if( not IsPos(Drf(*a)) ) TypeError("positive integer") ;
					break ;
			case 'n':
					if( not IsNumber(Drf(*a)) ) TypeError("number") ;
					break ;
			case 't':
					if( not IsAtomic(Drf(*a)) ) TypeError("atomic term") ;
					break ;
 			case 'v':
					if( not IsVar(Drf(*a)) ) TypeError("variable") ;
					break ;
			case 's':
					if( not IsStruct(Drf(*a)) ) TypeError("structure") ;
					break ;
			case 'l':
					if( not IsList(Drf(*a)) || Drf(*a) != TagAtom(nilAtom) ) TypeError("list") ;
					break ;
			case '?':
					break ;
			default: Default("TypeCheck") ;
		}
	}
}


/* TEST PREDICATES */

static void PTrue()
{
	JumpNext()
}

static void PFail()
{
	DoFail()
}

static void PVar()
{
	if( IsVar(R0) ) JumpNext()
	DoFail()
}

static void PNoVar()
{
	if( IsVar(R0) ) DoFail()
	JumpNext()
}

static void PAtom()
{
	if( IsAtom(R0) ) JumpNext()
	DoFail()
}

static void PInt()
{
	if( IsInt(R0) ) JumpNext()
	DoFail()
}

static void PReal()
{
	if( IsReal(R0) ) JumpNext()
	DoFail()
}

static void PNumber()
{
	if( IsNumber(R0) ) JumpNext()
	DoFail()
}

static void PAtomic()
{
	if( IsAtomic(R0) ) JumpNext()
	DoFail()
}



/* TERM MANIPULATION */

static void DoClause()
{
	VarValue2(t0, X0) ;
	if( IsStruct(t0) ) pr = LookupPredicate(XStructFunctor(t0)) ;
	elif( IsAtom(t0) ) pr = LookupPredicate2(XAtomName(t0), 0) ;
	elif( IsList(t0) ) pr = LookupPredicate(listFunctor) ;
	elif( IsVar(t0) ) Error("Cannot determine predicate") ;
	elif( IsNumber(t0) ) Error("Clause head is a number") ;
	else Default("DoClause") ;

	if( A(2) == tNilAtom )
	{
		if( PredNClauses(pr) > 0 ) ;
		elif( PredIsC(pr)
				&& UnifyWithAtomic(X1, MakeAtom("*** C CODE ***")) )
		{
			A(2) = nil ;
			JumpNext()
		}
		else Jump(DiscardAndFail)
		cl = PredFirstClause(pr) ;
	}
	else cl = ClausePt(A(2)) ;
	
	loop
	{
		if( cl == nil ) Jump(DiscardAndFail) ;
		SplitClauseTerm(ClauseTerm(cl), &t2, &t3) ;
		hh = TR ;
		b = Unify(X0, t2) && Unify(X1, t3) ;
		RestoreTrail(hh, h0) ;
		if( b ) break ;
		cl = ClauseNext(cl) ;
	}

	A(2) = cPt(ClauseNext(cl)) ;
	SplitClauseTerm(CopyTermToGlobal(ClauseTerm(cl)), &t2, &t3) ;
	if( Unify(X0, t2) && Unify(X1, t3) ) JumpNext()
	Error("Internal error in DoClause") ;	
}

static void PAsserta()
{
	CompileClause(R0, false) ;
	JumpNext()
}

static void PAssertz()
{
	CompileClause(R0, true) ;
	JumpNext()
}

static void DoRetract()
{
	SplitClauseTerm(X0, &t0, &t1) ;

	if( IsStruct(t0) ) pr = LookupPredicate(XStructFunctor(t0)) ;
	elif( IsAtom(t0) ) pr = LookupPredicate2(XAtomName(t0), 0) ;
	elif( IsList(t0) ) pr = LookupPredicate(listFunctor) ;
	elif( IsVar(t0) ) Error("Cannot determine predicate") ;
	elif( IsNumber(t0) ) Error("Clause head is a number") ;
	else Default("DoRetract") ;

	if( PredIsSys(pr) )
		Error("Atempt to retract system predicate '%s'", PredNameArity(pr)) ;

	if( A(1) == tNilAtom )
	{
		if( PredNClauses(pr) == 0 ) Jump(DiscardAndFail) ;
		cl = PredFirstClause(pr) ;
	}
	else cl = ClausePt(A(1)) ;
	
	loop
	{
		if( cl == nil ) Jump(DiscardAndFail) ;
		hh = TR ;
		b = Unify(X0, ClauseTerm(cl)) ;
		RestoreTrail(hh, h0) ;
		if( b ) break ;
		cl = ClauseNext(cl) ;
	}

	A(1) = cPt(ClauseNext(cl)) ;
	if( Unify(X0, CopyTermToGlobal(ClauseTerm(cl))) )
	{
		DeleteCode(cl) ;
		JumpNext()
	}
	Error("Internal error in DoRetract") ;	
}

static void PFunctor()
{
	VarValue2(t0, X0) ;
	if( IsStruct(t0) )
	{
		if( UnifyWithAtomic(X1, TagAtom(XStructAtom(t0))) &&
			UnifyWithAtomic(X2, MakeInt(XStructArity(t0))) ) JumpNext()
		DoFail()
	}
	elif( IsList(t0) )
	{
		if( UnifyWithAtomic(X1, MakeAtom(".")) &&
			UnifyWithAtomic(X2, MakeInt(2)) ) JumpNext()
		DoFail()
	}
	elif( IsAtomic(t0) )
	{
		if( UnifyWithAtomic(X1, t0) &&
			UnifyWithAtomic(X2, MakeInt(0)) ) JumpNext()
		DoFail()
	}
	elif( IsVar(t0) )
	{
		VarValue2(t1, X1) ;
		VarValue2(t2, X2) ;
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
	TypeCheck("p") ;
	VarValue2(t0, X0) ;
	VarValue2(t1, X1) ;
	if( IsStruct(t1) )
	{
		if( XInt(t0) > XStructArity(t1) ) Error("Out of range") ;
		if( Unify(X2, XStructArg(t1, XInt(t0) - 1)) ) JumpNext()
		DoFail()
	}
	elif( IsList(t1) )
	{
		if( XInt(t0) > 2 ) Error("Out of range") ;
		if( Unify(X2, XListArg(t1, XInt(t0) - 1)) ) JumpNext()
		DoFail()
	}
	else TypeError("structure") ;
}

static void PName()
{
	VarValue2(t0, X0) ;
	VarValue2(t1, X1) ;
	if( IsAtom(t0) )
	{
		if( Unify(t1, AtomToString(t0)) ) JumpNext()
		DoFail()
	}
	elif( IsList(t1) || t1 == tNilAtom )
	{
		if( UnifyWithAtomic(t0, StringToAtom(t1)) ) JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PGetLevel()
{
	if( UnifyWithAtomic(X0, TagAtom(Ef(B))) ) JumpNext()
	DoFail()
}

static void PCut1()
{
	VarValue2(t0, X0) ;
	B = cChoicePointPt(XPt(t0)) ;
	HB = Bf(H) ;
	JumpNext()
}

static void PCut()
{
	Error("Interpreted '!/0' doesn't work") ;
	JumpNext()
}


/* EQUALITY */

static void PUnify()
{
	if( Unify(X0, X1) ) JumpNext()
	DoFail()
}

static void PEqual()
{
	if( Equal(X0, X1) ) JumpNext()
	DoFail()
}



/* INPUT & OUTPUT */

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

static void PSkip()
{
	TypeCheck("i") ;
	Skip(XInt(R0)) ;
	JumpNext()
}

static void PRead()
{
	if( ( t0 = ReadTerm(nil) ) != nil && Unify(X0, t0) ) JumpNext()
	DoFail()
}

static void PRead2()
{
	if( ( t0 = ReadTerm(&t1) ) != nil &&
		Unify(X0, t0) && Unify(X1, t1) ) JumpNext()
	DoFail()
}

static void PPut()
{
	TypeCheck("i") ;
	Put(XInt(R0)) ;
	JumpNext()
}

static void PNl()
{
	Nl() ;
	JumpNext()
}

static void PTab()
{
	TypeCheck("i") ;
	Tab(XInt(R0)) ;
	JumpNext()
}

static void PWrite()
{
	GWriteTerm(R0, wNormal, false) ;
	JumpNext()
}

static void PWriteQ()
{
	GWriteTerm(R0, quoted, false) ;
	JumpNext()
}

static void PPrint()
{
	GWriteTerm(R0, print, false) ;
	JumpNext()
}

static void PDisplay()
{
	GWriteTerm(R0, display, true) ;
	JumpNext()
}

static void POp()
{
	TypeCheck("ia") ;
   	if( DefineOperator(XInt(R0), XAtomName(R1), R2) ) JumpNext()
   	DoFail()
}



/* STREAMS */

static void PSee()
{
	TypeCheck("a") ;
	See(XAtomName(R0)) ;
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
	TypeCheck("a") ;
	Tell(XAtomName(R0)) ;
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



/* COMPARING NUMBERS */

static void PEq()
{
	b0 = Evaluate(X0, &i0, &r0) ;
	b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 == i1 : i0 == r1 )
		   : ( b1 ? r0 == i1 : r0 == r1 ) ) JumpNext()
	DoFail()
}

static void PNe()
{
	b0 = Evaluate(X0, &i0, &r0) ;
	b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 != i1 : i0 != r1 )
		   : ( b1 ? r0 != i1 : r0 != r1 ) ) JumpNext()
	DoFail()
}

static void PLt()
{
	b0 = Evaluate(X0, &i0, &r0) ;
	b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 < i1 : i0 < r1 )
		   : ( b1 ? r0 < i1 : r0 < r1 ) ) JumpNext()
	DoFail()
}

static void PGt()
{
	b0 = Evaluate(X0, &i0, &r0) ;
	b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 > i1 : i0 > r1 )
		   : ( b1 ? r0 > i1 : r0 > r1 ) ) JumpNext()
	DoFail()
}

static void PLe()
{
	b0 = Evaluate(X0, &i0, &r0) ;
	b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 <= i1 : i0 <= r1 )
		   : ( b1 ? r0 <= i1 : r0 <= r1 ) ) JumpNext()
	DoFail()
}

static void PGe()
{
	b0 = Evaluate(X0, &i0, &r0) ;
	b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 >= i1 : i0 >= r1 )
		   : ( b1 ? r0 >= i1 : r0 >= r1 ) ) JumpNext()
	DoFail()
}



/* MISC STANDARD */

static void PEvent()
{
	TypeCheck("i") ;
	longjmp(eventHandler, XInt(R0)) ;
	JumpNext()
}

static void PTrace()
{
	trace = true ;
	JumpNext()
}

static void PNoTrace()
{
	trace = false ;
	JumpNext()
}

static void PDTrace()
{
	hideTrace = false ;
	JumpNext()
}

static void PDNoTrace()
{
	hideTrace = true ;
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

static void PSucc()
{
	VarValue2(t0, X0) ;
	VarValue2(t1, X1) ;
	if( IsInt(t0) )
	{
		i = XInt(t0) + 1 ;
		if( i < 1 || not UnifyWithAtomic(t1, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	elif( IsInt(t1) )
	{
		i = XInt(t1) - 1 ;
		if( i < 0 || not UnifyWithAtomic(t0, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	else TypeError("integer") ;
}


/* THREADS */

static void PNewThread()
{
	TypeCheck("aiaa") ;
	if( CreateThread(XAtom(R0), XInt(R1) Kb, userAtom, XAtom(R2), XAtom(R3)) == nil )
		DoFail()
	else JumpNext()
}

static void PTransferToThread()
{
	TypeCheck("av") ;
	TransferToThread(LookupThread(XAtom(R0))) ;
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
	TypeCheck("a") ;
	KillThread(LookupThread(XAtom(R0))) ;
	JumpNext()
}


/* MISC */

static void DoUserPredicate()
{
	if( A(1) == tNilAtom )
	{
		foreach(pr, ModulePredicates(SourceModule()), PredNextM(pr))
			if( not PredIsSys(pr) && PredNClauses(pr) > 0 ) break ;
		A(1) = cPt(pr) ;
	}
	if( ( pr = cPredicatePt(A(1)) ) == nil ) Jump(DiscardAndFail) ;
	b = Unify(X0, MakeCleanTerm(PredFunctor(pr))) ;	
	foreach(pr, PredNextM(pr), PredNextM(pr))
		if( not PredIsSys(pr) && PredNClauses(pr) > 0 ) break ;
	A(1) = cPt(pr) ;
	if( b ) JumpNext()
	DoFail()
}

static void DoSysPredicate()
{
	if( A(1) == tNilAtom )
	{
		foreach(pr, ModulePredicates(sysModule), PredNextM(pr))
			if( PredIsSys(pr) ) break ;
		A(1) = cPt(pr) ;
	}
	if( ( pr = cPredicatePt(A(1)) ) == nil ) Jump(DiscardAndFail) ;
	b = Unify(X0, MakeCleanTerm(PredFunctor(pr))) ;	
	foreach(pr, PredNextM(pr), PredNextM(pr)) if( PredIsSys(pr) ) break ;
	A(1) = cPt(pr) ;
	if( b ) JumpNext()
	DoFail()
}

static void PCode1()
{
	TypeCheck("a") ;
	ListAtomCode(XAtom(R0)) ;
	JumpNext()
}

static void PCode2()
{
	TypeCheck("ai") ;
	ListPredicateCode(LookupPredicate2(XAtomName(R0), XInt(R1))) ;
	JumpNext()
}

static void PListCPreds()
{
	ListCPredicates() ;
	JumpNext()
}

static void PNoIndex()
{
	TypeCheck("ai") ;
	PredNoIndex(LookupPredicate2(XAtomName(R0), XInt(R1))) = true ;
	JumpNext()
}

static void PResetOps()
{
	SetupInitialAtomPriorities() ;
	JumpNext()
}


/* */

void InstallCPredicates()
{
	/*consult*/
	/*reconsult*/

	InstallCPredicate("true", 0, PTrue) ;
	InstallCPredicate("fail", 0, PFail) ;
	InstallCPredicate("var", 1, PVar) ;
	InstallCPredicate("nonvar", 1, PVar) ;
	InstallCPredicate("atom", 1, PAtom) ;
	InstallCPredicate("integer", 1, PInt) ;
	InstallCPredicate("real", 1, PReal) ;
	InstallCPredicate("number", 1, PNumber) ;
	InstallCPredicate("atomic", 1, PAtomic) ;

	/*listing*/
	InstallCPredicate("$clause", 2, DoClause) ;
	InstallCPredicate("asserta", 1, PAsserta) ;
	InstallCPredicate("assertz", 1, PAssertz) ;
	InstallCPredicate("assert", 1, PAssertz) ;
	InstallCPredicate("$retract", 1, DoRetract) ;
	InstallCPredicate("functor", 3, PFunctor) ;
	InstallCPredicate("arg", 3, PArg) ;
	InstallCPredicate("name", 2, PName) ;

	InstallCPredicate("!", 0, PCut) ;
	InstallCPredicate("get_level", 1, PGetLevel) ;
	InstallCPredicate("cut", 1, PCut1) ;
	/*repeat*/
	/*,*/
	/*;*/
	/*call*/
	/* not */

	InstallCPredicate("=", 2, PUnify) ;
	/*\=*/
	InstallCPredicate("==", 2, PEqual) ;
	/*\==*/

	InstallCPredicate("get0", 1, PGet0) ;
	InstallCPredicate("get", 1, PGet) ;
	InstallCPredicate("skip", 1, PSkip) ;
	InstallCPredicate("read", 1, PRead) ;
	InstallCPredicate("read", 2, PRead2) ;
	InstallCPredicate("put", 1, PPut) ;
	InstallCPredicate("nl", 0, PNl) ;
	InstallCPredicate("tab", 1, PTab) ;
	InstallCPredicate("write", 1, PWrite) ;
	InstallCPredicate("writeq", 1, PWriteQ) ;
	InstallCPredicate("print", 1, PPrint) ;
	InstallCPredicate("display", 1, PDisplay) ;
	InstallCPredicate("op", 3, POp) ;

	InstallCPredicate("see", 1, PSee) ;
	InstallCPredicate("seeing", 1, PSeeing) ;
	InstallCPredicate("seen", 0, PSeen) ;
	InstallCPredicate("tell", 1, PTell) ;
	InstallCPredicate("telling", 1, PTelling) ;
	InstallCPredicate("told", 0, PTold) ;

	InstallCPredicate("is", 2, PIs) ;
	InstallCPredicate("=:=", 2, PEq) ;
	InstallCPredicate("=\\=", 2, PNe) ;	/* Use of escape '\' */
	InstallCPredicate("<", 2, PLt) ;
	InstallCPredicate(">", 2, PGt) ;
	InstallCPredicate("=<", 2, PLe) ;
	InstallCPredicate(">=", 2, PGe) ;

	InstallCPredicate("new_thread", 4, PNewThread) ;
	InstallCPredicate("transfer_to_thread", 2, PTransferToThread) ;
	InstallCPredicate("actual_thread", 1, PActualThread) ;
	InstallCPredicate("kill_thread", 1, PKillThread) ;
	
	InstallCPredicate("$event", 1, PEvent) ;
	InstallCPredicate("trace", 0, PTrace) ;
	InstallCPredicate("notrace", 0, PNoTrace) ;
	InstallCPredicate("$trace", 0, PDTrace) ;
	InstallCPredicate("$notrace", 0, PDNoTrace) ;

	InstallCPredicate("$user_mode", 0, PUserMode) ;
	InstallCPredicate("statistics", 0, PStatistics) ;
	InstallCPredicate("$code", 1, PCode1) ;
	InstallCPredicate("$code", 2, PCode2) ;
	InstallCPredicate("clist", 0, PListCPreds) ;
	InstallCPredicate("succ", 2, PSucc) ;
	InstallCPredicate("noindex", 2, PNoIndex) ;
	InstallCPredicate("$user_predicate", 1, DoUserPredicate) ;
	InstallCPredicate("$system_predicate", 1, DoSysPredicate) ;
	InstallCPredicate("reset_ops", 0, PResetOps) ;
}
