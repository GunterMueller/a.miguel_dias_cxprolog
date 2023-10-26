/*
 *   This file is part of the CxProlog system

 *   Exception.c
 *   by A.Miguel Dias - 2003/08/20
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

static FunctorPt errorFunctor ;

Pt BuildExceptionTerm(CharPt kind, CharPt fmt, VoidPt v)
{
	Str1K mesg ;
	PredicatePt pr ;
	Pt prTerm ;

	vsprintf(mesg, fmt, v) ;
	
	if( P == &NotRunning ) pr = nil ;
	else pr = CurrCPred() ;

	if( pr == nil ) prTerm = tEmptyAtom ;
	else prTerm = MakePredSlashTerm(pr) ;
	
	return MakeTriStruct(errorFunctor, MakeTempAtom(kind), prTerm, MakeTempAtom(mesg)) ;
}

void WriteExceptionTerm(Pt exc)
{
	exc = Drf(exc) ;
	if( IsThisStruct(exc, errorFunctor) ) {
		Hdl a = XStructArgs(Drf(exc)) ;	
		if( a[1] == tEmptyAtom )
			WriteErr("{%s: %s.}\n",
						XAtomName(a[0]),
						XAtomName(a[2])) ;
		else
			WriteErr("{%s (%s/%d): %s.}\n",
						XAtomName(a[0]),
						XAtomName(XStructArg(a[1],0)),
						XInt(XStructArg(a[1],1)),
						XAtomName(a[2])) ;
	}
	else
		WriteErr("{UNCAUGHT EXCEPTION: %s.}\n", TermAsStr(exc)) ;
}

static Pt BuildStackTrace(EnvironmentPt ini, EnvironmentPt lim)
{
	PredicatePt pr ;
	EnvironmentPt saveE = E ;
	Pt list = tNilAtom ;
	Hdl h = &list + 1 ;

/* In case the current predicate is a C predicate... */
	if( (pr = CurrCPred()) != nil ) {
		h[-1] = MakeList(MakePredSlashTerm(pr), tNilAtom) ;
		h = H ;
	}
	
/* The rest of the stack */
	for( E = ini ; E != lim ; E = Ef(E) ) 
		if( Ef(P)[-1] == FAllocate )  { /* is this a regular environment? */
			PredicatePt pr = ClauseOwner(ClauseFromAllocate(Ef(P) - 1)) ;
			h[-1] = MakeList(MakePredSlashTerm(pr), tNilAtom) ;
			h = H ;
		}
	E = saveE ;
	return list ;
}

void Throw(Pt exc)
{
	ChoicePointPt newB ;
	Pt t ;
	EnvironmentPt iniE = E ;

/* part of global stack will be destroyed, so save the term here */
	exc = AllocateTermForAssign(exc) ;
	
	D = cPt(B) ;			/* save current choice point */
/* search for handler */
	for( B = R ; B != nil ; B = R ) {
		RestoreState(5) ;
		SetChoicePoint(B) ;
		if( Unifiable(exc, X1) ) goto found ;
	}
	WriteExceptionTerm(exc) ;
	EventRestart() ;

found:
	newB = Bf(B) ;
	B = cChoicePointPt(D) ;	/* restore current choice point */
	CutTo(newB) ;			/* cut prolog local stack */
	
/* By construction BuildStackTrace never overflows, if called from here */
	if( X2 != nil )			/* stack trace wanted? */
		if( !Unify(BuildStackTrace(iniE, Ef(E)), X2) )
			InternalError("Throw (1)") ;

/* This must be placed after the last use of iniE, because stacks can grow now */
	t = ZPushTerm(exc) ;
	ReleaseTerm(exc) ;
	if( !Unify(t, X1) ) InternalError("Throw (2)") ;

	X0 = X3 ;				/* prepare to execute Handler */
	P = &ExecuteVar ;
	EventShrink(XInt(X4)) ; /* cut C runtime stack */
}


/* CXPROLOG C'BUILTINS */

static void CatchPopAux()
{
	P = Rf(P) ;
	CP = Rf(CP) ;
	E = Rf(E) ;
	R = Rf(R) ;	/* pop exception point */
	JumpNext()
}
static Pt CatchCode[2] =
{
	cPt(WordsOf(Environment)),
	cPt(CatchPopAux)
} ;
static void PCatch()
{	/* X0 = ProtectedGoal, X1 = ExceptionTerm, X2 = StackTrace, X3 = Handler */
	if( X2 != nil ) XTestVar(X2) ;	/* nil means no stack trace wanted */
	X4 = MakeInt(CallLevel()) ;		/* save C runtime stack level */
	D = cPt(B) ;					/* save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - 5) - 1 ; /* push exception point */
	SaveState(cChoicePointPt(D), P, 5) ;
	R = B ;
	B = cChoicePointPt(D) ;		/* restore current choice point */
	E = cEnvironmentPt(R) + 1 ;	/* Sets new top of stack. Assumes CP[-1] = WordsOf(Environment). */
	CP = CatchCode + 1 ;
	Jump(ExecuteVar)			/* execute ProtectedGoal */
}

static void PCatch3()
{	/* X0 = ProtectedGoal, X1 = ExceptionTerm, X2 = Handler */
	static Pt Catch = InstEncode(PCatch) ;
/* swap X0 with X1 */
	X3 = X2 ;
	X2 = nil ; /* nil means no stack trace wanted */
	Jump(Catch)
}

static void POnException()
{	/* X0 = ExceptionTerm, X1 = ProtectedGoal, X2 = Handler */
	static Pt Catch = InstEncode(PCatch) ;
/* swap X0 with X1 */
	D = X0 ;
	X0 = X1 ;
	X1 = D ;
	X3 = X2 ;
	X2 = nil ; /* nil means no stack trace wanted */
	Jump(Catch)
}

static void PThrow()
{
	Throw(X0) ;
}

static void PRaiseException()
{
	Throw(X0) ;
}

void ExceptionsInit()
{
	errorFunctor = LookupFunctorByName("error", 3) ;
	
	InstallCBuiltinPred("catch", 3, PCatch3) ;
	InstallCBuiltinPred("catch", 4, PCatch) ;
	InstallCBuiltinPred("on_exception", 3, POnException) ;
	InstallCBuiltinPred("throw", 1, PThrow) ;
	InstallCBuiltinPred("raise_exception", 1, PRaiseException) ;
}
