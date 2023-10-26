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


/* PRIVATE FUNCTIONS */

static Pt BuildStackTrace(EnvironmentPt ini, EnvironmentPt lim) /* for catch/4 */
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
		if( Ef(P)[-1] == FAllocate )  { /* Is this a regular environment? */
			PredicatePt pr = ClauseOwner(ClauseFromAllocate(Ef(P) - 1)) ;
			h[-1] = MakeList(MakePredSlashTerm(pr), tNilAtom) ;
			h = H ;
		}
	E = saveE ;
	return list ;
}

static void WriteExceptionTerm(Pt exc)
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

static Pt BuildExceptionTermV(CharPt kind, CharPt fmt, VoidPt v)
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

static void PushExceptionPoint(Pt protected, Pt exceptionOut, Pt handler,
															Pt stackTraceOut)
{
	if( stackTraceOut != nil )		 /* nil means no stack trace wanted */
		XTestVar(stackTraceOut) ;
	X0 = protected ;
	X1 = exceptionOut ;
	X2 = handler ;
	X3 = stackTraceOut ;
	X4 = MakeInt(CallLevel()) ;		/* Save C runtime stack level */
	X5 = cPt(P) ;					/* Save P */
	D = cPt(B) ;					/* Save current choice point */
	B = cChoicePointPt(TopOfLocalStack() - 6) - 1 ; /* Push exception point */
	SaveState(cChoicePointPt(D), &DiscardAndFail, 6) ;
	R = B ;							/* Set first in exception chain */
}

static void PopExceptionPoint(void)
{
	CP = Rf(CP) ;					/* Restore CP */
	P = cHdl(RA(5)) ;				/* Restore P */
	if( B == R ) Discard() ;		/* If in the deterministic case, discard */
	R = Rf(R) ;						/* Pop exception point */
}

static void Throw(Pt exc)
{
	EnvironmentPt entryE = E ;
	Pt t ;
/* Part of global stack will be destroyed, so save the term here */
	exc = AllocateTermForAssign(exc) ;
/* Search for handler */	
	while( R != nil ) {
		SetChoicePoint(R) ;
		RestoreState(5) ;
		Discard() ;			/* Restore original "catch-time" choice point */
		if( Unifiable(exc, X1) ) goto found ;
	}
/* HANDLER NOT FOUND  */
	WriteExceptionTerm(exc) ;
	ReleaseTerm(exc) ;
	EventRestart() ;

found: /* HANDLER FOUND */
/* By construction BuildStackTrace never overflows, if called from here */
	if( X3 != nil )				/* Is stack trace wanted? */
		if( !Unify(BuildStackTrace(entryE, Ef(E)), X2) )
			InternalError("Throw (1)") ;
/* This must be placed after the last use of entryE, because stacks will grow now */
	t = ZPushTerm(exc) ;
	ReleaseTerm(exc) ;
	if( !Unify(t, X1) ) InternalError("Throw (2)") ;
	X0 = X2 ;					/* Prepare to execute Handler */
	P = &ExecuteVar ;
	EventShrink(XInt(X4)) ;		/* Cut C runtime stack */
}


/* MAIN OPERATIONS */

void ThrowPrologException(Pt t)
{
	Throw(t) ;
}

void ThrowPrologExceptionMesgV(CharPt kind, CharPt fmt, VoidPt v)
{
	Throw(BuildExceptionTermV(kind, fmt, v)) ;
}

void ThrowPrologExceptionMesg(CharPt kind, CharPt fmt, ...)
{
	va_list p ;	
	va_start(p, fmt) ;
	ThrowPrologExceptionMesgV(kind, fmt, p) ;
}


/* CXPROLOG C'BUILTINS */

static void CatchPopAux()
{
	PopExceptionPoint() ;
	JumpNext()
}
static Pt CatchCode[2] =
{
	cPt(WordsOf(Environment)),
	cPt(CatchPopAux)
} ;

static void PCatch()
{	/* X0 = ProtectedGoal, X1 = ExceptionTerm, X2 = StackTrace, X3 = Handler */
	PushExceptionPoint(X0, X1, X3, X2) ;
	CP = CatchCode + 1 ;
	Jump(ExecuteVar)			/* Execute ProtectedGoal */
}

static void PCatch3()
{	/* X0 = ProtectedGoal, X1 = ExceptionTerm, X2 = Handler */
	PushExceptionPoint(X0, X1, X2, nil) ;
	CP = CatchCode + 1 ;
	Jump(ExecuteVar)			/* Execute ProtectedGoal */
}

static void POnException()
{	/* X0 = ExceptionTerm, X1 = ProtectedGoal, X2 = Handler */
	PushExceptionPoint(X1, X0, X2, nil) ;
	CP = CatchCode + 1 ;
	Jump(ExecuteVar)			/* Execute ProtectedGoal */
}

static void PThrow()
{
	Throw(X0) ;
}

static void PRaiseException()
{
	Throw(X0) ;
}


/* INIT */

void ExceptionsInit()
{
	errorFunctor = LookupFunctorByName("error", 3) ;
	
	InstallCBuiltinPred("catch", 3, PCatch3) ;
	InstallCBuiltinPred("catch", 4, PCatch) ;
	InstallCBuiltinPred("on_exception", 3, POnException) ;
	InstallCBuiltinPred("throw", 1, PThrow) ;
	InstallCBuiltinPred("raise_exception", 1, PRaiseException) ;
}
