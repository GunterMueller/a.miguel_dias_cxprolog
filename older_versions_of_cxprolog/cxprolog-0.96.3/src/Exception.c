/*
 *   This file is part of the CxProlog system

 *   Exception.c
 *   by A.Miguel Dias - 2003/08/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* PRIVATE FUNCTIONS */

/* http://www.sju.edu/~jhodgson/prolog_proj/exceptions.html */

static void WriteException(Pt exc, Pt st)
{
	Pt e0, e1, i0, i1, s0, s1 ;
	if( (exc = Drf(exc))
		&& IsThisStruct(exc, errorFunctor)
		&& (e0 = Drf(XStructArg(exc, 0)))
		&& (e1 = Drf(XStructArg(exc, 1)))	
		&& IsThisStruct(e1, infoFunctor)
		&& (i0 = Drf(XStructArg(e1, 0)))
		&& (i1 = Drf(XStructArg(e1, 1)))	
		&& IsAtom(e0)
		&&	(	(i0 == tEmptyAtom && (s0 = s1 = tNilAtom))
				||
				(IsThisStruct(i0, slashFunctor)
				&& (s0 = Drf(XStructArg(i0, 0)))
				&& (s1 = Drf(XStructArg(i0, 1)))	
				&& IsAtom(s0) && IsInt(s1))
			)
		&& IsAtom(i1) ) {
			if( i0 == tEmptyAtom )
				WriteErr("{%s: %s.}\n",
							XAtomName(e0),
							XAtomName(i1)) ;
			else
				WriteErr("{%s (%s/%d): %s.}\n",
							XAtomName(e0),
							XAtomName(s0),
							XInt(s1),
							XAtomName(i1)) ;
	}
	elif( st == nil )
		WriteErr("{EXCEPTION: %s.}\n", TermAsStr(exc)) ;
	else
		WriteErr("{UNCAUGHT EXCEPTION: %s.}\n", TermAsStr(exc)) ;
#if 0
	if( errno != 0 ) {
		WriteErr("[[[%s]]]\n", sys_errlist[errno]) ;
		errno = 0 ;
	}
#endif
	if( st != nil ) {
		st = Drf(st) ;
		if( IsList(st) ) {		
			Pt t ;
			WriteErr(" ### STACK TRACE ###\n") ;
			for( t = st ; IsList(t) ; t = Drf(XListTail(t)) )
				WriteErr("    %s\n", SubtermAsStr(XListHead(t), st)) ;
		}
	}		
}

Pt BuildExceptionTermV(CharPt kind, CharPt fmt, va_list v)
{
	Pt prTerm, infoTerm ;
	CharPt mesg = GStrFormatV(fmt, v) ;
	PredicatePt pr = CurrCPred() ;	
	
	if( pr == nil ) prTerm = tEmptyAtom ;
	else prTerm = MakeSlashTerm(ClearNDetermFunctor(PredFunctor(pr))) ;
		
	infoTerm = MakeBinStruct(infoFunctor, prTerm, MakeTempAtom(mesg)) ;

	return MakeBinStruct(errorFunctor, MakeTempAtom(kind), infoTerm) ;
}

static void PushExceptionPoint(Pt protected, Pt exceptionOut, Pt handler,
															Pt stackTraceOut)
{
	if( stackTraceOut != nil )		 /* nil means no stack trace wanted */
		TestVar(stackTraceOut) ;
	X0 = protected ;
	X1 = exceptionOut ;
	X2 = handler ;
	X3 = stackTraceOut ;
	X4 = MakeInt(CallLevel()) ;		/* @@@ Save C runtime stack level */
	X5 = cPt(P) ;					/* Save P */
	CreateChoicePoint(&DiscardAndFail, 6) ;
	R = B ;							/* Set first in exception chain */
}

static void PopExceptionPoint(void)
{
	CP = Rf(CP) ;					/* Restore CP */
	P = cHdl(RA(5)) ;				/* Restore P */
	if( B == R ) Discard() ;		/* If in the deterministic case, discard */
	R = Rf(R) ;						/* Pop exception point */
}

void Throw(Pt exc)
{
	Pt st, t ;
/* Part of global stack will be destroyed, so save the term here */
	exc = AllocateTermForAssign(exc) ;
/* Same for stack trace */
	if( debug_flag == 0 ) 
		st = tNilAtom ;
	else st = AllocateTermForAssign(BuildStackTrace()) ;
/* Search for handler */	
	while( !EndOfChain(R) ) {
		SetChoicePoint(R) ;
		RestoreState(5) ;	/* yes 5, not 6 */
		Discard() ;			/* Restore original "catch-time" choice point */
		if( Unifiable(exc, X1) ) goto found ;
	}
/* HANDLER NOT FOUND  */
	WriteException(exc, st) ;
	ReleaseTerm(exc) ;
	ReleaseTerm(st) ;
	EventRestart() ;

found: /* HANDLER FOUND */
	if( X3 != nil )	{			/* Is stack trace wanted? */
		Pt t = ZPushTerm(st) ;
		ReleaseTerm(st) ;		
		Ensure( Unify(t, X3) ) ;
	}
	DebugThrow() ;
	t = ZPushTerm(exc) ;
	ReleaseTerm(exc) ;
	if( !Unify(t, X1) ) InternalError("Throw (2)") ;
	X0 = X2 ;					/* Prepare to execute Handler */
	P = &ExecuteVar ;
	EventShrink(XInt(X4)) ;		/* Cut C runtime stack */
}

#if COMPAT_0_90_3
void ThrowPrologException(Pt t)
{
    Throw(t) ;
}

void ThrowPrologExceptionMesgV(CharPt kind, CharPt fmt, va_list v)
{
    Throw(BuildExceptionTermV(kind, fmt, v)) ;
}

void ThrowPrologExceptionMesg(CharPt kind, CharPt fmt, ...)
{
    va_list v ;
    va_start(v, fmt) ;
    ThrowPrologExceptionMesgV(kind, fmt, v) ;
}
#endif


/* CXPROLOG C'BUILTINS */

static void CatchPopAux()
{
	PopExceptionPoint() ;
	JumpNext() ;
}
static Pt CatchCode[2] =
{
	cPt(WordsOf(Environment)),	/* Anyway, doesn't matter because B is on top */
	cPt(CatchPopAux)
} ;

static void PCatch()
{	/* X0 = ProtectedGoal, X1 = ExceptionTerm, X2 = StackTrace, X3 = Handler */
	PushExceptionPoint(X0, X1, X3, X2) ;
	CP = CatchCode + 1 ;
	Jump(ExecuteVar) ;			/* Execute ProtectedGoal */
}

static void PCatch3()
{	/* X0 = ProtectedGoal, X1 = ExceptionTerm, X2 = Handler */
	PushExceptionPoint(X0, X1, X2, nil) ;
	CP = CatchCode + 1 ;
	Jump(ExecuteVar) ;			/* Execute ProtectedGoal */
}

static void POnException()
{	/* X0 = ExceptionTerm, X1 = ProtectedGoal, X2 = Handler */
	PushExceptionPoint(X1, X0, X2, nil) ;
	CP = CatchCode + 1 ;
	Jump(ExecuteVar) ;			/* Execute ProtectedGoal */
}

static void PThrow()
{
	Throw(X0) ;
	InternalError("PThrow") ;
}

static void PRaiseException()
{
	Throw(X0) ;
	InternalError("PRaiseException") ;
}

static void PWriteException()
{
	WriteException(X0, nil) ;
	JumpNext() ;
}


/* INIT */

void ExceptionsInit()
{	
	InstallCBuiltinPred("catch", 3, PCatch3) ;
	InstallCBuiltinPred("catch", 4, PCatch) ;
	InstallCBuiltinPred("on_exception", 3, POnException) ;
	InstallCBuiltinPred("throw", 1, PThrow) ;
	InstallCBuiltinPred("raise_exception", 1, PRaiseException) ;
	InstallCBuiltinPred("write_exception", 1, PWriteException) ;
}