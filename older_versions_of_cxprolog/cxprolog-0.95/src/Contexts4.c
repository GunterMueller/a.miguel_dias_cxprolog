/*
 *   This file is part of the CxProlog system

 *   Contexts4.c
 *   by A.Miguel Dias - 2007/05/25
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


#if CONTEXTS == 4


/* PUBLIC FUNCTIONS */

PredicatePt LookupPredicateForMetaCall(FunctorPt f)
{
	if( FunctorIsBuiltin(f) )	/* Does not change CH */
		return FunctorPreds(f) ;
	if( CH == tNilAtom || CtxTop(CH) == tNilAtom )
		return LookupPredicateInUnit(bottomUnit, f) ;
	return LookupPredicate(f) ;
}

Bool DebugCallOverride(PredicatePt pr)
{
	static int i = 1 ;
	CharPt s, t ;
	if( /*(CH == tNilAtom || C == tNilAtom || UnitAtom(CtxTopUnit(C)) == LookupAtom("main")) &&*/ PredIsBuiltin(pr) /*&& PredName(pr)[3] != 'u'*/ )
		return true ;
	t =   PredIsBuiltin(pr) ? "BB"
		: PredIsUndefined(pr) ? "UU"
		: PredIsLocal(pr) ? "nh"
		: (PredIsXOver(pr) && PredIsVisible(pr)) ? "ov"
		: PredIsXOver(pr) ? "oh"
	 	: PredIsVisible(pr) ? "nv"
		: "??" ;
	WriteStd("   -------- %s\n", TermAsStr(CH)) ;
	WriteStd("   %s (%4ld) %s: ", t, i++, TermAsStr(C)) ;
	HSave() ;
	s = TermAsStr(MakeStruct(PredFunctor(pr), X)) ;
	HRestore() ;
	WriteStd("%s", s) ;
	WriteStd(" ? ") ;
	for(;;)
		switch( GetCharCommand(nil) ) {
			case '\n': {					/* creep */
				return true ;
			}
			case 'a': {					/* abort */
				WriteStd("%% Aborted.\n") ;
				EventRestart() ;
				return true ;
			}
			case EOF: {
				WriteEOF() ;
				/* fall through */
			}
			case 'e': {					/* exit */
				WriteStd("%% Bye.\n") ;
				EventExit() ;
				return true ;
			}
			default: {
				WriteStd("%12sOptions: <ret>-creep  a-abort  e-exit ? ", "") ;
				break ;
			}
		}
}


/* AUXILIARY FUNCTIONS */

static void SetVisibleProperty(PredicatePt pr, Bool visible)
{
	PredIsVisible(pr) = visible ;
	if( !PredIsVisible(pr) && !PredIsXOver(pr) ) { /* xnew + hidden == LOCAL */
		PredIsIndexable(pr) = true ;
		PredHasDynamicChain(pr) = false ;
	}
	else {
		PredIsIndexable(pr) = false ;	/* @@ WILL CHANGE */
		if( !PredHasDynamicChain(pr) ) {
			PredHasDynamicChain(pr) = true ;
			PredIsLogical(pr) = true ;
		}
	}
}

static void SetXOverProperty(PredicatePt pr, Bool xover)
{
	PredIsXOver(pr) = xover ;
	if( !PredIsVisible(pr) && !PredIsXOver(pr) ) { /* xnew + hidden == LOCAL */
		PredIsIndexable(pr) = true ;
		PredHasDynamicChain(pr) = false ;
	}
	else {	
		PredIsIndexable(pr) = false ;	/* @@ WILL CHANGE */
		if( !PredHasDynamicChain(pr) ) {
			PredHasDynamicChain(pr) = true ;
			PredIsLogical(pr) = true ;
		}
	}
}

static void SetVisible(PredicatePt pr)
{
	ChangingCurrUnit("declaring visible", pr) ;

	if( PredIsBuiltin(pr) )
		DatabaseError("Cannot set '%s' visible because it is a built-in",
												PredNameArity(pr)) ;
	elif( PredHandleConsult(pr, !PredIsVisible(pr)) )
		/* nothing */ ;
	elif( PredHasClauses(pr) && !PredIsVisible(pr) )
		DatabaseError("Cannot set '%s' visibe because it already has clauses",
												PredNameArity(pr)) ;
	SetVisibleProperty(pr, true) ;
//	if( PredIsUndefined(pr) ) {
		BindPredicateAsEmpty(pr) ;
		PredWasAbolished(pr) = false ;
//	}
}

static void SetHidden(PredicatePt pr)
{
	ChangingCurrUnit("declaring hidden", pr) ;

	if( PredIsBuiltin(pr) )
		DatabaseError("Cannot set '%s' hidden because it is a built-in",
												PredNameArity(pr)) ;
	elif( PredHandleConsult(pr, PredIsVisible(pr)) )
		/* nothing */ ;
	elif( PredHasClauses(pr) && PredIsVisible(pr) )
		DatabaseError("Cannot set '%s' hidden because it already has clauses",
												PredNameArity(pr)) ;
	SetVisibleProperty(pr, false) ;
//	if( PredIsUndefined(pr) ) {
		BindPredicateAsEmpty(pr) ;
		PredWasAbolished(pr) = false ;
//	}
}

static void SetXOver(PredicatePt pr, Bool xover)
{
	CharPt op = xover ? "xover" : "xnew" ;
	ChangingCurrUnit(xover ? "declaring xover" : "declaring xnew", pr) ;

	if( PredIsBuiltin(pr) )
		DatabaseError("Cannot set '%s' %s because it is a built-in",
												PredNameArity(pr), op) ;
	elif( PredHandleConsult(pr, PredIsXOver(pr) != xover) )
		/* nothing */ ;
	elif( PredHasClauses(pr) && PredIsXOver(pr) != xover )
		DatabaseError("Cannot set '%s' %s because it already has clauses",
												PredNameArity(pr), op) ;
	SetXOverProperty(pr, xover) ;
//	if( PredIsUndefined(pr) ) {
		BindPredicateAsEmpty(pr) ;
		PredWasAbolished(pr) = false ;
//	}
}


/* CXPROLOG C'BUILTINS */

static void PReserved()
{
	Error("Reserved predicate name") ;
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

static Size PHiddenAux(FunctorPt f)
{
	SetHidden(LookupPredicate(f)) ;
	return 1 ;
}
static void PHidden()
{
	ForEachInSpec(X0, PHiddenAux, false) ;
	JumpNext() ;
}

static Size PXNewAux(FunctorPt f)
{
	SetXOver(LookupPredicate(f), false) ;
	return 1 ;
}
static void PXNew()
{
	ForEachInSpec(X0, PXNewAux, false) ;
	JumpNext() ;
}

static Size PXOVerAux(FunctorPt f)
{
	SetXOver(LookupPredicate(f), true) ;
	return 1 ;
}
static void PXOver()
{
	ForEachInSpec(X0, PXOVerAux, false) ;
	JumpNext() ;
}

static void PContexts()
{
	MustBe( UnifyWithNumber(X0, MakeInt(4)) ) ;
}

void ContextsInit()	/* Forces NoCurrUnit() */
{
	C = tNilAtom ;
	CH = tNilAtom ;
}

void ContextsInit2()
{
	InstallCBuiltinPred("visible", 1, PVisible) ;
	InstallCBuiltinPred("hidden", 1, PHidden) ;
	InstallCBuiltinPred("xnew", 1, PXNew) ;
	InstallCBuiltinPred("xover", 1, PXOver) ;
	InstallCBuiltinPred(">>", 2, InstDecode(CtxExtension)) ;
	InstallCBuiltinPred("call_on_empty_context", 1, InstDecode(CtxEmpty)) ;
	InstallCBuiltinPred("callb", 1, InstDecode(CtxDown)) ;

	InstallCBuiltinPred("gopen", 1, PReserved) ;
	InstallCBuiltinPred("gclose", 1, PReserved) ;
	InstallCBuiltinPred("import", 2, PReserved) ;

	InstallCBuiltinPred("$$_contexts", 1, PContexts) ;
}

#endif
