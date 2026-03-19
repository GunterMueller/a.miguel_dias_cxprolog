/*
 *   This file is part of the CxProlog system

 *   ContextsInsts2.ch
 *   by A.Miguel Dias - 2006/05/07
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

/* This file included in "Instructions.c" */

/* AUXILIARY FUNCTIONS */

static PredicatePt CtxSearch(FunctorPt f) /* for the context instructions */
{
	Pt c ;
	if( FunctorIsBuiltin(f) ) {
		return FunctorPreds(f) ;
	}
	for( c = C ; c != tNilAtom ; c = CtxNext(c) ) { /* no need to deref */
		PredicatePt pr = FindPredicateInUnit(CtxTopUnit(c), f) ;
		if( pr != nil && (PredIsVisible(pr) || forceVisibility_flag) ) {
			C = c ;
			return pr ;
		}
	}
	if( undefWarnings_flag )
		Warning("Predicate '%s' is not visible in context %s",
						FunctorNameArity(f), TermAsStr(C)) ;
	return undefPred ;
}


/* CONTEXT 2 INSTRUCTIONS */

static void UndefPredInst()
{
	AllocEnv() ;
	CP = ctxCodeSegm + UndefPredEndPos ;

	ExecutePred(CtxSearch(LookFunctor())) ;	
	JumpNext() ;
}

static void UndefPredEndInst()
{
	CH = CtxNext(CH) ;
	Jump(DeallocProceed) ;
}

static void ImportInst()
{
	InternalError("ImportInst not available") ;	
}

static void ImportEndInst()
{
	InternalError("ImportEndInst not available") ;	
}

static void CtxSwitchInst()
{
	InternalError("CtxSwitchInst not available") ;	
}

static void CtxExtensionInst()
{
	AllocEnv() ;
	CP = ctxCodeSegm + CtxExtensionEndPos ;
	Y(OutPerm(0)) = CH ;	

	CH = tNilAtom ;
	Q.u = TermToUnit(X0, H) ; /* Beware the H */
	PushH(Q.u) ;	/* Unit is a ref stored in the global stack */
	PushH(X0) ;		/* Link unit-term in the context list */
	PushH(C) ;
	C = TagList(H-2) ;
	ExecutePred(CtxSearch(PrepareCall(X1))) ;	
	JumpNext() ;
}

static void CtxExtensionEndInst()
{
	C = CtxNext(C) ;
	CH = Y(OutPerm(0)) ;
	Jump(DeallocProceed) ;
}

static void CtxEmptyInst()
{
	AllocEnv() ;
	CP = ctxCodeSegm + CtxEmptyEndPos ;
	Y(OutPerm(0)) = C ;	

	C = tNilAtom ;
	ExecutePred(CtxSearch(PrepareCall(X0))) ;	
	JumpNext() ;
}

static void CtxEmptyEndInst()
{
	C = Y(OutPerm(0)) ;
	Jump(DeallocProceed) ;
}

static void CtxDownInst()
{
	if( C == tNilAtom )
		Error("Cannot use 'down/1' on the emtpy context") ;
	AllocEnv() ;
	CP = ctxCodeSegm + CtxDownEndPos ;
	Y(OutPerm(0)) = C ;

	C = XListTail(C) ;
	ExecutePred(CtxSearch(PrepareCall(X0))) ;
	JumpNext() ;
}

static void CtxDownEndInst()
{
	C = Y(OutPerm(0)) ;
	Jump(DeallocProceed) ;
}

static void HCtxPushInst()
{
	AllocEnv() ;
	CP = ctxCodeSegm + HCtxPushEndPos ;

	PushH(C) ;
	PushH(CH) ;
	CH = TagList(H-2) ;
	ExecutePred(LookupPredicate(PrepareCall(X0))) ;
	JumpNext() ;
}

static void HCtxPushEndInst()
{
	CH = CtxNext(CH) ;
	Jump(DeallocProceed) ;
}

static void HCtxEnterInst()
{
	InternalError("HCtxEnterInst not available") ;	
}

static void HCtxEnterEndInst()
{
	InternalError("HCtxEnterEndInst not available") ;	
}

static void VisibleEmptyPredInst()
{
	Jump(EmptyPred) ;
}

static void VisibleDynamicEnterInst()
{
	Jump(DynamicEnter) ;
}

static void VisibleDynamicElseInst()
{
	Jump(DynamicElse) ;
}
