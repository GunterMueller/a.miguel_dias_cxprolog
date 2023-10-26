/*
 *   This file is part of the CxProlog system

 *   Contexts1.h
 *   by A.Miguel Dias - 2006/05/07
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Contexts1_
#define _Contexts1_

#define PredIsExtern(pr) ( PredIsImported(pr) )

#define ppImported		true
#define ppVisible		true
#define ppGOpen			false
#define ppGClose		false
#define ppPrivate		true	

#define CtxTopUnit(c)	( cUnitPt(XListArg(c, -1)) )
#define CtxTop(c)		( XListHead(c) )
#define CtxNext(c)		( XListTail(c) )

#define NoCurrUnit()	( C == tNilAtom )
#define CurrUnit()		( NoCurrUnit() ? bottomUnit : CtxTopUnit(C) )
#define Z(idx)			( XStructArg(CtxTop(C), idx) )
#define OutParam(idx)	( idx )

PredicatePt LookupPredicateForMetaCall(FunctorPt f) ;
Bool DebugCallOverride(PredicatePt pr) ;
void ContextsInit(void) ;
void ContextsInit2(void) ;

#endif
