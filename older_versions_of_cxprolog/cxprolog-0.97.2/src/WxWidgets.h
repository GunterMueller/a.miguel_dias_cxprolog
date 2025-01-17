/*
 *   This file is part of the CxProlog system

 *   WxWidgetsBase.h
 *   by A.Miguel Dias - 2005/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _WxWidgets_
#define _WxWidgets_

#if USE_WXWIDGETS

/* WXOBJ */

typedef struct WxObj {
	ExtraDef(WxObj) ;
	VoidPt value ;
} WxObj, *WxObjPt ;

/* These are the only functions that can be called from the GUI thread. */
void WxPostEvent(CharPt fmt, ...) ;
void WxDeleted(VoidPt obj) ;	/* pre: obj has just been deleted */

/* WX object terms */
WxObjPt LookupWxObj(VoidPt obj) ; /* pre: obj is an alive object */
Pt MakeWxObj(VoidPt obj) ;	/* pre: obj is an alive object */
VoidPt XTestWxObj(Pt t) ;	/* t may represent an alive or dead object */

/* Invoking GUI services from the Prolog thread */
void WxGuiCall(Fun f) ;

/* Installing WX c built-in predicates */
void InstallWxGuiBuiltinPredAux(CharPt n, int a, Fun fINIT, Fun fGUI, Fun fEND) ;
#define InstallWxGuiBuiltinPred(n, a, f)		\
					InstallWxGuiBuiltinPredAux(n, a, f##INIT, f##GUI, f##END)

void WxSetEventNotifier(Fun f) ;
Pt WxGetEvent(void) ;
int WxHowManyEvents(void) ;
void WxDiscardAllEvents(void) ;

#endif

void WxWidgetsCallInst(void) ;
void WxWidgetsInit(void) ;

#endif
