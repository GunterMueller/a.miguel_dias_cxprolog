/*
 *   This file is part of the CxProlog system

 *   WXWidgets.h
 *   by A.Miguel Dias - 2005/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _WXWidgets_
#define _WXWidgets_

#if USE_WXWIDGETS

/* These are the only functions that can be called from the GUI thread. */
void WXPostEvent(CharPt fmt, ...) ;
void WXDeleted(VoidPt obj) ;	/* pre: obj has just been deleted */

/* WX object terms */
Pt MakeWXObj(VoidPt obj) ;	/* pre: obj is an alive object */
VoidPt XTestWXObj(Pt t) ;	/* t may represent an alive or dead object */

/* Invoking GUI services from the Prolog thread */
void WXGuiCall(Fun f) ;

/* Installing WX c built-in predicates */
void InstallWXCBuiltinPredAux(CharPt n, int a, Fun fINIT, Fun fGUI, Fun fEND) ;
#define InstallWXCBuiltinPred(n, a, f)		\
					InstallWXCBuiltinPredAux(n, a, f##INIT, f##GUI, f##END)
#endif

void WXSetEventNotifier(Fun f) ;
Pt WXGetEvent(void) ;
int WXHowManyEvents(void) ;
void WXDiscardAllEvents(void) ;
void WXWidgetsCallInst(void) ;
void WXWidgetsInit(void) ;

#endif
