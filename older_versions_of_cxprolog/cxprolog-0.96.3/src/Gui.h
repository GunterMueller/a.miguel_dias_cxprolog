/*
 *   This file is part of the CxProlog system

 *   Gui.h
 *   by A.Miguel Dias - 2006/09/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Gui_
#define _Gui_

typedef enum { fNone, fJava, fWxWidgets } ForeignInterface ;

void RegisterForeignInterface(ForeignInterface f) ;
void SetDefaultForeignInterface(ForeignInterface f) ;

/* INIT */
void GuiInit(void) ;

#endif
