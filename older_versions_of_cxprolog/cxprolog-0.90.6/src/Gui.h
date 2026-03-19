/*
 *   This file is part of the CxProlog system

 *   Gui.h
 *   by A.Miguel Dias - 2004/09/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _GUI_
#define _GUI_

#if !GUI_JAVA && !GUI_WXWIDGETS && !GUI_UNKNOWN
#define GUI_UNKNOWN	1
#endif

 /* INIT */
void GuiInit(void) ;

#endif
