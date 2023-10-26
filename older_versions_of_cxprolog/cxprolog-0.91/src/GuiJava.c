/*
 *   This file is part of the CxProlog system

 *   GuiJava.c
 *   by A.Miguel Dias - 2004/09/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

 *   it under the terms of the GNU General Public License as published by
 *   CxProlog is free software; you can redistribute it and/or modify
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

#if GUI_JAVA

void GuiJavaInit()
{
	if( !OSExists(PREFIX "/lib/cxprolog/CxProlog.class") ||
		!OSExists(PREFIX "/lib/cxprolog/java.pl") )
		FatalError("GuiJava - cannot access '%s'", PREFIX "/lib/cxprolog/") ;
	ZBasicLoadFile(PREFIX "/lib/cxprolog/java.pl") ;
}

#else

void GuiJavaInit()
{
    /* nothing */
}

#endif /* GUI_JAVA */
