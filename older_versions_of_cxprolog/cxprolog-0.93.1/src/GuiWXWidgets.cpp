/*
 *   This file is part of the CxProlog system

 *   GuiWXWidgets.cpp
 *   by A.Miguel Dias - 2005/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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


#if USE_WXWIDGETS
#	include <wx/wx.h>
#endif

extern "C" {
#	include "CxProlog.h"
}

#if USE_WXWIDGETS

#if !wxUSE_GUI
#	error "GUI support required CxProlog with wxWidgets GUI."
#endif

   /* ... */

/* CXPROLOG C'BUILTINS */

   /* ... */

void GuiWXWidgetsInit()
{
	/* ... */
}

#else /* !USE_WXWIDGETS */

void GuiWXWidgetsInit()
{
	/* nothing */
}

#endif /* USE_WXWIDGETS */
