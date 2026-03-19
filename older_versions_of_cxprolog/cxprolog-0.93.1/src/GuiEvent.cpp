/*
 *   This file is part of the CxProlog system

 *   GuiEvent.cpp
 *   by A.Miguel Dias - 2006/01/05
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
static wxMutex polyMutex ;
static wxCondition polyCondition(polyMutex) ;

static void EventNotifier()
{
	polyCondition.Signal() ;
}

Pt GetEvent()
{
	static Bool b = false ;
	wxMutexLocker lock(polyMutex) ;
	if( (b = !b) )
		for(;;) {
			if( WXHowManyEvents() > 0 ) return WXGetEvent() ;
			if( JavaHowManyEvents() > 0 ) return JavaGetEvent() ;
			polyCondition.Wait() ;
		}
	else
		for(;;) {
			if( JavaHowManyEvents() > 0 ) return JavaGetEvent() ;
			if( WXHowManyEvents() > 0 ) return WXGetEvent() ;
			polyCondition.Wait() ;
		}
}

#else /* USE_WXWIDGETS */
static Fun EventNotifier = nil ;

Pt GetEvent()
{
	return JavaGetEvent() ;
}

#endif /* USE_WXWIDGETS */

static void PPolyEventGet()
{
	MustBe( Unify(X0, GetEvent()) ) ;
}

static void PPolyEventIsAvailable()
{
	Bool eventsAvailable =
			WXHowManyEvents() > 0
		|| JavaHowManyEvents() > 0 ;
	MustBe( eventsAvailable ) ;
}

static void PPolyDiscardEvents()
{
	WXDiscardAllEvents() ;
	JavaDiscardAllEvents() ;
	JumpNext() ;
}

void GuiEventInit()
{
	InstallCBuiltinPred("gui_event_get", 1, PPolyEventGet) ;
	InstallCBuiltinPred("gui_event_is_available", 0, PPolyEventIsAvailable) ;
	InstallCBuiltinPred("gui_event_discard_all", 0, PPolyDiscardEvents) ;
	WXSetEventNotifier(EventNotifier) ;
	JavaSetEventNotifier(EventNotifier) ;
}
