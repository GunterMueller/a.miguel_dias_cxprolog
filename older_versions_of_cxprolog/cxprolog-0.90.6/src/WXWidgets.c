/*
 *   This file is part of the CxProlog system

 *   WXWidgets.c
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

#if USE_WXWIDGETS

static QueuePt wxQueue ;

/* WXOBJ */

static ExtraTypePt wxobjType ;

typedef struct WXObj {
	ExtraDef(WXObj) ;
	VoidPt value  ;
} WXObj, *WXObjPt ;

#define cWXObjPt(x)		((WXObjPt)(x))
#define WXObjValue(w)	(cWXObjPt(w)->value)

static Pt tNullAtom, tNullWXObj ;
static WXObjPt nullWXObj ;

static void CreateNullStuff(void)
{
	tNullAtom = MakeAtom("null") ;
	nullWXObj = ExtraNew(wxobjType) ;
	WXObjValue(nullWXObj) = nil ;
	tNullWXObj = TagExtra(nullWXObj) ;
}

static WXObjPt WXObjNew(VoidPt obj)
{
	WXObjPt wx  ;
	if( obj == nil ) return nullWXObj ;
	wx = ExtraNew(wxobjType) ;
	WXObjValue(wx) = obj ;
	return wx ;
}

Pt MakeWXObj(VoidPt obj)
{
	return obj == nil ? tNullAtom : TagExtra(WXObjNew(obj)) ;
}

VoidPt XTestWXObjValue(register Pt t)
{
	VarValue(t) ;
	if( t == tNullAtom ) return nil ;
	if( IsThisExtra(wxobjType, t) )
		return WXObjValue(XExtra(t)) ;
	return TypeError2("WXOBJ", t) ;
}

static CharPt WXObjNamingFun(VoidPt x)
{
	return x == nullWXObj ? "null object" : nil ;
}

void WXWidgetsInit()
{
	wxobjType = ExtraTypeNew("WXOBJ", WordsOf(WXObj), WXObjNamingFun) ;
	CreateNullStuff() ;
	wxQueue = QueueNew() ;
}

#else

void WXWidgetsInit()
{
	/* nothing */
}

#endif /* USE_WXWIDGETS */
