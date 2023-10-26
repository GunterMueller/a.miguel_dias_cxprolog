/*
 *   This file is part of the CxProlog system

 *   Locale.c
 *   by A.Miguel Dias - 2004/12/31
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

#include "CxProlog.h"
#include <locale.h>
#if USE_WIDECHARS
#include <wchar.h>
#endif 

CharPt defaultLocale, currentLocale ;

CharPt MakeLocale(CharPt l)
{	/* validates and allocates the locale string */
	CharPt loc ; /* cannot optimize because of fail_on_error */
	if( (loc = setlocale(LC_CTYPE, l)) == nil )
		Error("Invalid locale '%s'", l) ;
	else
		currentLocale = AtomName(LookupAtom(loc)) ;
	return currentLocale ;
}

void SetLocale(CharPt l)
{	/* pre: l is a previously validated and allocated locale string */
	if( setlocale(LC_CTYPE, l) == nil )
		FatalError("Cannot set locale %s", l) ;
	currentLocale = l ;
}

CharPt StrInternalize(CharPt s)
{
#if USE_WIDECHARS
	mbstate_t ps ;
	wchar_t c ;
	int len ;
	if( s == nil ) return s ;
	UseLocale(DefaultLocale()) ;
	GStrOpen() ;
	ClearBytes(&ps, sizeof(ps)) ;
	while( *s ) {
		len = mbrtowc(&c, s, 999, &ps) ;
		if( len <= 0 )
			return Error("Cannot convert to wide character") ;
		s += len ;
		GStrAddChar(c) ;
	}
	return GStrClose() ;
#else
	return s ;
#endif
}

CharPt StrExternalize(CharPt s)
{
#if USE_WIDECHARS
	mbstate_t ps ;
	int len ;
	if( s == nil ) return s ;
	UseLocale(DefaultLocale()) ;
	GStrOpen() ;
	ClearBytes(&ps, sizeof(ps)) ;
	while( *s ) {
		GStrCheck() ;
		len = wcrtomb(GStrCurr(), CharDecode(s), &ps) ;
		if( len == -1 )
			Error("Cannot convert to multibyte sequence") ;
		else GStrExtend(len) ;
	}
	return GStrClose() ;
#else
	return s ;
#endif
}


/* CXPROLOG C'BUILTINS */

static void PLocale()
{
	ShowVersion() ;
	Write("Default locale is '%s'\n", defaultLocale) ;
	JumpNext()
}

void LocaleInit()
{
	defaultLocale = MakeLocale("") ;
}

void LocaleInit2()
{
	InstallCBuiltinPred("locale", 0, PLocale) ;
}
