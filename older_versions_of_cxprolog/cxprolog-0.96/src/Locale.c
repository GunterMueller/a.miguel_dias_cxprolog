/*
 *   This file is part of the CxProlog system

 *   Locale.c
 *   by A.Miguel Dias - 2004/12/31
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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
#if UNDERSTAND_EXTERNAL_ENCODINGS
#include <wchar.h>
#endif 

CharPt defaultLocale, currentLocale, binaryLocale ;

static CharPt MakeDefaultLocaleCType()
{
	CharPt loc ; /* cannot optimize because of fail_on_error */
	if( (loc = setlocale(LC_CTYPE, "")) == nil ) {
		Warning("Locale not supported by C library. Using the fallback 'C' locale") ;
		return MakeLocaleCType("C") ;
	}
	else {
		currentLocale = AtomName(LookupAtom(loc)) ;	
		return currentLocale ;
	}
}

CharPt MakeLocaleCType(CharPt l)
{	/* validates and allocates the locale string */
	CharPt loc ; /* cannot optimize because of fail_on_error */
	if( (loc = setlocale(LC_CTYPE, l)) == nil )
		return Error("Cannot set locale %s", l) ;
	else {
		currentLocale = AtomName(LookupAtom(loc)) ;		
		return currentLocale ;
	}
}

void SetLocaleCType(CharPt l)
{	/* pre: l is a previously validated and allocated locale string */
	if( setlocale(LC_CTYPE, l) == nil )
		FatalError("Cannot set locale %s", l) ;
	currentLocale = l ;
}

CharPt SetLocaleCollate(CharPt l)
{
	Str256 s ;
	strcpy(s, l) ;
#if UNDERSTAND_EXTERNAL_ENCODINGS
	strcat(s, ".utf8") ;
#else
	strcat(s, ".iso88591") ;
#endif
	if( setlocale(LC_COLLATE, s) == nil )
		return nil ;
	return AtomName(LookupAtom(l)) ;		
}

CharPt StrInternalize(CharPt s)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	mbstate_t ps ;
	wchar_t c ;
	int len ;
	if( s == nil ) return s ;
	UseLocale(DefaultLocale()) ;
	ClearBytes(&ps, sizeof(ps)) ;
	GStrOpen() ;
	do {
		len = mbrtowc(&c, s, 999, &ps) ;
		if( len < 0 )
			return Error("Cannot convert to wide character") ;
		s += len ;
		GStrAddChar(c) ;
	} while( c != 0 ) ;
	return GStrClose() ;
#else
	return s ;
#endif
}

CharPt StrExternalize(CharPt s)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	mbstate_t ps ;
	wchar_t c ;
	int len ;
	if( s == nil ) return s ;
	UseLocale(DefaultLocale()) ;
	ClearBytes(&ps, sizeof(ps)) ;
	GStrOpen() ;
	do {
		c = CharDecode(s) ;
		len = wcrtomb(GStrCurr(), c, &ps) ;
		if( len < 0 )
			Error("Cannot convert to multibyte sequence") ;
		else GStrExtend(len) ;
	} while( c != 0 ) ;
	return GStrClose() ;
#else
	return s ;
#endif
}

CharPt StrInternalizeW(WCharPt s)
{
	if( s == nil ) InternalError("StrInternalizeW") ;
	GStrOpen() ;
	while( *s != 0 )
		GStrAddChar(*s++) ;
	return GStrClose() ;
}


/* CXPROLOG C'BUILTINS */

static void PLocale()
{
	ShowVersion() ;
	Write("DEFAULT LOCALE:\n    '%s'\n", defaultLocale) ;
	JumpNext() ;
}

void LocaleInit()
{
	defaultLocale = MakeDefaultLocaleCType() ;
	binaryLocale = XAtomName(tBinaryAtom) ;
}

void LocaleInit2()
{
	InstallCBuiltinPred("locale", 0, PLocale) ;
}
