/*
 *   This file is part of the CxProlog system

 *   Locale.h
 *   by A.Miguel Dias - 2004/12/31
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Locale_
#define _Locale_

#define UseLocale(l)		if( (l) != currentLocale ) SetLocaleCType(l)
#define DefaultLocale()		( defaultLocale )

extern CharPt defaultLocale, currentLocale ;

CharPt MakeLocaleCType(CharPt l) ;
void SetLocaleCType(CharPt l) ;
CharPt SetLocaleCollate(CharPt l) ;

CharPt StrInternalize(CharPt s) ;
CharPt StrExternalize(CharPt s) ;
CharPt StrInternalizeW(WCharPt s) ;

void LocaleInit(void) ;
void LocaleInit2(void) ;

#endif
