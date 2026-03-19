/*
 *   This file is part of the CxProlog system

 *   Locale.c
 *   by A.Miguel Dias - 2004/12/31
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* LOCALES */

#include <locale.h>

CharPt systemLocale, currentLocale ;

static CharPt EnableLocaleCType(CharPt l)
{	/* validates and allocates the locale string */
	CharPt loc ; /* cannot optimize because of fail_on_error */
	if( (loc = setlocale(LC_CTYPE, l)) == nil )
		return Error("Unknown locale %s", l) ;
	else {
		currentLocale = AtomName(LookupAtom(loc)) ;
		return currentLocale ;
	}
}

static CharPt EnableSystemLocaleCType()
{
	CharPt loc ; /* cannot optimize because of fail_on_error */
	if( (loc = setlocale(LC_CTYPE, "")) == nil ) {
		Warning("Locale not supported by C library. Using the fallback 'C' locale") ;
		return EnableLocaleCType("C") ;
	}
	else {
		currentLocale = AtomName(LookupAtom(loc)) ;
		return currentLocale ;
	}
}

void SetLocaleCType(CharPt l)
{	/* pre: l is a previously validated and allocated locale string */
	if( setlocale(LC_CTYPE, l) == nil )
		FatalError("Unknown locale %s", l) ;
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


/* ENCODINGS */

#if UNDERSTAND_EXTERNAL_ENCODINGS
#include <wchar.h>
static mbstate_t ps ;
#endif

CharPt systemEncoding, defaultEncoding ;
static Char encodings[lastEncoding] ;

static void InitEncodings(void)
{
	CharPt pt = encodings ;
	CharEncoding e ;
	for( e = octetEncoding ; e < lastEncoding ; e++ )
		*pt++ = cChar(e) ;
}

static CharPt EncodingMakeFromLocale(CharPt l)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	CharPt enc ;
	if( (enc = strchr(l, '.')) != nil ) {
		switch( enc[1] ) {
			case 'i': {
				if( StrEqual(enc, ".iso-8859-1") || StrEqual(enc, ".iso_8859-1")
						|| StrEqual(enc, ".iso-8859-15")
						|| StrEqual(enc, ".iso_8859-15") )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'I': {
				if( StrEqual(enc, ".ISO-8859-1") || StrEqual(enc, ".ISO_8859-1")
						|| StrEqual(enc, ".ISO-8859-15")
						|| StrEqual(enc, ".ISO_8859-15") )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'u': {
				if( StrEqual(enc, ".utf-8") || StrEqual(enc, ".utf8") )
					return encodings + utf8Encoding ;
				break ;
			}
			case'U': {
				if( StrEqual(enc, ".UTF-8") || StrEqual(enc, ".UTF8") )
					return encodings + utf8Encoding ;
				break ;
			}
		}
	}
	else {
		switch( *l ) {
			case 'b': {
				if( StrEqualN(l, "br_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'c': {
				if( StrEqualN(l, "ca_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'd': {
				if( StrEqualN(l, "de_", 3) || StrEqualN(l, "da_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'e': {
				if( StrEqualN(l, "en_", 3) || StrEqualN(l, "es_", 3)
						|| StrEqualN(l, "eu_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'f': {
				if( StrEqualN(l, "fr_", 3) || StrEqualN(l, "fi_", 3)
													|| StrEqualN(l, "fo_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'i': {
				if( StrEqualN(l, "it_", 3)  )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'n': {
				if( StrEqualN(l, "nl_", 3) || StrEqualN(l, "nn_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'p': {
				if( StrEqualN(l, "pt_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
			case 'u': {
				if( StrEqualN(l, "us_", 3) )
					return encodings + latin1Encoding ;
				break ;
			}
		}
	}

	if( (enc = strchr(l, '@')) != nil ) {
		if( StrEqual(enc, "@euro") )
			return encodings + latin1Encoding ;
	}
	return l ;
#else
	return encodings + latin1Encoding ;
#endif

}

CharPt EncodingMake(CharPt enc)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	switch( *enc ) {
		case 'a':
			if( StrEqual(enc, "ascii") )
				return encodings + asciiEncoding ;
			break ;
		case 'd':
			if( StrEqual(enc, "default") )
				return DefaultEncoding() ;
			break ;
		case 'i':
			if( StrEqual(enc, "iso_latin_1") )
				return encodings + latin1Encoding ;
			break ;
		case 'o':
			if( StrEqual(enc, "octet") )
				return encodings + octetEncoding ;
			break ;
		case 't':
			if( StrEqual(enc, "text") )
				return encodings + textEncoding ;
			break ;
		case 'u':
			if( StrEqual(enc, "utf8") )
				return encodings + utf8Encoding ;
			if( StrEqual(enc, "utf16") )
				return encodings + utf16Encoding ;
			if( StrEqual(enc, "utf16be") )
				return encodings + utf16beEncoding ;
			if( StrEqual(enc, "utf16le") )
				return encodings + utf16leEncoding ;
			if( StrEqual(enc, "utf32") )
				return encodings + utf32Encoding ;
			if( StrEqual(enc, "utf32be") )
				return encodings + utf32beEncoding ;
			if( StrEqual(enc, "utf32le") )
				return encodings + utf32leEncoding ;
			if( StrEqual(enc, "unicode") )
				return encodings + unicodeEncoding ;
			if( StrEqual(enc, "unicode_be") )
				return encodings + unicodebeEncoding ;
			if( StrEqual(enc, "unicode_le") )
				return encodings + unicodeleEncoding ;
			break ;
	}
	return EnableLocaleCType(enc) ;
#else
	return encodings + latin1Encoding ;
#endif
}

CharPt EncodingName(CharPt l)
{
	switch( l[0] ) {
		case textEncoding:
			return "text" ;
		case asciiEncoding:
			return "ascii" ;
		case utf8Encoding:
			return "utf8" ;
		case utf16Encoding:
			return "utf16" ;
		case utf16beEncoding:
			return "utf16be" ;
		case utf16leEncoding:
			return "utf16le" ;
		case utf32Encoding:
			return "utf32" ;
		case utf32beEncoding:
			return "utf32be" ;
		case utf32leEncoding:
			return "utf32le" ;
		case octetEncoding:
			return "octet" ;
		case latin1Encoding:
			return "iso_latin_1" ;
		case unicodeEncoding:
			return "unicode" ;
		case unicodebeEncoding:
			return "unicode_be" ;
		case unicodeleEncoding:
			return "unicode_le" ;
		default:
			return l ;
	}
}

void EncodingsRestart()
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	ClearBytes(&ps, sizeof(mbstate_t)) ;
#endif
}

void SetDefaultEncoding(CharPt enc)
{
	defaultEncoding = enc ;
}

WChar FileGetInternalize(FILE *file, CharPt encoding, CharPt fName)
{
	WChar c ;
#if UNDERSTAND_EXTERNAL_ENCODINGS
	int len = 0 ;	/* 0 used for EOF detection */
	Char ch ;
	UseLocale(encoding) ;
	do {
		if( (c = fgetc(file)) == EOF )
			break ;
		ch = c ;
	} while( (len = mbrtowc(&c, &ch, 1, &ps)) == -2 ) ;
	if( len < 0 )
		FileError("Wide character conversion error on input file '%s'",
														fName) ;
#else
	c = fgetc(file) ;
#endif
	return CharFixCode(c) ;
}

void FilePutExternalize(FILE *file, WChar c, CharPt encoding, CharPt fName)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	char buff[MB_LEN_MAX] ;
	int len ;
	UseLocale(encoding) ;
	if( (len = wcrtomb(buff, c, &ps) ) == -1 ) {
		if( OSIsATty(file) ) { /* Instead of giving an error... */
			strcpy(buff, CharAsNumericEscape(c)) ;
			len = strlen(buff) ;
		}
		else
			FileError("Could not convert to multibyte sequence on file '%s'", fName) ;
	}
	if( fwrite(buff, 1, len, file) != len )
		FileError("Could not write to file '%s'", fName) ;
#else
	if( fputc(c, file) == EOF )
		FileError("Could not write to file '%s'", fName) ;
#endif
}

WChar StrGetCharInternalize(CharHdl s, CharPt encoding)
{
	WChar c ;
#if UNDERSTAND_EXTERNAL_ENCODINGS
	int len ;
	UseLocale(encoding) ;
	if( (len = mbrtowc(&c, *s, 999, &ps)) <= 0 ) {
		if( len == 0 ) len = 1 ;
		else
			Error("Could not convert to wide character (StrGetCharInternalize)") ;
	}
	*s += len ;
#else
	c = *(*s)++ ;
#endif
	return CharFixCode(c) ;
}

void StrPutCharExternalize(CharHdl s, WChar c, CharPt encoding)
{  /* pre: s has MB_LEN_MAX free slots, at least */
#if UNDERSTAND_EXTERNAL_ENCODINGS
	int len ;
	UseLocale(encoding) ;
	if( (len = wcrtomb(*s, c, &ps)) == -1 )
		Error("Could not convert to multibyte sequence (StrPutCharExternalize)") ;
	*s += len ;
#else
	*(*s)++ = c ;
#endif
}

CharPt StrInternalize(CharPt s)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	WChar c ;
	int len ;
	if( s == nil ) return s ;
	UseLocale(SystemLocale()) ;
	GStrOpen() ;
	do {
		len = mbrtowc(&c, s, 999, &ps) ;
		if( len < 0 )
			return Error("Could not convert to wide character (StrInternalize)") ;
		s += len ;
		GStrAddChar(CharFixCode(c)) ;
	} while( c != 0 ) ;
	return GStrClose() ;
#else
	return s ;
#endif
}

CharPt StrInternalizeConv(CharPt s, WChar oldChar, WChar newChar)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	WChar c ;
	int len ;
	if( s == nil ) return s ;
	UseLocale(SystemLocale()) ;
	GStrOpen() ;
	do {
		len = mbrtowc(&c, s, 999, &ps) ;
		if( len < 0 )
			return Error("Could not convert to wide character (StrInternalizeConv)") ;
		s += len ;
		if( c == oldChar )
			c = newChar ;
		GStrAddChar(CharFixCode(c)) ;
	} while( c != 0 ) ;
	return GStrClose() ;
#else
	register CharPt pt ;
	for( pt = s ; *pt != '\0' ; pt++ )
		if( *pt == oldChar )
			*pt = newChar ;
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

CharPt StrExternalize(CharPt s)
{
#if UNDERSTAND_EXTERNAL_ENCODINGS
	WChar c ;
	int len ;
	if( s == nil ) return s ;
	UseLocale(SystemLocale()) ;
	GStrOpen() ;
	do {
		c = CharDecode(s) ;
		len = wcrtomb(GStrCurr(), c, &ps) ;
		if( len < 0 )
			Error("Could not convert to multibyte sequence (StrExternalize)") ;
		else GStrExtend(len) ;
	} while( c != 0 ) ;
	return GStrClose() ;
#else
	return s ;
#endif
}


/* CXPROLOG C'BUILTINS */

static void PLocale()
{
	ShowVersion() ;
	Write("LOCALE:\n    '%s'\n", SystemLocale()) ;
	JumpNext() ;
}

void LocaleInit()
{
	systemLocale = EnableSystemLocaleCType() ;
	InitEncodings() ;
	SetDefaultEncoding(EncodingMakeFromLocale(systemLocale)) ;
	systemEncoding = DefaultEncoding() ;
	EncodingsRestart() ;
}

void LocaleInit2()
{
	InstallCBuiltinPred("locale", 0, PLocale) ;
}
