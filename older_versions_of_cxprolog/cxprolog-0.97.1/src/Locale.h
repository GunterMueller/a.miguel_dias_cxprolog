/*
 *   This file is part of the CxProlog system

 *   Locale.h
 *   by A.Miguel Dias - 2004/12/31
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Locale_
#define _Locale_


/* LOCALES */

#define UseLocale(l)		if( (l) != currentLocale ) SetLocaleCType(l)
#define SystemLocale()		( systemLocale )

extern CharPt systemLocale, currentLocale ;

void SetLocaleCType(CharPt l) ;
CharPt SetLocaleCollate(CharPt l) ;

void LocaleInit(void) ;
void LocaleInit2(void) ;


/* ENCODINGS */

typedef enum
{
	octetEncoding,
	textEncoding,
	asciiEncoding,
	latin1Encoding,
	utf8Encoding,
	utf16beEncoding,
	utf16leEncoding,
	utf32beEncoding,
	utf32leEncoding,
/* Ambiguous */
	utf16Encoding,
	utf32Encoding,
	unicodeEncoding,	/* UCS-2 */
/* Compatibility */
	unicodebeEncoding,	/* UCS-2 BE */
	unicodeleEncoding,	/* UCS-2 LE */
	lastEncoding	/* Marks the end */
} CharEncoding ;

#define SystemEncoding()	( systemEncoding )
#define DefaultEncoding()	( defaultEncoding )

extern CharPt systemEncoding, defaultEncoding ;

#define EncodingGet(l)		((l)[0])
CharPt EncodingMake(CharPt enc) ;
CharPt EncodingName(CharPt l) ;
void EncodingsRestart(void) ;
void SetDefaultEncoding(CharPt enc) ;

WChar FileGetInternalize(FILE *file, CharPt encoding, CharPt fName) ;
void FilePutExternalize(FILE *file, WChar c, CharPt encoding, CharPt fName) ;

WChar StrGetCharInternalize(CharHdl s, CharPt encoding) ;
void StrPutCharExternalize(CharHdl s, WChar c, CharPt encoding) ;

CharPt StrInternalize(CharPt s) ;
CharPt StrInternalizeConv(CharPt s, WChar oldChar, WChar newChar) ;
CharPt StrInternalizeW(WCharPt s) ;
CharPt StrExternalize(CharPt s) ;

#endif
