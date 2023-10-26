/*
 *   This file is part of the CxProlog system

 *   TermChars.h
 *   by A.Miguel Dias - 1997/08/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _TermChars_
#define _TermChars_

#define CharType(c)		((_allChars+1)[c])

#define cx_isalnum(c)	(CharType(c) <= _DG)
#define cx_isalpha(c)	(CharType(c) <= _UC)
#define cx_isupper(c)	(CharType(c) == _UC)
#define cx_isdigit(c)	(CharType(c) == _DG)
#define cx_isspace(c)	(CharType(c) == _BL)
#define cx_issymbol(c)	(CharType(c) == _SY)
#define cx_isprint(c)	(CharType(c) <= _BL)
#define cx_iseof(c)     (CharType(c) == _EF)

typedef enum
{		/* DO NOT CHANGE THE ORDER OF THE FOLLOWING LINES */
	_ER,	/* error */
	_LC,	/* lower case */
	_UC,	/* upper case */
	_DG,	/* digit */
	_SO,	/* solo char */
	_SY,	/* symbol */
	_BL,	/* blank */
    _EF,    /* End Of File char */
	__I		/* invalid (except between single quotes, double quotes and commentaries) */
} CharTypes ;

/*	There are some categories containing a single char.
	These are represented by the char itself. */

extern CharTypes _allChars[257] ;

#endif
