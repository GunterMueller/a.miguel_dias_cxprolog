/*
 *   This file is part of the CxProlog system

 *   TermChars.h
 *   by A.Miguel Dias - 1997/08/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _TermChars_
#define _TermChars_

/* remove most "ctype.h" definitions  */
#define iscntrl		err @ err
#define isgraph		err @ err
#define islower		err @ err
#define isprint		err @ err
#define ispunct		err @ err
#define isupper		err @ err
#define isxdigit	err @ err

#define isalnum(c)	(allChars[c] <= _DG)
#define isalpha(c)	(allChars[c] <= _UC)
#define isdigit(c)	(allChars[c] == _DG)
#define isspace(c)	(allChars[c] == _BL)
#define issymbol(c)	(allChars[c] == _SY)
#define iseof(c)	(allChars[c] == _EF)

typedef enum
{		/* DO NOT CHANGE THE ORDER OF THE FOLLOWING LINES */
	_LC,	/* lower case */
	_UC,	/* upper case */
	_DG,	/* digit */
	_SO,	/* solo char */
	_SY,	/* symbol */
	_BL,	/* blank */
	_EF,	/* End Of File char */
	__I		/* invalid (except between single quotes, double quotes and commentaries) */
} CharCategories ;

/*	There are some categories containing a single char.
	These are represented by the char itself. */

extern CharCategories allChars[256] ;

#endif
