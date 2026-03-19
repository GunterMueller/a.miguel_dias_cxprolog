/*
 *   This file is part of the NanoProlog system

 *   TermChars.h
 *   by A.Miguel Dias - 97/8/5
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1997 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 970805: Array of char categories introduced for the first time to cope
 			with Latin-1 characters.

*/

#ifndef _TermChars_
#define _TermChars_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif

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
