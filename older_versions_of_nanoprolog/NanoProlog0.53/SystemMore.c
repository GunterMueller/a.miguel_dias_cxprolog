/*
 *   This file is part of the NanoProlog system

 *   MoreSystem.c
 *   by A.Miguel Dias - 89/12/3
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

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

 931117: release of version 0.5

*/

/* INSTALL HERE YOUR OWN SYSTEM PREDICATES */

#include "NanoProlog.h"


/* */

#define X0		Xc(0)
#define X1		Xc(1)
#define X2		Xc(2)
#define X3		Xc(3)
#define X4		Xc(4)

#define R0		Drf(X0)
#define R1		Drf(X1)
#define R2		Drf(X2)
#define R3		Drf(X3)
#define R4		Drf(X4)

static Bool b, b0, b1 ;
static Int i, i0, i1 ;
static int n, n0, n1 ;
static Real r, r0, r1 ;
static Pt t0, t1, t2, t3 ;
static Hdl hh, h0, h1 ;
static char *str ;
static Clause *cl ;
static Predicate *pr ;
static Atom *at ;

static void TypeError(char *s)
{
	Error("Type Error: %s expected", s) ;
}

static void TypeCheck(char *types)
{
	Hdl a ;

	for( a = X ; *types != '\0' ; a++ )
	{
		switch(	*types++ )
		{
			case 'a':
					if( not IsAtom(Drf(*a)) ) TypeError("atom") ;
					break ;
			case 'i':
					if( not IsInt(Drf(*a)) ) TypeError("integer") ;
					break ;
			case 'p':
					if( not IsPos(Drf(*a)) ) TypeError("positive integer") ;
					break ;
			case 'n':
					if( not IsNumber(Drf(*a)) ) TypeError("number") ;
					break ;
			case 't':
					if( not IsAtomic(Drf(*a)) ) TypeError("atomic term") ;
					break ;
 			case 'v':
					if( not IsVar(Drf(*a)) ) TypeError("variable") ;
					break ;
			case 's':
					if( not IsStruct(Drf(*a)) ) TypeError("structure") ;
					break ;
			case 'l':
					if( not IsList(Drf(*a)) || Drf(*a) != TagAtom(nilAtom) ) TypeError("list") ;
					break ;
			case '?':
					break ;
			default: Default("TypeCheck") ;
		}
	}
}


/* PREDICATES */

static Pt StackAddr()
{
	int x ;
	
	return( cPt(&x) ) ;
}

static void PSys() /* Just a test */
{
	printf("STACK = %lx\n", StackAddr()) ;
	printf("&trailBegin = %lx\n", &trailBegin) ;
	printf("&h = %lx\n", &hh) ;
	printf("&CP = %lx\n", &CP) ;
	JumpNext()
}


/* */

void InstallMoreCPredicates()
{
	InstallCPredicate("sys", 0, PSys) ;
}
