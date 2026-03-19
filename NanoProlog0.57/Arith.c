/*
 *   This file is part of the NanoProlog system

 *   Arith.c
 *   by A.Miguel Dias - 89/12/5
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

 931203: symbol mac removed and symbol clockDef introduced.
 931117: release of version 0.5

*/

#include "NanoProlog.h"
#include <math.h>

#if clockDef == 1
	#include <time.h>
	#define ClockRate	(double)CLOCKS_PER_SEC
	static clock_t startClock ;
#endif
#if clockDef == 2
	#include <sys/types.h>
	#include <sys/times.h>
	#include <sys/stat.h>
	#define ClockRate	60.0
#endif

void InitTime()
{
#if clockDef == 1
	startClock = clock() ;
#endif
}
	
double NGetTime()
{
#if clockDef == 1
	return( ( clock() - startClock ) / (double)CLOCKS_PER_SEC ) ;
#endif
#if clockDef == 2
	struct tms theTime ;

	times(&theTime) ;
	return ((double)theTime.tms_utime)/ClockRate ;
#endif
}

Bool Narrow(double f, Int *i)
{
	if( f < minInt || f > maxInt || cInt(f) != f ) return( false ) ;
	*i = cInt(f) ;
	return( true ) ;
}

static void ArithError(Pt t)
{
	if( IsAtom(t) ) 
		Error("Unknown ATOM in expression: %d", XAtomName(t)) ;
	
	if( IsExtra(t) ) 
		Error("Unknown EXTRA in expression: %d", XExtraName(t)) ;
	
	if( IsStruct(t) )
		Error("Unknown FUNCTOR in expression: %s", XStructNameArity(t)) ;

	if( IsVar(t) )
		Error("Unbounded VARIABLE in expression") ;

	if( IsList(t) )
		Error("LIST in in expression") ;
	
	FatalError("In expression") ;
}

Bool Evaluate(Pt t, Int *i, Real *r)	/* Returns true if the result type is integer */
{
	Bool b0, b1 ;
	Int i0, i1 ;
	Real r0, r1 ;
	CharPt name ;
	int arity, length ;
	
	VarValue(t) ;

	if( IsInt(t) )
	{
		*i = XInt(t) ;
		return( true ) ;
	}
	
	if( IsReal(t) )
	{
		*r = XReal(t) ;
		return( false ) ;
	}
	
	if( IsAtom(t) )
	{
		name = XAtomName(t) ;

		if( EqualStr(name, "cputime") )
		{
			*r = NGetTime() ;
			return( false ) ;
		}
	
		if( EqualStr(name, "heapused") )
		{
			*i = HeapUsed() Wd ;
			return( true ) ;
		}
	
		ArithError(t) ;
	}
	
	if( IsStruct(t) )
	{
		name = XStructName(t) ;
		length = strlen(name) ;
		arity = XStructArity(t) ;
		switch( name[0] ) 
		{
			case '+':
			{
				if( arity != 2 || length != 1 ) ArithError(t) ;
				b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;
				b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;		
				if( b0 && b1 ) *i = i0 + i1 ;
				else *r = b0 ? i0 + r1 : ( b1 ? r0 + i1 : r0 + r1 ) ;
				return( b0 && b1 ) ;
			}
			
			case '-':
			{
				if( arity != 2 || length != 1 ) ArithError(t) ;
				b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;
				b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;				
				if( b0 && b1 ) *i = i0 - i1 ;
				else *r = b0 ? i0 - r1 : ( b1 ? r0 - i1 : r0 - r1 ) ;
				return( b0 && b1 ) ;
			}
	
			case '*':
			{
				if( arity != 2 || length != 1 ) ArithError(t) ;
				b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;
				b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;				
				if( b0 && b1 ) *i = i0 * i1 ;
				else *r = b0 ? i0 * r1 : ( b1 ? r0 * i1 : r0 * r1 ) ;
				return( b0 && b1 ) ;
			}
	
			case '/':
			{
				if( arity != 2 || length != 1 ) ArithError(t) ;
				b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;
				b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;				
				if( b0 ) r0 = i0 ;
				if( b1 ) r1 = i1 ;
				if( r1 == 0 ) Error("Division by zero") ;
				*r = r0 / r1 ;
				return( false ) ;
			}
		
			case 'm':
			{
				if( arity != 2 || not EqualStr(name, "mod") ) ArithError(t) ;
				b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;
				b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;				
				if( not ( b0 && b1 ) )
					Error("Both arguments of 'mod/2' must be integers") ;
				*i = i0 % i1 ;
				return( true ) ;
			}
			
			default: ArithError(t) ;
		}
	}
	
	if( IsList(t) )
		return( Evaluate(XListHead(t), i, r) ) ;

	ArithError(t) ;
}

Pt TermEval(Pt t)
{
	Int i ;
	Real r ;

	if( Evaluate(t, &i, &r) || Narrow(r, &i) )
		return( MakeInt(i) ) ;
	else return( MakeReal(r) ) ;
}
