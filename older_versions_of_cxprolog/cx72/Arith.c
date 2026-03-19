/*
 *   This file is part of the CxProlog system

 *   Arith.c
 *   by A.Miguel Dias - 1989/12/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"
#include <math.h>

Bool Narrow(double f, Int *i)
{
	if( f < minInt || f > maxInt || cInt(f) != f )
		return false ;
	*i = cInt(f) ;
	return true ;
}

static void TermArithError(Pt t)
{
	if( IsAtomOrText(t) ) 
		ArithError("Unknown ATOM in expression: '%s'", XAtomOrTextName(t)) ;
	if( IsExtra(t) ) 
		ArithError("Unknown EXTRA in expression: '%s'", XExtraAsStr(t)) ;
	if( IsStruct(t) )
		ArithError("Unknown FUNCTOR in expression: '%s'", XStructNameArity(t)) ;
	if( IsVar(t) )
		ArithError("Unbound VARIABLE in expression") ;
	if( IsList(t) )
		ArithError("LIST in expression") ;
	Default("TermArithError") ;
}

Bool Evaluate(register Pt t, Int *i, Real *r)	/* Returns true if the result type is integer */
{
	Bool b0, b1 ;
	Int i0, i1 ;
	Real r0, r1 ;
	CharPt name ;
	int arity, length ;
	
	VarValue(t) ;

	if( IsInt(t) ) {
		*i = XInt(t) ;
		return true ;
	}
	if( IsReal(t) ) {
		*r = XReal(t) ;
		return false ;
	}
	if( IsAtomOrText(t) ) {
		name = XAtomOrTextName(t) ;
		if( EqualStr(name, "cputime") ) {
			*r = CurrTime() ;
			return false ;
		}
		if( EqualStr(name, "heapused") ) {
			*i = MemoryUsed() Wd ;
			return true ;
		}
		if( EqualStr(name, "pi") ) {
			*r = 3.141592653589793233846 ;
			return false ;
		}
		if( EqualStr(name, "maxint") ) {
			*i = maxInt ;
			return true ;
		}
		if( EqualStr(name, "minint") ) {
			*i = minInt ;
			return true ;
		}
		TermArithError(t) ;
	}
	if( IsStruct(t) ) {
		name = XStructName(t) ;
		length = strlen(name) ;
		arity = XStructArity(t) ;
		if( arity > 1 )
			b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;
		b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;

		if( arity <= 2 )
		switch( *name ) {
			case '+': {
				if( arity == 2 && length == 1 ) {
					if( b0 && b1 ) *i = i0 + i1 ;
					else *r = b0 ? i0 + r1 : ( b1 ? r0 + i1 : r0 + r1 ) ;
					return b0 && b1 ;
				}
				break ;
			}
			case '-': {
				if( arity == 1 && length == 1 ) {
					if( b0 ) *i = -i0 ;
					else *r = -r0 ;
					return b0 ;
				}
				if( arity == 2 && length == 1 ) {
					if( b0 && b1 ) *i = i0 - i1 ;
					else *r = b0 ? i0 - r1 : ( b1 ? r0 - i1 : r0 - r1 ) ;
					return b0 && b1 ;
				}
				break ;
			}
			case '*': {
				if( arity == 2 && length == 1 ) {
					if( b0 && b1 ) *i = i0 * i1 ;
					else *r = b0 ? i0 * r1 : ( b1 ? r0 * i1 : r0 * r1 ) ;
					return b0 && b1 ;
				}
				break ;
			}
			case '/': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					if( r1 == 0 ) Error("Division by zero") ;
					*r = r0 / r1 ;
					return false ;
				}
				if( arity == 2 && EqualStr(name, "//") ) {
					if( not ( b0 && b1 ) )
						Error("Both arguments of '//'/2 must be integers") ;
					*i = i0 / i1 ;
					return true ;
				}
				if( arity == 2 && EqualStr(name, "/\\") ) {
					if( not ( b0 && b1 ) )
						Error("Both arguments of '/\\'/2 must be integers") ;
					*i = i0 & i1 ;
					return true ;
				}
				break ;
			}
			case '\\': {
				if( arity == 1 && length == 1 ) {
					if( not b0 )
						Error("The argument of '\\/2' must be a integer") ;
					*i = ~i0 ;
					return true ;
				}
				if( arity == 2 && EqualStr(name, "\\/") ) {
					if( not ( b0 && b1 ) )
						Error("Both arguments of '\\/'/2 must be integers") ;
					*i = i0 | i1 ;
					return true ;
				}
				break ;
			}
			case '^': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					*r = pow(r0, r1) ;
					return false ;
				}
				break ;
			}
			case '>': {
				if( arity == 2 && EqualStr(name, ">>") ) {
					if( not ( b0 && b1 ) )
						Error("Both arguments of '>>/2' must be integers") ;
					*i = i0 >> i1 ;
					return true ;
				}
				break ;
			}
			case '<': {
				if( arity == 2 && EqualStr(name, "<<") ) {
					if( not ( b0 && b1 ) )
						Error("Both arguments of '<</2' must be integers") ;
					*i = i0 << i1 ;
					return true ;
				}
				break ;
			}
			case 'a': {
				if( arity == 1 ) {
					if( b0 ) r0 = i0 ;
					if( EqualStr(name, "asin") ) *r = asin(r0) ;
					elif( EqualStr(name, "acos") ) *r = acos(r0) ;
					elif( EqualStr(name, "atan") ) *r = atan(r0) ;
					else break ;
					return false ;
				}
				break ;
			}
			case 'c': {
				if( arity == 1 ) {
					if( b0 ) r0 = i0 ;
					if( EqualStr(name, "cos") ) {
						*r = cos(r0) ;
						return false ;
					}
					elif( EqualStr(name, "ceil") ) {
						*i = ceil(r0) ;
						return true ;
					}
					else break ;
				}
				break ;
			}
			case 'e': {
				if( arity == 1 ) {
					if( b0 ) r0 = i0 ;
					if( EqualStr(name, "exp") ) {
						*r = exp(r0) ;
						return false ;
					}
					else break ;
				}
				break ;
			}
			case 'f': {
				if( arity == 1 ) {
					if( b0 ) r0 = i0 ;
					if( EqualStr(name, "floor") ) {
						*i = floor(r0) ;
						return true ;
					}
					else break ;
				}
				break ;
			}
			case 'l': {
				if( arity == 1 ) {
					if( b0 ) r0 = i0 ;
					if( EqualStr(name, "log") ) *r = log(r0) ;
					elif( EqualStr(name, "log10") ) *r = log10(r0) ;
					else break ;
					return false ;
				}
				break ;
			}
			case 'm': {
				if( arity == 2 && EqualStr(name, "mod") ) {
					if( not ( b0 && b1 ) )
						Error("Both arguments of 'mod/2' must be integers") ;
					*i = i0 % i1 ;
					return true ;
				}
				break ;
			}
			case 's': {
				if( arity == 1 ) {
					if( b0 ) r0 = i0 ;
					if( EqualStr(name, "sqrt") ) *r = sqrt(r0) ;
					elif( EqualStr(name, "sin") ) *r = sin(r0) ;
					else break ;
					return false ;
				}
				break ;
			}

			case 't': {
				if( arity == 1 ) {
					if( b0 ) r0 = i0 ;
					if( EqualStr(name, "tan") ) *r = tan(r0) ;
					else break ;
					return false ;
				}
				break ;
			}
			default: break ;
		}
	}
	if( IsList(t) ) {
		if( Drf(XListTail(t)) != tNilAtom )
			Error("Expression list should have one single component") ;
		return Evaluate(XListHead(t), i, r) ;
	}
	TermArithError(t) ;
}

Pt TermEval(Pt t)
{
	Int i ; Real r ;

	if( Evaluate(t, &i, &r) || Narrow(r, &i) )
		return MakeInt(i) ;
	else return MakeReal(r) ;
}


/* CXPROLOG C'BUILTINS */

static void PIs()
{
	if( UnifyWithNumber(X0, TermEval(X1)) ) JumpNext()
	DoFail()
}

static void PEq()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 == i1 : i0 == r1 )
			: ( b1 ? r0 == i1 : r0 == r1 ) ) JumpNext()
	DoFail()
}

static void PNe()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 != i1 : i0 != r1 )
			: ( b1 ? r0 != i1 : r0 != r1 ) ) JumpNext()
	DoFail()
}

static void PLt()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 < i1 : i0 < r1 )
			: ( b1 ? r0 < i1 : r0 < r1 ) ) JumpNext()
	DoFail()
}

static void PGt()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 > i1 : i0 > r1 )
			: ( b1 ? r0 > i1 : r0 > r1 ) ) JumpNext()
	DoFail()
}

static void PLe()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 <= i1 : i0 <= r1 )
			: ( b1 ? r0 <= i1 : r0 <= r1 ) ) JumpNext()
	DoFail()
}

static void PGe()
{
	Int i0, i1 ;
	Real r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 >= i1 : i0 >= r1 )
			: ( b1 ? r0 >= i1 : r0 >= r1 ) ) JumpNext()
	DoFail()
}

static void PSucc()
{
	Pt t0 = Drf(X0) ;
	if( IsInt(t0) ) {
		Int i = XInt(t0) + 1 ;
		if( i < 1 || not UnifyWithNumber(X1, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	elif( IsVar(t0) ) {
		Int i = XTestInt(X1) - 1 ;
		if( i < 0 || not UnifyWithNumber(t0, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	XTestInt(X0) ;
}

void InitArith()
{
	InstallCBuiltinPred("is", 2, PIs) ;
	InstallCBuiltinPred("=:=", 2, PEq) ;
	InstallCBuiltinPred("=\\=", 2, PNe) ;	/* Use of escape '\' */
	InstallCBuiltinPred("<", 2, PLt) ;
	InstallCBuiltinPred(">", 2, PGt) ;
	InstallCBuiltinPred("=<", 2, PLe) ;
	InstallCBuiltinPred(">=", 2, PGe) ;
	InstallCBuiltinPred("succ", 2, PSucc) ;
}
