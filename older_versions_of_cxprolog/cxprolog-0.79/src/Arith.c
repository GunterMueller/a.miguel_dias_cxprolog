/*
 *   This file is part of the CxProlog system

 *   Arith.c
 *   by A.Miguel Dias - 1989/12/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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

static Pt TermEvalt(register Pt t)
{
	PFloat r0, r1 ;
	VarValue(t) ;
	if( IsNumber(t) ) return t ;
	UseBuffer() ;
	BufferPush(t) ;
	do {
		if( IsStruct(BufferTop()) )
			BufferPush(Drf(XStructArg(BufferTop(),0))) ;
		elif( IsStruct(BufferXTop(1)) )
			BufferPush(Drf(XStructArg(BufferXTop(1),1))) ;
		else {
			t = BufferPop() ;
			r1 = IsInt(t) ? XInt(t) : XFloat(t) ;
			t = BufferPop() ;
			r0 = IsInt(t) ? XInt(t) : XFloat(t) ;
			switch( XStructName(BufferPop())[0] ) {
				case '+': t = MakeFloat(r0 + r1) ; break ;
				case '-': t = MakeFloat(r0 - r1) ; break ;
				case '*': t = MakeFloat(r0 * r1) ; break ;
				case '/': t = MakeFloat(r0 / r1) ; break ;
				default: Default("TermEvalt") ;
			}
			BufferPush(t) ;
		}
	} while( BufferUsed() > 1 ) ;
	t = BufferPop() ;
	FreeBuffer() ;
	return t ;
}

static void TermArithError(Pt t)
{
	if( IsAtom(t) ) 
		ArithError("Unknown ATOM in expression: '%s'", XAtomName(t)) ;
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

#if 1
static Bool Evaluate(register Pt t, PInt *i, PFloat *r)	/* Returns true if the result type is integer */
{
	Bool b0, b1 ;
	PInt i0, i1 ;
	PFloat r0, r1 ;
	CharPt name ;
	int arity ;
	Size length ;
	VarValue(t) ;
	if( IsInt(t) ) {
		*i = XInt(t) ;
		return true ;
	}
	if( IsFloat(t) ) {
		*r = XFloat(t) ;
		return false ;
	}
	if( IsAtom(t) ) {
		name = XAtomName(t) ;
		if( EqualStr(name, "cputime") ) {
			*r = CurrTime() ;
			return false ;
		}
		if( EqualStr(name, "heapused") ) {
			*i = WordsAsBytes(StaticMemoryUsed()) ;
			return true ;
		}
		if( EqualStr(name, "pi") ) {
			*r = 3.141592653589793233846 ;
			return false ;
		}
		if( EqualStr(name, "max_int") ) {
			*i = maxInt ;
			return true ;
		}
		if( EqualStr(name, "min_int") ) {
			*i = minInt ;
			return true ;
		}
		if( EqualStr(name, "int_size") ) {
			*i = intSize ;
			return true ;
		}
		if( EqualStr(name, "float_size") ) {
			*i = floatSize ;
			return true ;
		}
		if( EqualStr(name, "inf") ) {
			*r = infFloat ;
			return false ;
		}
	}
	if( IsStruct(t) ) {
		name = XStructName(t) ;
		length = strlen(name) ;
		arity = XStructArity(t) ;
		if( arity > 1 )
			b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;
		b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;

		if( arity <= 2 )
		switch( name[0] ) {
			case '+': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					*r = r0 + r1 ;
					return false ;
				}
				break ;
			}
			case '-': {
				if( arity == 1 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					*r = -r0 ;
					return false ;
				}
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					*r = r0 - r1 ;
					return false ;
				}
				break ;
			}
			case '*': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					*r = r0 * r1 ;
					return false ;
				}
				break ;
			}
			case '/': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
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
	if( IsList(t) && compatibleStrings_flag ) {
		if( Drf(XListTail(t)) != tNilAtom )
			Error("Expression list must have one single component") ;
		return Evaluate(XListHead(t), i, r) ;
	}
	TermArithError(t) ;
	return true ;
}
#else
static Bool Evaluate(register Pt t, PInt *i, PFloat *r)	/* Returns true if the result type is integer */
{
	Bool b0, b1 ;
	PInt i0, i1 ;
	PFloat r0, r1 ;
	CharPt name ;
	int arity ;
	Size length ;
	VarValue(t) ;
	if( IsInt(t) ) {
		*i = XInt(t) ;
		return true ;
	}
	if( IsFloat(t) ) {
		*r = XFloat(t) ;
		return false ;
	}
	if( IsAtom(t) ) {
		name = XAtomName(t) ;
		if( EqualStr(name, "cputime") ) {
			*r = CurrTime() ;
			return false ;
		}
		if( EqualStr(name, "heapused") ) {
			*i = WordsAsBytes(StaticMemoryUsed()) ;
			return true ;
		}
		if( EqualStr(name, "pi") ) {
			*r = 3.141592653589793233846 ;
			return false ;
		}
		if( EqualStr(name, "max_int") ) {
			*i = maxInt ;
			return true ;
		}
		if( EqualStr(name, "min_int") ) {
			*i = minInt ;
			return true ;
		}
		if( EqualStr(name, "int_size") ) {
			*i = intSize ;
			return true ;
		}
		if( EqualStr(name, "float_size") ) {
			*i = floatSize ;
			return true ;
		}
		if( EqualStr(name, "inf") ) {
			*r = infFloat ;
			return false ;
		}
	}
	if( IsStruct(t) ) {
		name = XStructName(t) ;
		length = strlen(name) ;
		arity = XStructArity(t) ;
		if( arity > 1 )
			b1 = Evaluate(XStructArg(t,1), &i1, &r1) ;
		b0 = Evaluate(XStructArg(t,0), &i0, &r0) ;

		if( arity <= 2 )
		switch( name[0] ) {
			case '+': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					*r = r0 + r1 ;
					return false ;
				}
				break ;
			}
			case '-': {
				if( arity == 1 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					*r = -r0 ;
					return false ;
				}
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					*r = r0 - r1 ;
					return false ;
				}
				break ;
			}
			case '*': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
					*r = r0 * r1 ;
					return false ;
				}
				break ;
			}
			case '/': {
				if( arity == 2 && length == 1 ) {
					if( b0 ) r0 = i0 ;
					if( b1 ) r1 = i1 ;
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
	if( IsList(t) && compatibleStrings_flag ) {
		if( Drf(XListTail(t)) != tNilAtom )
			Error("Expression list must have one single component") ;
		return Evaluate(XListHead(t), i, r) ;
	}
	TermArithError(t) ;
	return true ;
}
#endif

Pt TermEval(Pt t)
{
	PInt i ; PFloat r ;
	if( Evaluate(t, &i, &r) )
		return MakeInt(i) ;
	else return MakeFloat(r) ;
}



/* CXPROLOG C'BUILTINS */

static void PIst()
{
	if( UnifyWithNumber(X0, TermEvalt(X1)) ) JumpNext()
	DoFail()
}

static void PIs()
{
	if( UnifyWithNumber(X0, TermEval(X1)) ) JumpNext()
	DoFail()
}

static void PEq()
{
	PInt i0, i1 ;
	PFloat r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 == i1 : i0 == r1 )
			: ( b1 ? r0 == i1 : r0 == r1 ) ) JumpNext()
	DoFail()
}

static void PNe()
{
	PInt i0, i1 ;
	PFloat r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 != i1 : i0 != r1 )
			: ( b1 ? r0 != i1 : r0 != r1 ) ) JumpNext()
	DoFail()
}

static void PLt()
{
	PInt i0, i1 ;
	PFloat r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 < i1 : i0 < r1 )
			: ( b1 ? r0 < i1 : r0 < r1 ) ) JumpNext()
	DoFail()
}

static void PGt()
{
	PInt i0, i1 ;
	PFloat r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 > i1 : i0 > r1 )
			: ( b1 ? r0 > i1 : r0 > r1 ) ) JumpNext()
	DoFail()
}

static void PLe()
{
	PInt i0, i1 ;
	PFloat r0, r1 ;
	Bool b0 = Evaluate(X0, &i0, &r0) ;
	Bool b1 = Evaluate(X1, &i1, &r1) ;
	if( b0 ? ( b1 ? i0 <= i1 : i0 <= r1 )
			: ( b1 ? r0 <= i1 : r0 <= r1 ) ) JumpNext()
	DoFail()
}

static void PGe()
{
	PInt i0, i1 ;
	PFloat r0, r1 ;
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
		PInt i = XInt(t0) + 1 ;
		if( i < 1 || not UnifyWithNumber(X1, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	elif( IsVar(t0) ) {
		PInt i = XTestInt(X1) - 1 ;
		if( i < 0 || not UnifyWithNumber(t0, MakeInt(i)) ) DoFail()
		JumpNext()
	}
	XTestInt(X0) ;
}

void ArithInit()
{
	InstallCBuiltinPred("ist", 2, PIst) ;
	InstallCBuiltinPred("is", 2, PIs) ;
	InstallCBuiltinPred("=:=", 2, PEq) ;
	InstallCBuiltinPred("=\\=", 2, PNe) ;	/* Use of escape '\' */
	InstallCBuiltinPred("<", 2, PLt) ;
	InstallCBuiltinPred(">", 2, PGt) ;
	InstallCBuiltinPred("=<", 2, PLe) ;
	InstallCBuiltinPred(">=", 2, PGe) ;
	InstallCBuiltinPred("succ", 2, PSucc) ;
}