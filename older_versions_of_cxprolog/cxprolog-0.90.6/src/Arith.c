/*
 *   This file is part of the CxProlog system

 *   Arith.c
 *   by A.Miguel Dias - 1989/12/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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
#include <math.h>

#define IVarSupport 0

/* PRIVATE FUNCTIONS */

static VoidPt TermArithError(Pt t)
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
	return InternalError("TermArithError") ;
}

#define chk(s)	StrEqual(name, s)

static Pt EvalArith0(Pt t)
{
	CharPt name = XAtomName(t) ;
	if( StrEqual(name, "cputime") )
		return MakeFloat(CpuTime()) ;
	switch( name[0] ) {
		case 'c': if( chk("cputime") ) return MakeFloat(CpuTime()) ;
		case 'f': if( chk("float_size") ) return MakeInt(floatSize) ;	
		case 'h': if( chk("heapused") )
					return MakeInt(WordsAsBytes(CodeAreaUsed())) ;
		case 'i': if( chk("int_size") ) return MakeInt(intSize) ;	
				  if( chk("inf") ) return MakeFloat(INFINITY) ;
		case 'm': if( chk("max_int") ) return MakeInt(maxInt) ;
				  if( chk("min_int") ) return MakeInt(minInt) ;
		case 'n': if( chk("nan") ) return MakeFloat(NAN) ;
		case 'p': if( chk("pi") )
					return MakeFloat(3.1415926535897932384626433832795029L) ;	
	}
#if IVarSupport
	if( IsAtom(t) && AtomToIVar(XAtom(t)) != nil ) {
		Pt val = IVarGet(XAtom(t)) ;
		if( IsNumber(val) ) return val ;
	}
#endif
	return TermArithError(t) ;
}

static Pt EvalArith1(FunctorPt f, Pt arg)
{
	CharPt name = FunctorName(f) ;
	switch( name[0] ) {
		case '-': if( chk("-") ) {
					if( IsInt(arg) ) return MakeInt(-XInt(arg)) ;
					else return MakeFloat(-XAsFloat(arg)) ;
					}
		case '\\': if( chk("\\") ) {
					if( IsInt(arg) ) return MakeInt(~XInt(arg)) ;
					else ArithError("Arg of '\\/1' must be an integer") ;
					}
		case 'a': if( chk("asin") ) return MakeFloat(asin(XAsFloat(arg))) ;
				  if( chk("acos") ) return MakeFloat(acos(XAsFloat(arg))) ;
				  if( chk("atan") ) return MakeFloat(atan(XAsFloat(arg))) ;
		case 'c': if( chk("cos") ) return MakeFloat(cos(XAsFloat(arg))) ;
#if __USE_ISOC99
				  if( chk("ceil") ) return MakeFloat(ceill(XAsFloat(arg))) ;
#else
				  if( chk("ceil") ) return MakeFloat(ceil(XAsFloat(arg))) ;
#endif
		case 'e': if( chk("exp") ) return MakeFloat(exp(XAsFloat(arg))) ;
#if __USE_ISOC99
		case 'f': if( chk("floor") ) return MakeFloat(floorl(XAsFloat(arg))) ;
#else
		case 'f': if( chk("floor") ) return MakeFloat(floor(XAsFloat(arg))) ;
#endif
		case 'l': if( chk("log") ) return MakeFloat(log(XAsFloat(arg))) ;
				  if( chk("log10") ) return MakeFloat(log10(XAsFloat(arg))) ;
#if __USE_ISOC99
		case 'r': if( chk("round") ) return MakeFloat(roundl(XAsFloat(arg))) ;
#else
		case 'r': if( chk("round") ) {
					double pint, pfrac = modf(XAsFloat(arg), &pint) ;
					return MakeFloat(pfrac <= 0.5 ? pint + 1 : pint) ;
				}
#endif
		case 's': if( chk("sqrt") ) return MakeFloat(sqrt(XAsFloat(arg))) ;
				  if( chk("sin") ) return MakeFloat(sin(XAsFloat(arg))) ;
		case 't': if( chk("tan") ) return MakeFloat(tan(XAsFloat(arg))) ;
	}
	return TermArithError(MakeUnStruct(f, arg)) ;
}

static Pt EvalArith2(FunctorPt f, Pt arg1, Pt arg2)
{
	CharPt name = FunctorName(f) ;
	switch( name[0] ) {
		case '+': if( chk("+") )
					return MakeFloat(XAsFloat(arg1) + XAsFloat(arg2)) ;
		case '-': if( chk("-") )
					return MakeFloat(XAsFloat(arg1) - XAsFloat(arg2)) ;
		case '*': if( chk("*") )
					return MakeFloat(XAsFloat(arg1) * XAsFloat(arg2)) ;
		case '/': if( chk("/") )
					return MakeFloat(XAsFloat(arg1) / XAsFloat(arg2)) ;
				  if( chk("//") ) {
					if( IsInt(arg1) && IsInt(arg2) ) {
						int den = XInt(arg2) ;
						if( den == 0 ) ArithError("Zero divisor") ;
						return MakeInt(XInt(arg1) / den) ;
					}
					else ArithError("Both args of '///2' must be integers") ;
				  }
				  if( chk("/\\") ) {
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) & XInt(arg2)) ;
					else ArithError("Both args of '/\\/2' must be integers") ;
				  }
		case '\\': if( chk("\\/") ) {
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) | XInt(arg2)) ;
					else ArithError("Both args of '\\//2' must be integers") ;
				   }
		case '^': if( chk("^") )
					return MakeFloat(pow(XAsFloat(arg1), XAsFloat(arg2))) ;
		case '>': if( chk(">>") ) {
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) >> XInt(arg2)) ;
					else ArithError("Both args of '>>/2' must be integers") ;
				  }
		case '<': if( chk("<<") ) {
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) << XInt(arg2)) ;
					else ArithError("Both args of '<</2' must be integers") ;
				  }
		case 'm': if( chk("mod") ) {
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) % XInt(arg2)) ;
					else ArithError("Both args of 'mod/2' must be integers") ;
				  }
	}
	return TermArithError(MakeBinStruct(f, arg1, arg2)) ;
}

static Pt EvalPre(register Pt t)
{
redo:
	VarValue(t) ;
	if( IsNumber(t) ) return t ;
	if( IsStruct(t) ) return IsUnitParam(t) ? Z(OutParam(XUnitParam(t))) : t ;
	if( IsAtom(t) ) return EvalArith0(t) ;	
	if( IsList(t) && compatibleStrings_flag ) {
		if( Drf(XListTail(t)) != tNilAtom )
			ArithError("Expression list must have a single component") ;
		t = XListHead(t) ;
		goto redo ;
	}
	return TermArithError(t) ;
}


/* MAIN OPERATIONS */

Pt Evaluate(register Pt t)
{
	UseScratch() ;
	ScratchPush(EvalPre(t)) ;
	for(;;) {
		if( IsStruct(ScratchTop()) ) {
			Pt e = EvalPre(XStructArg(ScratchTop(),0)) ;
			ScratchPush(e) ;
		}
			
		elif( ScratchUsed() == 1 ) break ;

		elif( IsStruct(ScratchXTop(1)) )
		  switch( XStructArity(ScratchXTop(1)) ) {
			case 1: {
				Pt arg = ScratchPop() ;
				ScratchTop() =
					EvalArith1(XStructFunctor(ScratchTop()), arg) ;
				break ;
			}
			case 2: {
				Pt e = EvalPre(XStructArg(ScratchXTop(1),1)) ;
				ScratchPush(e) ;
				break ;
			}
			default: TermArithError(ScratchXTop(1)) ;
		}

		else {
			Pt arg2 = ScratchPop() ;
			Pt arg1 = ScratchPop() ;
			ScratchTop() =
				EvalArith2(XStructFunctor(ScratchTop()), arg1, arg2) ;
		}
	}
	t = ScratchPop() ;
	FreeScratch() ;
	return t ;
}


/* CXPROLOG C'BUILTINS */

static void PIs()
{
	Pt t = Drf(X0) ;
#if IVarSupport
	if( IsAtom(t) ) {
		IVarSet(XAtom(t), Evaluate(X1)) ;
		JumpNext()
	}
	else
#endif
	MustBe( UnifyWithNumber(t, Evaluate(X1)) )
}

static void PEq()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == 0 )
}

static void PNe()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) != 0 )
}

static void PLt()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == -1 )
}

static void PGt()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == 1 )
}

static void PLe()
{
	int r = CompareNumber(Evaluate(X0),Evaluate(X1)) ;
	MustBe( r == -1 || r == 0 ) /* supports inf and nan */
}

static void PGe()
{
	int r = CompareNumber(Evaluate(X0),Evaluate(X1)) ;
	MustBe( r == 0 || r == 1 ) /* supports inf and nan */
}

static void PSucc()
{
	Pt t0 = Drf(X0) ;
	if( IsInt(t0) ) {
		PInt i = XInt(t0) + 1 ;
		MustBe( i >= 1 && UnifyWithNumber(X1, MakeInt(i)) )
	}
	elif( IsVar(t0) ) {
		PInt i = XTestInt(X1) - 1 ;
		MustBe( i >= 0 && UnifyWithNumber(t0, MakeInt(i)) )
	}
	XTestInt(X0) ;
}


/* INIT */

void ArithInit()
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

#if IVarSupport
// Possible changelog:
//     * Now, ivars containing numbers can appear in aritmetic expressions.
//	  	An ivar can also be used at the left side of predicate is/2. As an
//		example, this goal makes sense now: a is 3*b+cputime.
//		The predicates involved are these: is/2, =:=/2, </2, >/2, =</2, >=/2.
#endif
