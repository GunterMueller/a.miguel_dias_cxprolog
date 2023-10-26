/*
 *   This file is part of the CxProlog system

 *   Arith.c
 *   by A.Miguel Dias - 1989/12/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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

#ifndef __APPLE__
#ifdef FP_ILOGB0
#define HAS_LONG_DOUBLE_OPS		1
#endif
#endif

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
	switch( name[0] ) {
		case 'c': if( chk("cputime") ) return MakeFloat(CpuTime()) ;
				  if( chk("currtime") ) return MakeLLInt(AbsoluteTime()) ;
		case 'e': if( chk("e") ) return eFloatPt ;	
		case 'f': if( chk("float_size") ) return MakeInt(floatSize) ;	
		case 'h': if( chk("heapused") )
					return MakeInt(WordsAsBytes(CodeAreaUsed())) ;
		case 'i': if( chk("int_size") ) return MakeInt(intSize) ;	
				  if( chk("inf") ) return infFloatPt ;
		case 'm': if( chk("max_int") ) return maxIntPt ;
				  if( chk("min_int") ) return MakeInt(minInt) ;
		case 'n': if( chk("nan") ) return nanFloatPt ;
		case 'p': if( chk("pi") ) return piFloatPt ;	
		case 'r': if( chk("random") ) return MakeFloat(FloatRandom()) ;
	}
#if IVarSupport
	if( IsAtom(t) && AtomToIVar(XAtom(t)) != nil ) {
		Pt val = IVarGet(XAtom(t)) ;
		if( IsNumber(val) ) return val ;
	}
#endif
	return TermArithError(t) ;
}

#if HAS_LONG_DOUBLE_OPS
#define MATH(f)		f##l
#else
#define MATH(f)		f
#endif

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
		case 'a': if( chk("asin") ) return MakeFloat(MATH(asin)(XAsFloat(arg))) ;
				  if( chk("acos") ) return MakeFloat(MATH(acos)(XAsFloat(arg))) ;
				  if( chk("atan") ) return MakeFloat(MATH(atan)(XAsFloat(arg))) ;
		case 'c': if( chk("cos") ) return MakeFloat(MATH(cos)(XAsFloat(arg))) ;
				  if( chk("ceil") ) return MakeFloat(MATH(ceil)(XAsFloat(arg))) ;
		case 'e': if( chk("exp") ) return MakeFloat(MATH(exp)(XAsFloat(arg))) ;
		case 'f': if( chk("floor") ) return MakeFloat(MATH(floor)(XAsFloat(arg))) ;
		case 'l': if( chk("log") ) return MakeFloat(MATH(log)(XAsFloat(arg))) ;
				  if( chk("log10") ) return MakeFloat(MATH(log10)(XAsFloat(arg))) ;
		case 'r': if( chk("random") ) {
					if( IsPos(arg) ) return MakeInt(IntRandom(XInt(arg))) ;
					else ArithError("The arg of 'random/1' must be a positive integer") ;
				  }
				  if( chk("round") ) { /* works even if ISOC99 is not available */
					PFloat pint, pfrac = MATH(modf)(XAsFloat(arg), &pint) ;
					return MakeFloat(pfrac >= 0.5 ? pint + 1
									: pfrac <= -0.5 ? pint - 1 : pint) ;
				  }
		case 's': if( chk("sqrt") ) return MakeFloat(MATH(sqrt)(XAsFloat(arg))) ;
				  if( chk("sin") ) return MakeFloat(MATH(sin)(XAsFloat(arg))) ;
		case 't': if( chk("tan") ) return MakeFloat(MATH(tan)(XAsFloat(arg))) ;
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

Bool IsIntFloat(PFloat f)
{
	return f == MATH(floor)(f) ;
}

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
		JumpNext() ;
	}
	else
#endif
	MustBe( UnifyWithNumber(t, Evaluate(X1)) ) ;
}

static void PEq()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == 0 ) ;
}

static void PNe()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) != 0 ) ;
}

static void PLt()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == -1 ) ;
}

static void PGt()
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == 1 ) ;
}

static void PLe()
{
	int r = CompareNumber(Evaluate(X0),Evaluate(X1)) ;
	MustBe( r == -1 || r == 0 ) ; /* supports inf and nan */
}

static void PGe()
{
	int r = CompareNumber(Evaluate(X0),Evaluate(X1)) ;
	MustBe( r == 0 || r == 1 ) ; /* supports inf and nan */
}

static void PSucc()
{
	X0 = TestIntOrVar(X0) ;
	X1 = TestIntOrVar(X1) ;
	if( IsInt(X0) ) {
		if( IsNeg(X0) )
			Error("First argument cannot be negative") ;
		if( X0 == maxIntPt )
			Error("First argument cannot be the 'max_int' constant (overflow)") ;
		MustBe( UnifyWithNumber(X1, IncIntPt(X0)) ) ;
	}
	if( IsInt(X1) ) {
		if( IsNeg(X1) )
			Error("Second argument cannot be negative") ;
		MustBe( X1 != zeroIntPt && UnifyWithNumber(X0, DecIntPt(X1)) ) ;
	}
	Error("Arguments insufficiently instantiated") ;
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
