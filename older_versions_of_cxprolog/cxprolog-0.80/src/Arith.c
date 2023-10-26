/*
 *   This file is part of the CxProlog system

 *   Arith.c
 *   by A.Miguel Dias - 1989/12/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define chk(s)	EqualStr(name, s)

static Pt EvalArith0(Pt t)
{
	CharPt name = XAtomName(t) ;
	switch( name[0] ) {
		case 'c': if( chk("cputime") ) return MakeFloat(CurrTime()) ;
		case 'f': if( chk("float_size") ) return MakeInt(floatSize) ;	
		case 'h': if( chk("heapused") )
					return MakeInt(WordsAsBytes(StaticMemoryUsed())) ;
		case 'i': if( chk("int_size") ) return MakeInt(intSize) ;	
				  if( chk("inf") ) return MakeFloat(infFloat) ;
		case 'm': if( chk("max_int") ) return MakeInt(maxInt) ;
				  if( chk("min_int") ) return MakeInt(minInt) ;
		case 'p': if( chk("pi") )
					return MakeFloat(3.141592653589793233846) ;	
	}
	TermArithError(t) ;
}

static Pt EvalArith1(FunctorPt f, Pt arg)
{
	CharPt name = FunctorName(f) ;
	switch( name[0] ) {
		case '-': if( chk("-") )
					if( IsInt(arg) ) return MakeInt(-XInt(arg)) ;
					else return MakeFloat(-XAsFloat(arg)) ;
		case '\\': if( chk("\\") )
					if( IsInt(arg) ) return MakeInt(~XInt(arg)) ;
					else ArithError("Arg of '\\/2' must be an integer") ;
		case 'a': if( chk("asin") ) return MakeFloat(asin(XAsFloat(arg))) ;
				  if( chk("acos") ) return MakeFloat(acos(XAsFloat(arg))) ;
				  if( chk("atan") ) return MakeFloat(atan(XAsFloat(arg))) ;
		case 'c': if( chk("cos") ) return MakeFloat(cos(XAsFloat(arg))) ;
				  if( chk("ceil") ) return MakeInt(ceil(XAsFloat(arg))) ;
		case 'e': if( chk("exp") ) return MakeFloat(exp(XAsFloat(arg))) ;
		case 'f': if( chk("floor") ) return MakeInt(floor(XAsFloat(arg))) ;
		case 'l': if( chk("log") ) return MakeFloat(log(XAsFloat(arg))) ;
				  if( chk("log10") ) return MakeFloat(log10(XAsFloat(arg))) ;
		case 's': if( chk("sqrt") ) return MakeFloat(sqrt(XAsFloat(arg))) ;
				  if( chk("sin") ) return MakeFloat(sin(XAsFloat(arg))) ;
		case 't': if( chk("tan") ) return MakeFloat(tan(XAsFloat(arg))) ;
	}
	TermArithError(TagStruct(&f)) ;
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
				  if( chk("//") )
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) / XInt(arg2)) ;
					else ArithError("Both args of '///2' must be integers") ;
				  if( chk("/\\") )
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) & XInt(arg2)) ;
					else ArithError("Both args of '/\\/2' must be integers") ;
		case '\\': if( chk("\\/") )
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) | XInt(arg2)) ;
					else ArithError("Both args of '\\//2' must be integers") ;
		case '^': if( chk("^") )
					return MakeFloat(pow(XAsFloat(arg1), XAsFloat(arg2))) ;
		case '>': if( chk(">>") )
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) >> XInt(arg2)) ;
					else ArithError("Both args of '>>/2' must be integers") ;
		case '<': if( chk("<<") )
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) << XInt(arg2)) ;
					else ArithError("Both args of '<</2' must be integers") ;
		case 'm': if( chk("mod") )
					if( IsInt(arg1) && IsInt(arg2) )
						return MakeInt(XInt(arg1) % XInt(arg2)) ;
					else ArithError("Both args of 'mod/2' must be integers") ;
	}
	TermArithError(TagStruct(&f)) ;
}

static Pt EvalPre(register Pt t)
{
redo:
	VarValue(t) ;
	if( IsNumber(t) || IsStruct(t) ) return t ;
	if( IsAtom(t) ) return EvalArith0(t) ;	
	if( IsList(t) && compatibleStrings_flag ) {
		if( Drf(XListTail(t)) != tNilAtom )
			ArithError("Expression list must have a single component") ;
		t = XListHead(t) ;
		goto redo ;
	}
	TermArithError(t) ;
}

static Pt Evaluate(register Pt t)
{
	UseBuffer() ;
	BufferPush(EvalPre(t)) ;
	for(;;) {
		if( IsStruct(BufferTop()) )
			BufferPush(EvalPre(XStructArg(BufferTop(),0))) ;
			
		elif( BufferUsed() == 1 ) break ;

		elif( IsStruct(BufferXTop(1)) )
		  switch( XStructArity(BufferXTop(1)) ) {
			case 1: {
				Pt arg = BufferPop() ;
				BufferTop() =
					EvalArith1(XStructFunctor(BufferTop()), arg) ;
				break ;
			}
			case 2: {
				BufferPush(EvalPre(XStructArg(BufferXTop(1),1))) ;
				break ;
			}
			default: TermArithError(BufferXTop(1)) ;
		}

		else {
			Pt arg2 = BufferPop() ;
			Pt arg1 = BufferPop() ;
			BufferTop() =
				EvalArith2(XStructFunctor(BufferTop()), arg1, arg2) ;
		}
	}
	t = BufferPop() ;
	FreeBuffer() ;
	return t ;
}



/* CXPROLOG C'BUILTINS */

static void PIs()
{
	if( UnifyWithNumber(X0, Evaluate(X1)) ) JumpNext()
	DoFail()
}

static void PEq()
{
	if( Eq(Evaluate(X0),Evaluate(X1)) ) JumpNext()
	DoFail()
}

static void PNe()
{
	if( not Eq(Evaluate(X0),Evaluate(X1)) ) JumpNext()
	DoFail()
}

static void PLt()
{
	if( CompareNumber(Evaluate(X0),Evaluate(X1)) < 0 ) JumpNext()
	DoFail()
}

static void PGt()
{
	if( CompareNumber(Evaluate(X0),Evaluate(X1)) > 0 ) JumpNext()
	DoFail()
}

static void PLe()
{
	if( CompareNumber(Evaluate(X0),Evaluate(X1)) <= 0 ) JumpNext()
	DoFail()
}

static void PGe()
{
	if( CompareNumber(Evaluate(X0),Evaluate(X1)) >= 0 ) JumpNext()
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
	InstallCBuiltinPred("is", 2, PIs) ;
	InstallCBuiltinPred("=:=", 2, PEq) ;
	InstallCBuiltinPred("=\\=", 2, PNe) ;	/* Use of escape '\' */
	InstallCBuiltinPred("<", 2, PLt) ;
	InstallCBuiltinPred(">", 2, PGt) ;
	InstallCBuiltinPred("=<", 2, PLe) ;
	InstallCBuiltinPred(">=", 2, PGe) ;
	InstallCBuiltinPred("succ", 2, PSucc) ;
}
