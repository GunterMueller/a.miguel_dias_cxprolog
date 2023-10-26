/*
 *   This file is part of the CxProlog system

 *   Arith.c
 *   by A.Miguel Dias - 1989/12/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL

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


#define MakeISOInt(n)	(iso_flag ? MakeFloat(n) : MakeInt(n))

#if FLOAT_IS_LONG_DOUBLE
#define MATH(f)		f##l
typedef PFloat OpsFloat ;
#else
#define MATH(f)		f
typedef double OpsFloat ;
#endif

/* PRIVATE FUNCTIONS */

static Pt ArithRound(Pt arg, Bool forceInt)
{
	PInt i ;
	if( false ) /* rounds down, this is ISO but ignore! */
		i = MATH(floor)(XAsFloat(arg)) ;
	else { /* rounds outward */
		OpsFloat pint ;
		PFloat pfrac = MATH(modf)(XAsFloat(arg), &pint) ;
		i = pfrac >= 0.5 ? pint + 1
			: pfrac <= -0.5 ? pint - 1
			: pint ;		
	}
	return forceInt ? MakeInt(i) : MakeISOInt(i) ;
}

static Pt ArithIntPart(Pt arg)
{
	OpsFloat pint ;
	MATH(modf)(XAsFloat(arg), &pint) ;
	return MakeISOInt(pint) ;
}

static Pt ArithFracPart(Pt arg)
{
	OpsFloat pint ;
	return MakeFloat(MATH(modf)(XAsFloat(arg), &pint)) ;
}

static Pt ArithIVar(Pt arg)
{
	Pt val ;
	if( IsAtom(arg) && (val = IVarGet(XAtom(arg))) != nil ) {
		if( IsNumber(val) )
			return val ;
		else
			return TypeError2("EVALUABLE", arg,
				"Contents of IVAR '%s' is not numeric", XAtomName(arg)) ;
	}
	else 
		return TypeError2("EVALUABLE", arg,
					"Unknown IVAR '%s' in expression", XAtomName(arg)) ;
}

static VoidPt TermArithError(Pt t)
{
	Str mesg, value ;
	if( IsAtom(t) ) {
		mesg = "Unknown ATOM '%s' in expression" ;
		value = XAtomName(t) ;
		t = MakeSlashTerm(XTermFunctor(t)) ;
	}
	elif( IsExtra(t) ) {
		mesg = "Unknown EXTRA '%s' in expression" ;
		value = XExtraAsStr(t) ;
	}
	elif( IsStruct(t) ) {
		mesg = "Unknown FUNCTOR '%s' in expression" ;
		value = XStructNameArity(t) ;
		t = MakeSlashTerm(XTermFunctor(t)) ;
	}
	elif( IsVar(t) ) {
		mesg = "Unbound VARIABLE in expression" ;
		value = "" ;
	}
	elif( IsList(t) ) {
		mesg = "Invalid LIST in expression" ;
		value = "" ;
	}
	else 	
		return InternalError("TermArithError") ;
	
	return TypeError2("EVALUABLE", t, mesg, value) ;
}


static void ArithIntError(Str op, Pt a1, Pt a2)
{
	TypeError2("INT",
			IsInt(a1) ? a2 : a1,
			GStrFormat("Both args of '%s/2' must be integers", op)) ;
}


static void ZeroDivisorError(void)
{
	EvaluationError("zero_divisor", "Divisor is zero") ;
}

#define chk(s)	StrEqual(op, s)

static Pt EvalArith0(Pt t)
{
	Str op = XAtomName(t) ;
	switch( op[0] ) {
	  case 'c':	if( chk("cputime") )
						return MakeFloat(CpuTime()) ;
				if( chk("currtime") )
						return MakeLLInt(AbsoluteTime()) ;
	  case 'e':	if( chk("e") )
						return eFloatPt ;
				if( chk("epsilon") )
						return epsilonFloatPt ;
	  case 'f':	if( chk("float_size") )
						return MakeInt(CalculateFloatSize()) ;
	  case 'g':	if( chk("global") )
						return MakeInt(WordsAsBytes(LocalStackUsed())) ;
	  case 'h':	if( chk("heapused") )
						return MakeInt(WordsAsBytes(ProgramSpaceUsed())) ;
	  case 'i':	if( chk("int_size") )
						return MakeInt(CalculateIntSize()) ;
				if( chk("inf") )
						return infFloatPt ;
	  case 'l':	if( chk("local") )
						return MakeInt(WordsAsBytes(GlobalStackUsed())) ;
	  case 'm':	if( chk("max_int") )
						return maxIntPt ;
				if( chk("min_int") )
						return minIntPt ;
	  case 'n':	if( chk("nan") )
						return nanFloatPt ;
	  case 'p':	if( chk("pi") )
						return piFloatPt ;
	  case 'r':	if( chk("random") || chk("random_float") )
						return MakeFloat(FloatRandom()) ;
	}
	return TermArithError(t) ;
}

static Pt EvalArith1(FunctorPt f, Pt a)
{
	Str op = FunctorName(f) ;
	PFloat af = XAsFloat(a) ;
	switch( op[0] ) {
	  case '+':	if( chk("+") )
						return a ;
	  case '-':	if( chk("-") ) {
						if( IsInt(a) ) return MakeInt(-XInt(a)) ;
						 else return MakeFloat(-af) ;
				}
	 case '\\':	if( chk("\\") ) {
						if( IsInt(a) ) return MakeInt(~XInt(a)) ;
						else TypeError2("INT", a,
								"The arg of '\\/1' must be an integer") ;
				}
	  case 'a':	if( chk("asin") ) {
						if( arithmetic_checking_flag && fabs(af) > 1 )
							DomainError("asin/1 abs(ARG)<=1", a) ;
						else return MakeFloat(MATH(asin)(af)) ;
				}
				if( chk("acos") ) {
						if( arithmetic_checking_flag && fabs(af) > 1 )
							DomainError("acos/1 abs(ARG)<=1", a) ;
						else return MakeFloat(MATH(acos)(af)) ;
				}
				if( chk("atan") )
						return MakeFloat(MATH(atan)(af)) ;
				if( chk("abs") ) {
						if( IsInt(a) ) {
							PInt i = XInt(a) ;
							return i >= 0 ? a : MakeInt(-i) ;
						}
						else {
							PFloat f = XFloat(a) ;
							return f >= 0 ? a : MakeFloat(-f) ;
						}
				}
	  case 'c':	if( chk("cos") )
						return MakeFloat(MATH(cos)(af)) ;
				if( chk("ceil") )
						return MakeISOInt(MATH(ceil)(af)) ;
				if( chk("ceiling") )
						return MakeISOInt(MATH(ceil)(af)) ;
	  case 'e':	if( chk("exp") )
						return MakeFloat(MATH(exp)(af)) ;
	  case 'f':	if( chk("floor") )
						return MakeISOInt(MATH(floor)(af)) ;
				if( chk("float") )
						return MakeFloat(af) ; ;
				if( chk("float_integer_part") )
						return ArithIntPart(a) ;
				if( chk("float_fractional_part") )
						return ArithFracPart(a) ;
	  case 'i':	if( chk("integer") )
						return ArithRound(a, true) ;
	  case 'l':	if( chk("log") ) {
						if( arithmetic_checking_flag && af <= 0 )
							DomainError("log/1 ARG>0", a) ;
						else return MakeFloat(MATH(log)(af)) ;
				}
				if( chk("log10") ) {
						if( arithmetic_checking_flag && af <= 0 )
							DomainError("log10/1 ARG>0", a) ;
						else return MakeFloat(MATH(log10)(af)) ;
				}
	  case 'r':	if( chk("random") ) {
						if( IsPos(a) ) return MakeInt(IntRandom(XInt(a))) ;
						else TypeError2("INT", a, "'random/1' expected a positive integer") ;
				}
				if( chk("round") )
						return ArithRound(a, false) ;
	  case 's':	if( chk("sqrt") ) {
						if( arithmetic_checking_flag && af < 0 )
							DomainError("sqrt/1 ARG>=0", a) ;
						else return MakeFloat(MATH(sqrt)(af)) ;
				}
				if( chk("sin") )
						return MakeFloat(MATH(sin)(af)) ;
				if( chk("sign") ) {				
						PFloat f = XAsFloat(a) ;
						return f == 0 ? zeroIntPt
							: f > 0 ? oneIntPt
							: minusOneIntPt ;
				}
	  case 't':	if( chk("tan") )
						return MakeFloat(MATH(tan)(af)) ;
				if( chk("truncate") )
						return ArithIntPart(a) ;
	}
	return TermArithError(MakeUnStruct(f, a)) ;
}

static Pt EvalArith2Int(Str op, PInt a1, PInt a2)
{
	switch( op[0] ) {
	  case '+':	if( chk("+") )
						return MakeInt(a1 + a2) ;
	  case '-':	if( chk("-") )
						return MakeInt(a1 - a2) ;
	  case '*':	if( chk("*") )
						return MakeInt(a1 * a2) ;
				if( chk("**") )
						return MakeFloat(pow(a1, a2)) ;
	  case '/':	if( chk("/") ) {
					if(arithmetic_checking_flag && a2 == 0) ZeroDivisorError() ;
					else return a2 != 0 && a1 % a2 == 0
							? MakeISOInt(a1 / a2)
							: MakeFloat(cPFloat(a1) / cPFloat(a2)) ;
				}
				if( chk("//") ) {
					if( a2 == 0 ) ZeroDivisorError() ;
					else {
						ldiv_t  r = ldiv(a1, a2) ;
						return MakeInt(r.quot) ;
					}
				}
				if( chk("/\\") )
						return MakeInt(a1 & a2) ;
	 case '\\':	if( chk("\\/") )
						return MakeInt(a1 | a2) ;
	  case '^':	if( chk("^") )
						return MakeFloat(pow(a1, a2)) ;
	  case '>':	if( chk(">>") )
						return MakeInt(a1 >> a2) ;
	  case '<':	if( chk("<<") )
						return MakeInt(a1 << a2) ;
	  case 'd':	if( chk("div") ) {
					if( a2 == 0 ) ZeroDivisorError() ;
					else {
						ldiv_t r = ldiv(a1, a2) ;
						if( r.quot < 0 && r.rem != 0 )
							r.quot-- ;
						return MakeInt(r.quot) ;
					}
				}
	  case 'e':	if( chk("exp") )
						return MakeFloat(pow(a1, a2)) ;
	  case 'm':	if( chk("max") )
						return MakeInt(a1 >= a2 ? a1 : a2) ;
				if( chk("min") )
						return MakeInt(a1 <= a2 ? a1 : a2) ;
				if( chk("mod") ) {
					if( a2 == 0 ) ZeroDivisorError() ;
					else {
						ldiv_t r = ldiv(a1, a2) ;
						if( r.quot < 0 && r.rem != 0 )
							r.rem += a2 ;
						return MakeInt(r.rem) ;
					}
				}
	  case 'r':	if( chk("rem") ) {
					if( a2 == 0 ) ZeroDivisorError() ;
					else {
						ldiv_t r = ldiv(a1, a2) ;
						return MakeInt(r.rem) ;
					}
				}				
	}
	return nil ;
}

static Pt EvalArith2Float(Str op, Pt a1, Pt a2)
{
	PFloat a1v = XAsFloat(a1), a2v = XAsFloat(a2) ;
	switch( op[0] ) {
	  case '+':	if( chk("+") )
						return MakeFloat(a1v + a2v) ;
	  case '-':	if( chk("-") )
						return MakeFloat(a1v - a2v) ;
	  case '*':	if( chk("*") )
						return MakeFloat(a1v * a2v) ;
				if( chk("**") )
						return MakeFloat(pow(a1v, a2v)) ;
	  case '/':	if( chk("/") ) {
					if( arithmetic_checking_flag && a2v == 0 ) ZeroDivisorError() ;
					else return MakeFloat(a1v / a2v) ;
				}
				if( chk("//") )
						ArithIntError(op, a1, a2) ;
				if( chk("/\\") )
						ArithIntError(op, a1, a2) ;
	 case '\\':	if( chk("\\/") )
						ArithIntError(op, a1, a2) ;
	  case '^':	if( chk("^") )
						return MakeFloat(pow(a1v, a2v)) ;
	  case '>':	if( chk(">>") )
						ArithIntError(op, a1, a2) ;
	  case '<':	if( chk("<<") )
						ArithIntError(op, a1, a2) ;
	  case 'd':	if( chk("div") )
						ArithIntError(op, a1, a2) ;
	  case 'e':	if( chk("exp") )
						return MakeFloat(pow(a1v, a2v)) ;
	  case 'm': if( chk("max") )
						return a1v >= a2v ? a1 : a2 ;
				if( chk("min") )
						return a1v <= a2v ? a1 : a2 ;
				if( chk("mod") )
						ArithIntError(op, a1, a2) ;
	  case 'r':	if( chk("rem") )
						ArithIntError(op, a1, a2) ;
	}
	return nil ;
}

static Pt EvalArith2(FunctorPt f, Pt a1, Pt a2)
{
	Pt res =
		IsInt(a1) && IsInt(a2)
			? EvalArith2Int(FunctorName(f), XInt(a1), XInt(a2))
			: EvalArith2Float(FunctorName(f), a1, a2) ;
	return res == nil
			? TermArithError(MakeBinStruct(f, a1, a2))
			: res ;
}

static Pt EvalPre(register Pt t)
{
redo:
	VarValue(t) ;
	if( IsNumber(t) ) return t ;
	if( IsStruct(t) )
		return IsUnitParam(t) ? Z(XUnitParam(t))
			 : IsThisStruct(t, iVarFunctor) ? ArithIVar(XStructArg(t, 0))
			 : t ;
	if( IsAtom(t) ) return EvalArith0(t) ;
	if( IsList(t) && compatibleIfThen_flag && Drf(XListTail(t)) == tNilAtom ) {
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

static void PIs(void)
{
	MustBe( UnifyWithNumber(Drf(X0), Evaluate(X1)) ) ;
}

static void PEq(void)
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == 0 ) ;
}

static void PNe(void)
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) != 0 ) ;
}

static void PLt(void)
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == -1 ) ;
}

static void PGt(void)
{
	MustBe( CompareNumber(Evaluate(X0),Evaluate(X1)) == 1 ) ;
}

static void PLe(void)
{
	int r = CompareNumber(Evaluate(X0),Evaluate(X1)) ;
	MustBe( r == -1 || r == 0 ) ; /* supports inf and nan */
}

static void PGe(void)
{
	int r = CompareNumber(Evaluate(X0),Evaluate(X1)) ;
	MustBe( r == 0 || r == 1 ) ; /* supports inf and nan */
}

static void PSucc(void)
{
	X0 = TestNatOrVar(X0) ;
	X1 = TestNatOrVar(X1) ;
	if( IsInt(X0) ) {
		if( X0 == maxIntPt )
			DomainError("INT<MAX_INT", X0) ;
		MustBe( UnifyWithNumber(X1, IncIntPt(X0)) ) ;
	}
	if( IsInt(X1) )
		MustBe( X1 != zeroIntPt && UnifyWithNumber(X0, DecIntPt(X1)) ) ;
	TypeError("INT>=0",X0) ;
}

static void PPlus(void)
{
	X0 = TestIntOrVar(X0) ;
	X1 = TestIntOrVar(X1) ;
	X2 = TestIntOrVar(X2) ;
	if( IsInt(X0) && IsInt(X1) )
		MustBe( UnifyWithNumber(X2, MakeInt(XInt(X0) + XInt(X1))) ) ;
	elif( IsInt(X0) && IsInt(X2) )
		MustBe( UnifyWithNumber(X1, MakeInt(XInt(X2) - XInt(X0))) ) ;
	elif( IsInt(X1) && IsInt(X2) )
		MustBe( UnifyWithNumber(X0, MakeInt(XInt(X2) - XInt(X1))) ) ;
	TypeError2("INT", X0, "Arguments are insufficiently instantiated") ;
}

static void PNDBetween(void)
{
	if( A(3) == tNilAtom ) {
		A(0) = MakeInt(XTestInt(A(0))) ;
		A(1) = Drf(A(1)) ;
		if( !IsInt(A(1)) && (A(1) == infFloatPt || A(1) == MakeAtom("inf")) )
			A(1) = maxIntPt ;
		A(1) = MakeInt(XTestInt(A(1))) ;
		A(2) = Drf(A(2)) ;
		if( IsInt(A(2)) ) {
			Bool b = CompareNumber(A(0), A(2)) <= 0
					&& CompareNumber(A(2), A(1)) <= 0 ;
			Discard() ;
			MustBe( b ) ;	
		}
		elif( !IsVar(A(2)) )
			XTestInt(A(2)) ;
		A(3) = DecIntPt(A(0)) ;
	}
	if( A(3) == A(1) ) Jump(DiscardAndFail) ;
	A(3) = IncIntPt(A(3)) ;
	if( UnifyWithNumber(X2, A(3)) ) JumpNext() ;
	InternalError("PNDBetween") ;
}



/* INIT */

void ArithInit(void)
{
	InstallCBuiltinPred("is", 2, PIs) ;
	InstallCBuiltinPred("=:=", 2, PEq) ;
	InstallCBuiltinPred("=\\=", 2, PNe) ;	/* Use of escape '\' */
	InstallCBuiltinPred("<", 2, PLt) ;
	InstallCBuiltinPred(">", 2, PGt) ;
	InstallCBuiltinPred("=<", 2, PLe) ;
	InstallCBuiltinPred(">=", 2, PGe) ;
	InstallCBuiltinPred("succ", 2, PSucc) ;
	InstallCBuiltinPred("plus", 3, PPlus) ;
	InstallNDeterCBuiltinPred("between", 3, PNDBetween) ;
}
