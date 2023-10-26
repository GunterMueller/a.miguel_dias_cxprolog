/*
 *   This file is part of the CxProlog system

 *   Operator.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

static OperatorPt opList = nil ;

static OperatorPt NewOperator(AtomPt atom)
{
	register OperatorPt op ;

	if( localOperators_flag && UnitIsPermanent(CurrUnit()) ) {
		if( NoCurrUnit() )
			DatabaseError("No current unit when attempting to define "
							"new operator '%s'", AtomName(atom)) ;
		else
			DatabaseError("Cannot create the new operator '%s' in "
							"the system unit", AtomName(atom)) ;
	}

	op = PermAllocate(WordsOf(Operator)) ;
	OperatorAtom(op) = atom ;
	OperatorUnit(op) = localOperators_flag ? CurrUnit() : systemUnit ;
	op->prePriority = 0 ;
	op->inPriority = 0 ;
	op->posPriority = 0 ;
	op->isBuiltin = false ;
	op->forceParenthesis = false ;
	AtomToOperator(atom) = op ;
	AtomIsPermanent(atom) = true ;
	op->next = opList ;
	opList = op ;
	return op ;
}

static OperatorPt LookupOperatorByName(CharPt n, Bool change)
{
	AtomPt atom = LookupAtom(n) ;
	OperatorPt op = AtomToOperator(atom) ;
	if( op != nil ) {
		if( localOperators_flag && change && OperatorIsBuiltin(op) )
			DatabaseError("Attempt to change built-in operator '%s'",
													OperatorName(op)) ;
		return op ;
	}
	return NewOperator(atom) ;
}

int Prefix(AtomPt atom, int *rp)
{
	register OperatorPt op ;
	if( (op = AtomToOperator(atom)) == nil ) return 0 ;
	if( op->prePriority == 0 ) return 0 ;
	*rp = op->prePriority - 1 + Logic(op->preAssoc) ;
	return op->prePriority ;
}

int Infix(AtomPt atom, int *lp, int *rp)
{
	register OperatorPt op ;
	if( (op = AtomToOperator(atom)) == nil ) return 0 ;
	if( op->inPriority == 0 ) return 0 ;
	*lp = op->inPriority - 1 + Logic(op->inLAssoc) ;
	*rp = op->inPriority - 1 + Logic(op->inRAssoc) ;
	return op->inPriority ;
}

int Postfix(AtomPt atom, int *lp)
{
	register OperatorPt op ;
	if( (op = AtomToOperator(atom)) == nil ) return 0 ;
	if( op->posPriority == 0 ) return 0 ;
	*lp = op->posPriority - 1 + Logic(op->posAssoc) ;
	return op->posPriority ;
}

static void MakePrefixOperator(CharPt n, int p, Bool a)
{
	register OperatorPt op = LookupOperatorByName(n, true) ;	
	op->prePriority = p ;
	op->preAssoc = a ;
}

static void MakeInfixOperator(CharPt n, int p, Bool la, Bool ra)
{
	register OperatorPt op = LookupOperatorByName(n, true) ;
	op->inPriority = p ;
	op->inLAssoc = la ;
	op->inRAssoc = ra ;
}

static void MakePosfixOperator(CharPt n, int p, Bool a)
{
	register OperatorPt op = LookupOperatorByName(n, true) ;
	op->posPriority = p ;
	op->posAssoc = a ;
}

void ResetOperators()
{
/* xfx -- infix non associative */
	MakeInfixOperator(":-",	   maxPrec, false, false) ;		/* ISO */
	MakeInfixOperator("-->",   maxPrec, false, false) ;		/* ISO */
	MakeInfixOperator("<=",	   maxPrec, false, false) ;
	MakeInfixOperator("<->",	  1190, false, false) ;
	MakeInfixOperator("<-",		  1190, false, false) ;
	MakeInfixOperator("from",	  1050, false, false) ; /* import . from . */
	MakeInfixOperator("?:=",       990, false, false) ;
	MakeInfixOperator("&:=",       990, false, false) ;
	MakeInfixOperator("#:=",       990, false, false) ;
	MakeInfixOperator(":=",	       990, false, false) ;
	MakeInfixOperator("=:",	       990, false, false) ;
	MakeInfixOperator("until",	   980, false, false) ;
	MakeInfixOperator("unless",	   980, false, false) ;
	MakeInfixOperator("===",	   700, false, false) ;
	MakeInfixOperator("=:=",	   700, false, false) ;		/* ISO */
	MakeInfixOperator("=\\=",	   700, false, false) ;		/* ISO */
	MakeInfixOperator("<",		   700, false, false) ;		/* ISO */
	MakeInfixOperator(">=",		   700, false, false) ;		/* ISO */
	MakeInfixOperator(">",		   700, false, false) ;		/* ISO */
	MakeInfixOperator("=<",		   700, false, false) ;		/* ISO */
	MakeInfixOperator("is",		   700, false, false) ;		/* ISO */
	MakeInfixOperator("=..",	   700, false, false) ;		/* ISO */
	MakeInfixOperator("=",		   700, false, false) ;		/* ISO */
	MakeInfixOperator("\\=",	   700, false, false) ;		/* ISO */
	MakeInfixOperator("==",		   700, false, false) ;		/* ISO */
	MakeInfixOperator("\\==",	   700, false, false) ;		/* ISO */
	MakeInfixOperator("@<",		   700, false, false) ;		/* ISO */
	MakeInfixOperator("@>=",	   700, false, false) ;		/* ISO */
	MakeInfixOperator("@>",		   700, false, false) ;		/* ISO */
	MakeInfixOperator("@=<",	   700, false, false) ;		/* ISO */
	MakeInfixOperator("**" ,	   200, false, false) ;		/* ISO */

/* xfy -- infix right associative */
	MakeInfixOperator(";",	   barPrec, false, true) ;		/* ISO */
	MakeInfixOperator("->",		  1050, false, true) ;		/* ISO */
	MakeInfixOperator(",",   commaPrec, false, true) ;		/* ISO */
	MakeInfixOperator(".",		   999, false, true) ;
	MakeInfixOperator(":" ,		   600, false, false) ;
	MakeInfixOperator(">>",		   400, false, true) ; /* violates ISO */
	MakeInfixOperator("<>",		   400, false, true) ;
	MakeInfixOperator("^",		   200, false, true) ;		/* ISO */

/* yfx -- infix left associative */
	MakeInfixOperator("+",		   500, true, false) ;		/* ISO */
	MakeInfixOperator("-",		   500, true, false) ;		/* ISO */
	MakeInfixOperator("\\/",	   500, true, false) ;		/* ISO */
	MakeInfixOperator("/\\",	   500, true, false) ;		/* ISO */
	MakeInfixOperator("*",		   400, true, false) ;		/* ISO */
	MakeInfixOperator("/",		   400, true, false) ;		/* ISO */
	MakeInfixOperator("div",	   400, true, false) ;
	MakeInfixOperator("mod",	   400, true, false) ;		/* ISO */
	MakeInfixOperator("rem",	   400, true, false) ;		/* ISO */
	MakeInfixOperator("//",		   400, true, false) ;
	MakeInfixOperator("<<",		   400, true, false) ;		/* ISO */

/* fx -- prefix non associative */
	MakePrefixOperator(":-",		    maxPrec, false) ;	/* ISO */
	MakePrefixOperator("?-",		    maxPrec, false) ;	/* ISO */
	MakePrefixOperator("dynamic",		   1150, false) ;
	MakePrefixOperator("dynamic_iu",	   1150, false) ;
	MakePrefixOperator("visible",		   1150, false) ;
	MakePrefixOperator("gopen",			   1150, false) ;
	MakePrefixOperator("gclose",		   1150, false) ;
	MakePrefixOperator("import",		   1150, false) ;	/* import . from . */
	MakePrefixOperator("multifile",		   1150, false) ;
	MakePrefixOperator("discontiguous",	   1150, false) ;
	MakePrefixOperator("code",			   1150, false) ;
	MakePrefixOperator("spy",			   1150, false) ;
	MakePrefixOperator("nospy",			   1150, false) ;
	MakePrefixOperator("gen",			   990, false) ;
	MakePrefixOperator("try",			   980, false) ;
	MakePrefixOperator("once",			   970, false) ;
	MakePrefixOperator("possible",		   970, false) ;
	MakePrefixOperator("unit",			   900, false) ;
	MakePrefixOperator("push",			   900, false) ;
	MakePrefixOperator("+",				   500, false) ;
//	MakePrefixOperator("-",				   500, false) ;		/* ??? */
	MakePrefixOperator("@",					10, false) ;
	MakePrefixOperator("@@",				10, false) ;

/* fy -- prefix associative */
	MakePrefixOperator("not",			   900, true) ;
	MakePrefixOperator("\\+",			   900, true) ;		/* ISO */
	MakePrefixOperator("down",			   900, true) ;
	MakePrefixOperator(">",				   700, true) ;
	MakePrefixOperator("<",				   700, true) ;
	MakePrefixOperator("-",				   200, true) ;		/* ISO */
	MakePrefixOperator("\\",			   200, true) ;		/* ISO */

/* xf -- postfix non associative */
	MakePosfixOperator("!",		 		   990, false) ;
	MakePosfixOperator("#",		 		   990, false) ;
}

void DefineOperator(int p, CharPt type, register Pt ops)
{
	CharPt opName ;
	if( !InRange(p, 0, maxPrec) )
		DatabaseError("Precedence (%d) out of range [1,%d]", p, maxPrec) ;
	ops = Drf(ops) ;
	while( ops != tNilAtom ) {
		if( IsAtom(ops) ) {
			opName = XAtomName(ops) ;
			ops = tNilAtom ;
		}
		elif( IsList(ops) ) {
			Pt t = Drf(XListHead(ops)) ;
			if( IsAtom(t) )
				opName = XAtomName(t) ;
			else {
				opName = nil ;
				DatabaseError("Operator names must be atoms or texts") ;
			}
			ops = Drf(XListTail(ops)) ;
		}
		else {
			opName = nil ;
			DatabaseError("Invalid operator specification") ;
		}
		
		if( StrEqual(type, "xfx") )
			MakeInfixOperator(opName, p, false, false) ;
		elif( StrEqual(type, "xfy") )
			MakeInfixOperator(opName, p, false, true) ;
		elif( StrEqual(type, "yfx") )
			MakeInfixOperator(opName, p, true, false) ;
		elif( StrEqual(type, "fx") )
			MakePrefixOperator(opName, p, false) ;
		elif( StrEqual(type, "fy") )
			MakePrefixOperator(opName, p, true) ;
		elif( StrEqual(type, "xf") )
			MakePosfixOperator(opName, p, false) ;
		elif( StrEqual(type, "yf") )
			MakePosfixOperator(opName, p, true) ;
		else DatabaseError("Invalid associativity specification") ;
	}
}


/* CXPROLOG C'BUILTINS */

static void POperator()
{
	DefineOperator(XTestInt(X0), XTestAtomName(X1), X2) ;
	JumpNext() ;
}

static void PResetOperators()
{
	ResetOperators() ;
	JumpNext() ;
}

static void PNDCurrentOperator()
{
	OperatorPt o = A(1) == tNilAtom
				? opList
				: cOperatorPt(A(1))->next ;
	A(1) = cPt(o) ;
	if( o == nil ) Jump(DiscardAndFail) ;
	MustBe( Unify(X0, TagAtom(OperatorAtom(o))) ) ;
}

static void PIsOperator()
{
	int p, lp, rp ;
	switch( XTestInt(X1) )
	{
		case 0:	lp = p = Prefix(XTestAtom(X0), &rp) ; break ;
		case 1:	p = Infix(XTestAtom(X0), &lp, &rp) ; break ;
		case 2:	rp = p = Postfix(XTestAtom(X0), &lp) ; break ;
		default: DatabaseError("Second argument should be 0, 1 ou 2") ; return ;
	}
	MustBe( p != 0 &&
			UnifyWithNumber(X2, MakeInt(p)) &&
			UnifyWithNumber(X3, MakeInt(lp)) &&
			UnifyWithNumber(X4, MakeInt(rp)) ) ;
}

static void POperators()
{
	OperatorPt op ;
	ShowVersion() ;
	Write("OPS:\n") ;
	doseq(op, opList, op->next) {
		Write(" %16.16s -> ", OperatorName(op)) ;
		if( op->prePriority )
			Write("%4d/%s ", op->prePriority, op->preAssoc ? "fy " : "fx ") ;
		if( op->inPriority )
			Write("%4d/%s ", op->inPriority,
				op->inLAssoc ? (op->inRAssoc ? "yfy" : "yfx")
							 : (op->inRAssoc ? "xfy" : "xfx")) ;
		if( op->posPriority )
			Write("%4d/%s ", op->posPriority, op->posAssoc ? "yf " : "xf ") ;
		Write("\n") ;
	}
	JumpNext() ;
}

static void POperatorFlag3()
{
	OperatorPt op = AtomToOperator(XTestAtom(X0)) ;
	CharPt fl = XTestAtomName(X1) ;
	Ensure( op != nil ) ;
	if( StrEqual(fl, "parenthesised") ) {
		Pt t = OperatorParenthesised(op) ? tOnAtom : tOffAtom ;
		MustBe( UnifyWithAtomic(X2, t) ) ;
	}
	else
		DatabaseError("Unknown flag: '%s'", fl) ;
}	

static void POperatorFlag4()
{
	OperatorPt op = AtomToOperator(XTestAtom(X0)) ;
	CharPt fl = XTestAtomName(X1) ;
	Ensure( op != nil ) ;
	if( StrEqual(fl, "parenthesised") ) {
		Pt t = OperatorParenthesised(op) ? tOnAtom : tOffAtom ;
		Ensure( UnifyWithAtomic(X2, t) ) ;
		OperatorParenthesised(op) = XTestOnOff(X3) ;
	}
	else
		DatabaseError("Unknown flag: '%s'", fl) ;
}

void OperatorsInit()
{
	ResetOperators() ;

	InstallCBuiltinPred("op", 3, POperator) ;
	InstallCBuiltinPred("reset_ops", 0, PResetOperators) ;
	InstallNDeterCBuiltinPred("$$_current_op_aux", 1, PNDCurrentOperator) ;
	InstallCBuiltinPred("$$_is_op", 5, PIsOperator) ;
	InstallCBuiltinPred("ops", 0, POperators) ;
	InstallCBuiltinPred("op_flag", 3, POperatorFlag3) ;
	InstallCBuiltinPred("op_flag", 4, POperatorFlag4) ;
}
