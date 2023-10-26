/*
 *   This file is part of the CxProlog system

 *   Operator.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

typedef struct Op
{
	struct Op *next ;
	AtomPt atom ;
	struct Unit *unit ;
	short prePriority, inPriority, posPriority ;
	Bool preAssoc, inLAssoc, inRAssoc, posAssoc ;
	Bool isBuiltin ;
} Op, *OpPt ;

#define cOpPt(x)				((OpPt)(x))

#define OpAtom(op)				(op)->atom
#define OpName(op)				AtomName((op)->atom)
#define OpUnit(op)				(op)->unit
#define OpPrePriority(op)		(op)->prefixPriority
#define OpInPriority(op)		(op)->infixPriority
#define OpPosPriority(op)		(op)->posfixPriority
#define OpIsBuiltin(op)			(op)->isBuiltin

static OpPt opList = nil ;

static OpPt NewOp(AtomPt atom)
{
	register OpPt op ;
	if( localOperators_flag && CurrUnit() == builtinUnit )
		if( C == tNilAtom )
			DatabaseError("No current unit when attempting to define new operator '%s'",
															AtomName(atom)) ;
		else
			DatabaseError("Cannot create the new operator '%s' in the builtin unit",
															AtomName(atom)) ;

	op = PermBlockAllocate(WordsOf(Op)) ;
	OpAtom(op) = atom ;
	OpUnit(op) = localOperators_flag ? CurrUnit() : builtinUnit ;
	op->prePriority = 0 ;
	op->inPriority = 0 ;
	op->posPriority = 0 ;
	op->isBuiltin = false ;
	AtomToOp(atom) = op ;
	AtomPermanent(atom) = true ;
	op->next = opList ;
	opList = op ;
	return op ;
}

static OpPt LookupOpByName(CharPt n, Bool change)
{
	AtomPt atom = LookupAtom(n) ;
	OpPt op = AtomToOp(atom) ;
	if( op != nil ) {
		if( localOperators_flag && change && OpIsBuiltin(op) )
			DatabaseError("Attempt to change builtin operator '%s'", OpName(op)) ;
		return op ;
	}
	return NewOp(atom) ;
}

int Prefix(AtomPt atom, int *rp)
{
	register OpPt op ;
	if( (op = AtomToOp(atom)) == nil ) return 0 ;
	if( op->prePriority == 0 ) return 0 ;
	*rp = op->prePriority - 1 + Logic(op->preAssoc) ;
	return op->prePriority ;
}

int Infix(AtomPt atom, int *lp, int *rp)
{
	register OpPt op ;
	if( (op = AtomToOp(atom)) == nil ) return 0 ;
	if( op->inPriority == 0 ) return 0 ;
	*lp = op->inPriority - 1 + Logic(op->inLAssoc) ;
	*rp = op->inPriority - 1 + Logic(op->inRAssoc) ;
	return op->inPriority ;
}

int Postfix(AtomPt atom, int *lp)
{
	register OpPt op ;
	if( (op = AtomToOp(atom)) == nil ) return 0 ;
	if( op->posPriority == 0 ) return 0 ;
	*lp = op->posPriority - 1 + Logic(op->posAssoc) ;
	return op->posPriority ;
}

Bool ExclusivelyPrefix(AtomPt atom)
{
	register OpPt op ;
	return (op = AtomToOp(atom)) == nil ||
			op->inPriority == 0 &&
			op->posPriority == 0 ;
}

static void MakePrefixOp(CharPt n, int p, Bool a)
{
	register OpPt op = LookupOpByName(n, true) ;	
	op->prePriority = p ;
	op->preAssoc = a ;
}

static void MakeInfixOp(CharPt n, int p, Bool la, Bool ra)
{
	register OpPt op = LookupOpByName(n, true) ;
	op->inPriority = p ;
	op->inLAssoc = la ;
	op->inRAssoc = ra ;
}

static void MakePosfixOp(CharPt n, int p, Bool a)
{
	register OpPt op = LookupOpByName(n, true) ;
	op->posPriority = p ;
	op->posAssoc = a ;
}

void ResetOps()
{
/* xfx */
	MakeInfixOp(":-",	 maxPrec, false, false) ;
	MakeInfixOp("-->",	 maxPrec, false, false) ;
	MakeInfixOp("<=",	 maxPrec, false, false) ;
	MakeInfixOp("<->",		1190, false, false) ;
	MakeInfixOp("<-",		1190, false, false) ;
	MakeInfixOp("?:=",	 subPrec, false, false) ;
	MakeInfixOp("#:=",	 subPrec, false, false) ;
	MakeInfixOp(":=",	 subPrec, false, false) ;
	MakeInfixOp("=:",	 subPrec, false, false) ;
	MakeInfixOp("until",	 990, false, false) ;
	MakeInfixOp("unless",	 990, false, false) ;
	MakeInfixOp("from",		 800, false, false) ;
	MakeInfixOp("===",		 700, false, false) ;
	MakeInfixOp("=:=",		 700, false, false) ;
	MakeInfixOp("=\\=",		 700, false, false) ;
	MakeInfixOp("<"	,		 700, false, false) ;
	MakeInfixOp(">=",		 700, false, false) ;
	MakeInfixOp(">"	,		 700, false, false) ;
	MakeInfixOp("=<",		 700, false, false) ;
	MakeInfixOp("is",		 700, false, false) ;
	MakeInfixOp("=..",		 700, false, false) ;
	MakeInfixOp("==",		 700, false, false) ;
	MakeInfixOp("\\==",		 700, false, false) ;
	MakeInfixOp("="	,		 700, false, false) ;
	MakeInfixOp("\\=",		 700, false, false) ;
	MakeInfixOp("@<",		 700, false, false) ;
	MakeInfixOp("@>=",		 700, false, false) ;
	MakeInfixOp("@>",		 700, false, false) ;
	MakeInfixOp("@=<",		 700, false, false) ;
	MakeInfixOp("mod",		 300, false, false) ;
	MakeInfixOp(":" ,		 300, false, false) ;
/* xfy */
	MakeInfixOp(";",	 barPrec, false, true) ;
	MakeInfixOp("->",		1050, false, true) ;
	MakeInfixOp(",",   commaPrec, false, true) ;
	MakeInfixOp(".",		 999, false, true) ;
	MakeInfixOp(">>",		 400, false, true) ;
	MakeInfixOp("<>",		 400, false, true) ;
	MakeInfixOp("^",		 200, false, true) ;
/* yfx */
	MakeInfixOp("+"	,		 500, true, false) ;
	MakeInfixOp("-"	,		 500, true, false) ;
	MakeInfixOp("\\/",		 500, true, false) ;
	MakeInfixOp("/\\",		 500, true, false) ;
	MakeInfixOp("*"	,		 400, true, false) ;
	MakeInfixOp("/"	,		 400, true, false) ;
	MakeInfixOp("div",		 400, true, false) ;
	MakeInfixOp("//",		 400, true, false) ;
	MakeInfixOp("<<",		 400, true, false) ;
/* fx */
	MakePrefixOp(":-",	 maxPrec, false) ;
	MakePrefixOp("?-",	 maxPrec, false) ;
	MakePrefixOp("gen",		 990, false) ;
	MakePrefixOp("try",		 980, false) ;
	MakePrefixOp("once",	 970, false) ;
	MakePrefixOp("possible", 970, false) ;
	MakePrefixOp("unit",	 900, false) ;
	MakePrefixOp("visible",	 900, false) ;
	MakePrefixOp("import",	 900, false) ;
	MakePrefixOp("show",	 900, false) ;
	MakePrefixOp("push",	 900, false) ;
	MakePrefixOp("spy",		 900, false) ;
	MakePrefixOp("nospy",	 900, false) ;
	MakePrefixOp("code",	 900, false) ;
	MakePrefixOp("+",		 500, false) ;
	MakePrefixOp("-",		 500, false) ;
	MakePrefixOp("\\",		 500, false) ;
	MakePrefixOp("@",		  10, false) ;
	MakePrefixOp("@@",		  10, false) ;
/* fy */
	MakePrefixOp("not",		 980, true) ;
	MakePrefixOp("\\+",		 980, true) ;
	MakePrefixOp("down",	 900, true) ;
	MakePrefixOp(">",		 700, true) ;
	MakePrefixOp("<",		 700, true) ;
/* xf */
	MakePosfixOp("!",		 999, false) ;
	MakePosfixOp("#",		 999, false) ;
}

void DefineOp(int p, CharPt type, register Pt ops)
{
	CharPt opName ;
	if( !InRange(p, 1, maxPrec - 1) )
		DatabaseError("Precedence (%d) out of range [1,%d]", p, maxPrec - 1) ;
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
			else DatabaseError("Operator names must be atoms or texts") ;
			ops = Drf(XListTail(ops)) ;
		}
		else DatabaseError("Invalid operator specification") ;
		
		if( EqualStr(type, "xfx") )
			MakeInfixOp(opName, p, false, false) ;
		elif( EqualStr(type, "xfy") )
			MakeInfixOp(opName, p, false, true) ;
		elif( EqualStr(type, "yfx") )
			MakeInfixOp(opName, p, true, false) ;
		elif( EqualStr(type, "fx") )
			MakePrefixOp(opName, p, false) ;
		elif( EqualStr(type, "fy") )
			MakePrefixOp(opName, p, true) ;
		elif( EqualStr(type, "xf") )
			MakePosfixOp(opName, p, false) ;
		elif( EqualStr(type, "yf") )
			MakePosfixOp(opName, p, true) ;
		else DatabaseError("Invalid associativity specification") ;
	}
}


/* CXPROLOG C'BUILTINS */

static void POp()
{
	DefineOp(XTestInt(X0), XTestAtomName(X1), X2) ;
	JumpNext()
}

static void PResetOps()
{
	ResetOps() ;
	JumpNext()
}

static void PNDCurrentOp()
{
	OpPt o = A(1) == tNilAtom
				? opList
				: cOpPt(A(1))->next ;
	A(1) = cPt(o) ;
	if( o == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagAtom(OpAtom(o))) ) JumpNext()
	DoFail()
}

static void PIsOp()
{
	int p, lp, rp ;
	switch( XTestInt(X1) )
	{
		case 0:	lp = p = Prefix(XTestAtom(X0), &rp) ; break ;
		case 1:	p = Infix(XTestAtom(X0), &lp, &rp) ; break ;
		case 2:	rp = p = Postfix(XTestAtom(X0), &lp) ; break ;
		default: DatabaseError("Second argument should be 0, 1 ou 2") ;
	}
	if( p != 0 &&
			UnifyWithNumber(X2, MakeInt(p)) &&
			UnifyWithNumber(X3, MakeInt(lp)) &&
			UnifyWithNumber(X4, MakeInt(rp)) ) JumpNext()
	DoFail()
}

static void POps()
{
	OpPt op ;
	VersionShow() ;
	Write("Ops:\n") ;
	doseq(op, opList, op->next) {
		Write(" %16.16s -> ", OpName(op)) ;
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
	JumpNext()
}

void OpsInit()
{
	ResetOps() ;

	InstallCBuiltinPred("op", 3, POp) ;
	InstallCBuiltinPred("reset_ops", 0, PResetOps) ;
	InstallNDeterCBuiltinPred("$$_current_op_aux", 1, PNDCurrentOp) ;
	InstallCBuiltinPred("$$_is_op", 5, PIsOp) ;
	InstallNDeterCBuiltinPred("ops", 0, POps) ;
}
