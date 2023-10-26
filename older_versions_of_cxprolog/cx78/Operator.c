/*
 *   This file is part of the CxProlog system

 *   Operator.c
 *   by A.Miguel Dias - 1992/02/23
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

static HashTablePt opHashTable ;
Bool barOpDefined = false ;

static OpPt NewOp(AtomPt atom)
{
	register OpPt op ;
	if( localOperators_flag && UnitIsReserved(CurrUnit()) )
		if( C == tNilAtom )
			Error("No current unit when attempting to define new operator '%s'",
															AtomName(atom)) ;
		else
			Error("Cannot create the new operator '%s' in the builtin unit",
															AtomName(atom)) ;

	op = PermBlockAllocate(WordsOf(Op)) ;
	OpAtom(op) = atom ;
	OpUnit(op) = localOperators_flag ? CurrUnit() : builtinUnit ;
	op->prePriority = 0 ;
	op->inPriority = 0 ;
	op->posPriority = 0 ;
	op->isBuiltin = false ;
	op->nextHash = HashTableAdd(opHashTable, op) ; /* Must be at end */
	return op ;
}

static VoidPt getNextFun(VoidPt pt)
{
	return cOpPt(pt)->nextHash ;
}

static AtomPt getAtomFun(VoidPt pt)
{
	return OpAtom(cOpPt(pt)) ;
}

static int filterFun(VoidPt pt)
{
	return 1 ;
	if( cOpPt(pt)->unit == CurrUnit() )
		return 1 ;
	elif( cOpPt(pt)->unit == builtinUnit )
		return 2 ;
	else return 0 ;
}

static void OpTableInit()
{
	opHashTable = NewHashTable(64, getNextFun, getAtomFun, filterFun) ;
}

static OpPt FindOp(AtomPt atom)
{
	return HashTableFind(opHashTable, atom) ;
}

static OpPt LookupOpByName(CharPt n, Bool change)
{
	AtomPt atom = LookupAtom(n) ;
	OpPt op = FindOp(atom) ;
	if( op != nil ) {
		if( localOperators_flag && change && OpIsBuiltin(op) )
			Error("Attempt to change builtin operator '%s'", OpName(op)) ;
		return op ;
	}
	return NewOp(atom) ;
}

OpPt FirstOp()
{
	return HashTableFirst(opHashTable) ;
}

OpPt NextOp(OpPt op)
{
	return HashTableNext(opHashTable, op) ;
}

int Prefix(AtomPt atom, int *rp)
{
	register OpPt op ;
	if( (op = FindOp(atom)) == nil ) return 0 ;
	if( op->prePriority == 0 ) return 0 ;
	*rp = op->prePriority - 1 + Logic(op->preAssoc) ;
	return op->prePriority ;
}

int Infix(AtomPt atom, int *lp, int *rp)
{
	register OpPt op ;
	if( (op = FindOp(atom)) == nil ) return 0 ;
	if( op->inPriority == 0 ) return 0 ;
	*lp = op->inPriority - 1 + Logic(op->inLAssoc) ;
	*rp = op->inPriority - 1 + Logic(op->inRAssoc) ;
	return op->inPriority ;
}

int Postfix(AtomPt atom, int *lp)
{
	register OpPt op ;
	if( (op = FindOp(atom)) == nil ) return 0 ;
	if( op->posPriority == 0 ) return 0 ;
	*lp = op->posPriority - 1 + Logic(op->posAssoc) ;
	return op->posPriority ;
}

Bool ExclusivelyPrefix(AtomPt atom)
{
	register OpPt op ;
	return (op = FindOp(atom)) == nil ||
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
	if( n[0] == '|' && n[1] == '\0' )
		barOpDefined = true ;
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
	MakePrefixOp("unit",	 900, false) ;
	MakePrefixOp("visible",	 900, false) ;
	MakePrefixOp("import",	 900, false) ;
	MakePrefixOp("show",	 900, false) ;
	MakePrefixOp("push",	 900, false) ;
	MakePrefixOp("+",		 500, false) ;
	MakePrefixOp("-",		 500, false) ;
	MakePrefixOp("\\",		 500, false) ;
	MakePrefixOp("@",		  10, false) ;
	MakePrefixOp("@@",		  10, false) ;
/* fy */
	MakePrefixOp("not",		 980, true) ;
	MakePrefixOp("\\+",		 980, true) ;
	MakePrefixOp("spy",		 900, true) ;
	MakePrefixOp("nospy",	 900, true) ;
	MakePrefixOp(">",		 700, true) ;
	MakePrefixOp("<",		 700, true) ;
/* xf */
	MakePosfixOp("!",		 999, false) ;
	MakePosfixOp("#",		 999, false) ;
}

void DefineOp(int p, CharPt type, register Pt ops)
{
	CharPt opName ;
	if( not InRange(p, 1, maxPrec - 1) )
		Error("Precedence (%d) out of range [1,%d]", p, maxPrec - 1) ;
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
			else Error("Operator names must be atoms or texts") ;
			ops = Drf(XListTail(ops)) ;
		}
		else Error("Invalid operator specification") ;
		
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
		else Error("Invalid associativity specification") ;
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
	OpPt o ;
	o = A(1) == tNilAtom ? FirstOp() : NextOp(cOpPt(A(1))) ;
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
		default: Error("Second argument should be 0, 1 ou 2") ;
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
	dolist(op, FirstOp(), NextOp(op)) {
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
	OpTableInit() ;
	ResetOps() ;

	InstallCBuiltinPred("op", 3, POp) ;
	InstallCBuiltinPred("reset_ops", 0, PResetOps) ;
	InstallNDeterCBuiltinPred("$$_current_op_aux", 1, PNDCurrentOp) ;
	InstallCBuiltinPred("$$_is_op", 5, PIsOp) ;
	InstallNDeterCBuiltinPred("ops", 0, POps) ;
}
