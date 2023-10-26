/*
 *   This file is part of the CxProlog system

 *   Operator.c
 *   by A.Miguel Dias - 1992/02/23
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

static HashTablePt opHashTable ;
Bool barOpDefined = false ;

static Bool localOperators = false ;

static OperatorPt NewOperator(AtomPt atom)
{
	register OperatorPt op ;

	if( localOperators && UnitIsReserved(CurrUnit()) )
		if( C == tNilAtom )
			Error("No current unit when attempting to define new operator '%s'",
															AtomName(atom)) ;
		else
			Error("Cannot create the new operator '%s' in the builtin unit",
															AtomName(atom)) ;

	op = cOperatorPt(SpaceAlloc(WordsOf(Operator), true)) ;
	OperatorAtom(op) = atom ;
	OperatorUnit(op) = localOperators ? CurrUnit() : builtinUnit ;
	op->prePriority = 0 ;
	op->inPriority = 0 ;
	op->posPriority = 0 ;
	op->isBuiltin = false ;
	op->nextHash = HashTableAdd(opHashTable, op) ; /* Must be at end */
	return op ;
}

static VoidPt getNextFun(VoidPt pt)
{
	return cOperatorPt(pt)->nextHash ;
}

static AtomPt getAtomFun(VoidPt pt)
{
	return OperatorAtom(cOperatorPt(pt)) ;
}

static int filterFun(VoidPt pt)
{
	if( cOperatorPt(pt)->unit == CurrUnit() )
		return 1 ;
	elif( cOperatorPt(pt)->unit == builtinUnit )
		return 2 ;
	else return 0 ;
}

void InitOperators()
{
	opHashTable = NewHashTable(64, getNextFun, getAtomFun, filterFun) ;
	ResetOperators() ;
}

static OperatorPt FindOperator(AtomPt atom)
{
	return HashTableFind(opHashTable, atom) ;
}

static OperatorPt LookupOperator(AtomPt atom, Bool change)
{
	OperatorPt op ;
	
	if( (op = FindOperator(atom)) != nil ) {
		if( localOperators && change && OperatorIsBuiltin(op) )
			Error("Attempt to change builtin operator '%s'", OperatorName(op)) ;
		return op ;
	}
	return NewOperator(atom) ;
}

void SetLocalOperatorsFlag(Bool b)
{
	localOperators = b ;
}

Bool GetLocalOperatorsFlag()
{
	return localOperators ;
}

OperatorPt FirstOperator()
{
	return HashTableFirst(opHashTable) ;
}

OperatorPt NextOperator(OperatorPt op)
{
	return HashTableNext(opHashTable, op) ;
}

int Prefix(AtomPt atom, int *rp)
{
	register OperatorPt op ;

	if( (op = FindOperator(atom)) == nil ) return 0 ;
	if( op->prePriority == 0 ) return 0 ;
	*rp = op->prePriority - 1 + Logic(op->preAssoc) ;
	return op->prePriority ;
}

int Infix(AtomPt atom, int *lp, int *rp)
{
	register OperatorPt op ;

	if( (op = FindOperator(atom)) == nil ) return 0 ;
	if( op->inPriority == 0 ) return 0 ;
	*lp = op->inPriority - 1 + Logic(op->inLAssoc) ;
	*rp = op->inPriority - 1 + Logic(op->inRAssoc) ;
	return op->inPriority ;
}

int Postfix(AtomPt atom, int *lp)
{
	register OperatorPt op ;

	if( (op = FindOperator(atom)) == nil ) return 0 ;
	if( op->posPriority == 0 ) return 0 ;
	*lp = op->posPriority - 1 + Logic(op->posAssoc) ;
	return op->posPriority ;
}

Bool ExclusivelyPrefix(AtomPt atom)
{
	register OperatorPt op ;

	return (op = FindOperator(atom)) == nil ||
			op->inPriority == 0 &&
			op->posPriority == 0 ;
}

static void MakePrefixOperator(CharPt n, int p, Bool a)
{
	register OperatorPt op = LookupOperator(LookupAtom(n), true) ;
	
	op->prePriority = p ;
	op->preAssoc = a ;
}

static void MakeInfixOperator(CharPt n, int p, Bool la, Bool ra)
{
	register OperatorPt op = LookupOperator(LookupAtom(n), true) ;
	
	op->inPriority = p ;
	op->inLAssoc = la ;
	op->inRAssoc = ra ;
	if( n[0] == '|' && n[1] == '\0' )
		barOpDefined = true ;
}

static void MakePosfixOperator(CharPt n, int p, Bool a)
{
	register OperatorPt op = LookupOperator(LookupAtom(n), true) ;
	
	op->posPriority = p ;
	op->posAssoc = a ;
}

void ResetOperators()
{
/* xfx */
	MakeInfixOperator(":-",		 maxPrec, false, false) ;
	MakeInfixOperator("-->",	 maxPrec, false, false) ;
	MakeInfixOperator("<=",		 maxPrec, false, false) ;
	MakeInfixOperator("<->",		1190, false, false) ;
	MakeInfixOperator("<-",			1190, false, false) ;
	MakeInfixOperator("until",		 990, false, false) ;
	MakeInfixOperator("unless",		 990, false, false) ;
	MakeInfixOperator("from",		 800, false, false) ;
	MakeInfixOperator("=:=",		 700, false, false) ;
	MakeInfixOperator("=\\=",		 700, false, false) ;
	MakeInfixOperator("<"	,		 700, false, false) ;
	MakeInfixOperator(">=",			 700, false, false) ;
	MakeInfixOperator(">"	,		 700, false, false) ;
	MakeInfixOperator("=<",			 700, false, false) ;
	MakeInfixOperator("is",			 700, false, false) ;
	MakeInfixOperator("=..",		 700, false, false) ;
	MakeInfixOperator("==",			 700, false, false) ;
	MakeInfixOperator("\\==",		 700, false, false) ;
	MakeInfixOperator("="	,		 700, false, false) ;
	MakeInfixOperator("\\=",		 700, false, false) ;
	MakeInfixOperator("@<",			 700, false, false) ;
	MakeInfixOperator("@>=",		 700, false, false) ;
	MakeInfixOperator("@>",			 700, false, false) ;
	MakeInfixOperator("@=<",		 700, false, false) ;
	MakeInfixOperator("mod",		 300, false, false) ;
	MakeInfixOperator(":" ,			 300, false, false) ;
/* xfy */
	MakeInfixOperator(";",		 barPrec, false, true) ;
	MakeInfixOperator("->",			1050, false, true) ;
	MakeInfixOperator(",",	   commaPrec, false, true) ;
	MakeInfixOperator(".",			 999, false, true) ;
	MakeInfixOperator(">>",			 400, false, true) ;
	MakeInfixOperator("<>",			 400, false, true) ;
	MakeInfixOperator("^",			 200, false, true) ;
/* yfx */
	MakeInfixOperator("+"	,		 500, true, false) ;
	MakeInfixOperator("-"	,		 500, true, false) ;
	MakeInfixOperator("\\/",		 500, true, false) ;
	MakeInfixOperator("/\\",		 500, true, false) ;
	MakeInfixOperator("*"	,		 400, true, false) ;
	MakeInfixOperator("/"	,		 400, true, false) ;
	MakeInfixOperator("div",		 400, true, false) ;
	MakeInfixOperator("//",			 400, true, false) ;
	MakeInfixOperator("<<",			 400, true, false) ;
/* fx */
	MakePrefixOperator(":-",	 maxPrec, false) ;
	MakePrefixOperator("?-",	 maxPrec, false) ;
	MakePrefixOperator("unit",		 900, false) ;
	MakePrefixOperator("visible",	 900, false) ;
	MakePrefixOperator("import",	 900, false) ;
	MakePrefixOperator("ivar",		 900, false) ;
	MakePrefixOperator("push",		 900, false) ;
	MakePrefixOperator("+",			 500, false) ;
	MakePrefixOperator("-",			 500, false) ;
	MakePrefixOperator("\\",		 500, false) ;
	MakePrefixOperator("@",			  10, false) ;
	MakePrefixOperator("@@",		  10, false) ;
/* fy */
	MakePrefixOperator("not",		 980, true) ;
	MakePrefixOperator("\\+",		 980, true) ;
	MakePrefixOperator("spy",		 900, true) ;
	MakePrefixOperator("nospy",		 900, true) ;
/* xf */
	MakePosfixOperator("!",			 999, false) ;
	MakePosfixOperator("#",			 999, false) ;
}

void DefineOperator(int p, CharPt type, register Pt ops)
{
	register Pt at ;
	CharPt atomName ;

	if( not InRange(p, 1, maxPrec - 1) )
		Error("Precedence (%d) out of range [1,%d]", p, maxPrec - 1) ;
	ops = Drf(ops) ;
	while( ops != tNilAtom ) {
		if( IsAtom(ops) ) {
			at = ops ;
			ops = tNilAtom ;
		}
		elif( IsList(ops) ) {
			at = Drf(XListHead(ops)) ;
			if( not IsAtom(at) )
				Error("Operator names must be atoms") ;
			ops = Drf(XListTail(ops)) ;
		}
		else Error("Invalid operator specification") ;
		
		atomName = XAtomName(at) ;
		if( EqualStr(type, "xfx") )
			MakeInfixOperator(atomName, p, false, false) ;
		elif( EqualStr(type, "xfy") )
			MakeInfixOperator(atomName, p, false, true) ;
		elif( EqualStr(type, "yfx") )
			MakeInfixOperator(atomName, p, true, false) ;
		elif( EqualStr(type, "fx") )
			MakePrefixOperator(atomName, p, false) ;
		elif( EqualStr(type, "fy") )
			MakePrefixOperator(atomName, p, true) ;
		elif( EqualStr(type, "xf") )
			MakePosfixOperator(atomName, p, false) ;
		elif( EqualStr(type, "yf") )
			MakePosfixOperator(atomName, p, true) ;
		else Error("Invalid associativity specification") ;
	}
}
