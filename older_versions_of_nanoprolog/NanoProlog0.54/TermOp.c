/*
 *   This file is part of the NanoProlog system

 *   TermOp.c
 *   by A.Miguel Dias - 92/2/23
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931117: release of version 0.5

*/

#include "NanoProlog.h"

#define opHashTableSize		128			/* Must be a power of 2 */

#define OperatorHash(pt)	( ( cWord(pt)>>4 ) & (opHashTableSize - 1) )

static OperatorPt opHashTable[opHashTableSize] = { nil } ;

static OperatorPt LookupOperator(AtomPt atom)
{
	register OperatorPt *pt, op ;
	int slot ;

	slot = OperatorHash(atom) ;
	for( pt = opHashTable + slot ; *pt != nil ; pt = &(**pt).next )
		if( (**pt).atom == atom ) return( *pt ) ;

	op = cOperatorPt(HeapAlloc(WordsOf(Operator), true)) ;
	op->next = nil ;
	op->atom = atom ;
	op->prePriority = 0 ;
	op->inPriority = 0 ;
	op->posPriority = 0 ;
	return( *pt = op ) ;
}

static OperatorPt FindOperator(AtomPt atom)
{
	register OperatorPt *pt ;
	int slot ;

	slot = OperatorHash(atom) ;
	for( pt = opHashTable + slot ; *pt != nil ; pt = &(**pt).next )
		if( (**pt).atom == atom ) return( *pt ) ;
	return( nil ) ;
}

OperatorPt FirstOperator()
{
	register int i ;

	dotimes(i, opHashTableSize)
		if( opHashTable[i] != nil )
			return( opHashTable[i] ) ;
}

OperatorPt NextOperator(OperatorPt o)
{
	register int i ;

	if( OperatorNext(o) != nil )
		return( OperatorNext(o) ) ;		
	for( i = OperatorHash(OperatorAtom(o)) + 1 ; i < opHashTableSize ; i++ )
		if( opHashTable[i] != nil )
			return( opHashTable[i] ) ;
	return( nil ) ;
}

int Prefix(AtomPt atom, int *rp)
{
	register OperatorPt op ;

	if( (op = FindOperator(atom)) == nil ) return( 0 ) ;
	if( op->prePriority == 0 ) return( 0 ) ;
	*rp = op->prePriority - 1 + Logic(op->preAssoc) ;
	return( op->prePriority ) ;
}

int Infix(AtomPt atom, int *lp, int *rp)
{
	register OperatorPt op ;

	if( (op = FindOperator(atom)) == nil ) return( 0 ) ;
	if( op->inPriority == 0 ) return( 0 ) ;
	*lp = op->inPriority - 1 + Logic(op->inLAssoc) ;
	*rp = op->inPriority - 1 + Logic(op->inRAssoc) ;
	return( op->inPriority ) ;
}

int Postfix(AtomPt atom, int *lp)
{
	register OperatorPt op ;

	if( (op = FindOperator(atom)) == nil ) return( 0 ) ;
	if( op->posPriority == 0 ) return( 0 ) ;
	*lp = op->posPriority - 1 + Logic(op->posAssoc) ;
	return( op->posPriority ) ;
}

Bool ExclusivelyPrefix(AtomPt atom)
{
	register OperatorPt op ;

	return( (op = FindOperator(atom)) == nil ||
			op->inPriority == 0 &&
			op->posPriority == 0 ) ;
}

static void MakePrefixOperator(char *n, int p, Bool a)
{
	register OperatorPt op = LookupOperator(LookupAtom(n)) ;
	
	op->prePriority = p ;
	op->preAssoc = a ;
}

static void MakeInfixOperator(char *n, int p, Bool la, Bool ra)
{
	register OperatorPt op = LookupOperator(LookupAtom(n)) ;
	
	op->inPriority = p ;
	op->inLAssoc = la ;
	op->inRAssoc = ra ;
}

static void MakePosfixOperator(char *n, int p, Bool a)
{
	register OperatorPt op = LookupOperator(LookupAtom(n)) ;
	
	op->posPriority = p ;
	op->posAssoc = a ;
}

void SetupInitialAtomPriorities()
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
	MakePrefixOperator("gen",		 990, false) ;
	MakePrefixOperator("try",		 980, false) ;
	MakePrefixOperator("once",		 970, false) ;
	MakePrefixOperator("possible",	 970, false) ;
	MakePrefixOperator("side_effects",970, false) ;
	MakePrefixOperator("unit",		 900, false) ;
	MakePrefixOperator("visible",	 900, false) ;
	MakePrefixOperator("import",	 900, false) ;
	MakePrefixOperator("push",		 900, false) ;
	MakePrefixOperator("down",		 900, false) ;
	MakePrefixOperator("set",		 900, false) ;
	MakePrefixOperator("dynamic",	 900, false) ;
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
	MakePrefixOperator("?",			 800, true) ;
	MakePrefixOperator(">",			 700, true) ;
	MakePrefixOperator("<",			 700, true) ;
/* xf */
	MakePosfixOperator("!",			 999, false) ;
	MakePosfixOperator("#",			 999, false) ;
}

Bool DefineOperator(int p, CharPt type, register Pt ops)
{
	register Pt at ;
	CharPt atomName ;

	if( not InRange(p, 1, maxPrec - 1) ) return( false ) ;
	while( ops != tNilAtom )
	{
		if( IsAtom(ops) )
		{
			at = ops ;
			ops = tNilAtom ;
		}
		elif( IsList(ops) )
		{
			at = Drf(XListHead(ops)) ;
			if( not IsAtom(at) ) return( false ) ;
			ops = Drf(XListTail(ops)) ;
		}
		else return( false ) ;
		
		atomName = XAtomName(at) ;
		if( strcmp(type, "xfx") == 0 )
			MakeInfixOperator(atomName, p, false, false) ;
		elif( strcmp(type, "xfy") == 0 )
			MakeInfixOperator(atomName, p, false, true) ;
		elif( strcmp(type, "yfx") == 0 )
			MakeInfixOperator(atomName, p, true, false) ;
		elif( strcmp(type, "yfy") == 0 )
			MakeInfixOperator(atomName, p, true, true) ;
		elif( strcmp(type, "fx") == 0 )
			MakePrefixOperator(atomName, p, false) ;
		elif( strcmp(type, "fy") == 0 )
			MakePrefixOperator(atomName, p, true) ;
		elif( strcmp(type, "xf") == 0 )
			MakePosfixOperator(atomName, p, false) ;
		elif( strcmp(type, "yf") == 0 )
			MakePosfixOperator(atomName, p, true) ;
		else Error("Invalid associativity specification") ;
	}
	return( true ) ;
}
