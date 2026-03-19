/*
 *   This file is part of the CxProlog system

 *   IVar.c
 *   by A.Miguel Dias - 2000/04/02
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

typedef struct IVar
{
	struct IVar *nextHash ;			/* Next imperative var in the hash chain */
	AtomPt atom ;					/* Variable name */
	Pt value ;						/* Value of imperative variable */
	Bool isConstant ;				/* ivar is a constant */
	Bool isBuiltin ;				/* ivar is builtin, cannot be locally redefined */
} IVar, *IVarPt ;

#define IVarIsBuiltin(iv)			(iv)->isBuiltin
#define IVarIsDefined(iv)			( (iv) != nil && (iv)->value != nil )

#define cIVarPt(p)					((IVarPt)(p))

static HashTablePt ivHashTable ;

static IVarPt IVarNew(AtomPt atom)
{
	register IVarPt iv = PermBlockAllocate(WordsOf(IVar)) ;
	iv->atom = atom ;
	iv->value = tNilAtom ;
	iv->isConstant = false ;
	iv->isBuiltin = false ;
	iv->nextHash = HashTableAdd(ivHashTable, iv) ; /* Must be at end */
	return iv ;
}

static VoidPt getNextFun(VoidPt pt)
{
	return cIVarPt(pt)->nextHash ;
}

static AtomPt getAtomFun(VoidPt pt)
{
	return cIVarPt(pt)->atom ;
}

static int filterFun(VoidPt pt)
{
	return IVarIsDefined(cIVarPt(pt)) ? 1 : 2 ;
}

static IVarPt FirstIVar()
{
	return HashTableFirst(ivHashTable) ;
}

static IVarPt NextIVar(IVarPt iv)
{
	return HashTableNext(ivHashTable, iv) ;
}

static IVarPt FindIVar(AtomPt atom)
{
	AtomPermanent(atom) = true ;
	return HashTableFind(ivHashTable, atom) ;
}

static void IVarsTableInit()
{
	ivHashTable = NewHashTable(32, getNextFun, getAtomFun, filterFun) ;
}

void IVarSet(AtomPt atom, Pt value)
{
	register IVarPt iv = FindIVar(atom) ;
	if( iv == nil ) iv = IVarNew(atom) ;
	if( iv->isConstant )
		Error("Attempt to change constant-ivar '%s'", AtomName(atom)) ;
	if( IVarIsDefined(iv) )
		ReleaseTerm(iv->value) ;
	iv->value = AllocateTermForAssign(value) ;
}
 
void IVarConstSet(AtomPt atom, Pt value)
{
	register IVarPt iv = FindIVar(atom) ;
	if( iv == nil ) iv = IVarNew(atom) ;
	if( iv->isConstant )
		Error("Attempt to change constant-ivar '%s'", AtomName(atom)) ;
	if( IVarIsDefined(iv) )
		ReleaseTerm(iv->value) ;
	iv->value = AllocateTermForAssign(value) ;
	iv->isConstant = true ;
}
 
Pt IVarGet(AtomPt atom)
{
	register IVarPt iv = FindIVar(atom) ;
	if( not IVarIsDefined(iv) )
		Error("Accessing undefined ivar '%s:%s'",
								UnitName(CurrUnit()), AtomName(atom)) ;
	return PushTerm(iv->value) ;
}

AtomPt IVarWith(Pt t)
{
	register IVarPt iv ;
	dolist(iv, FirstIVar(), NextIVar(iv))
		if( IVarIsDefined(iv) && Equal(t, iv->value) )
			return iv->atom ;
	return nil ;
}

void IVarsAtomGCMark()
{
	register IVarPt iv ;
	dolist(iv, FirstIVar(), NextIVar(iv))
		if( IVarIsDefined(iv) )
			TermAtomGCMark(iv->value) ;
}


/* CXPROLOG C'BUILTINS */

static void PIVar()
{
	AtomPt atom = XTestAtom(X0) ;
	register IVarPt iv = FindIVar(atom) ;
	if( IVarIsDefined(iv) ) JumpNext()
	DoFail()
}

static void PIVarDelete()
{
	AtomPt atom = XTestAtom(X0) ;
	register IVarPt iv = FindIVar(atom) ;
	if( iv != nil ) {
		if( IVarIsBuiltin(iv) )
			Error("Attempt to undefine builtin ivar '%s'", AtomName(atom)) ;
		iv->value = nil ;
		iv->isConstant = false ;
	}
	JumpNext()
}

static void PIVarCondSet()
{
	AtomPt atom = XTestAtom(X0) ;
	register IVarPt iv = FindIVar(atom) ;
	if( iv == nil ) {
		iv = IVarNew(atom) ;
		iv->value = AllocateTermForAssign(X1) ;
	}
	JumpNext()
}

static void PIVarConstSet()
{
	IVarConstSet(XTestAtom(X0), X1) ;
	JumpNext()
}

static void PIVarSet()
{
	IVarSet(XTestAtom(X0), X1) ;
	JumpNext()
}

static void PIVarGet()
{
	if( Unify(X1, IVarGet(XTestAtom(X0))) ) JumpNext()
	DoFail()
}

static void PNDCurrentIVar()
{
	register IVarPt iv =
		A(2) == tNilAtom ? FirstIVar() : NextIVar(cIVarPt(A(2))) ;
	A(2) = cPt(iv) ;
	if( iv == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagAtom(iv->atom)) && Unify(X1, PushTerm(iv->value)) )
		JumpNext()
	DoFail()
}

static void PIVars()
{
	register IVarPt iv ;
	VersionShow() ;
	Write("IVars:\n") ;
	dolist(iv, FirstIVar(), NextIVar(iv))
		Write(" %s %16.16s -> %1.50s \n",
						iv->isConstant ? "CONST" : "     ",
						AtomName(iv->atom),
						TermAsStr(iv->value)) ;
	JumpNext()
}

void IVarsInit()
{
	IVarsTableInit() ;

	IVarConstSet(LookupAtom("user_input"), TagExtra(userIn)) ;
	IVarConstSet(LookupAtom("user"), TagExtra(userIn)) ;
	IVarConstSet(LookupAtom("user_output"), TagExtra(userOut)) ;
	IVarConstSet(LookupAtom("user_error"), TagExtra(userErr)) ;

	InstallCBuiltinPred("ivar", 1, PIVar) ;
	InstallCBuiltinPred("ivar_delete", 1, PIVarDelete) ;
	InstallCBuiltinPred("?:=", 2, PIVarCondSet) ;
	InstallCBuiltinPred("#:=", 2, PIVarConstSet) ;
	InstallCBuiltinPred(":=", 2, PIVarSet) ;
	InstallCBuiltinPred("=:", 2, PIVarGet) ;
	InstallNDeterCBuiltinPred("current_ivar", 2, PNDCurrentIVar) ;
	InstallNDeterCBuiltinPred("ivars", 0, PIVars) ;
}
