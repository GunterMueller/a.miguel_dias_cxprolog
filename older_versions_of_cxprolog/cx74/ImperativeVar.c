/*
 *   This file is part of the CxProlog system

 *   ImperativeVar.c
 *   by A.Miguel Dias - 2000/04/02
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

static HashTablePt ivHashTable ;

static ImperativeVarPt NewImperativeVar(AtomPt atom)
{
	ImperativeVarPt iv ;
	
	if( UnitIsReserved(CurrUnit()) )
		if( C == tNilAtom )
			Error("No current unit when attempting to create new \
imperative variable '%s'", AtomName(atom)) ;
		else
			Error("Cannot create the imperative variable '%s' in \
the builtin unit", AtomName(atom)) ;
	iv = PermanentAllocate(WordsOf(ImperativeVar)) ;
	iv->atom = atom ;
	iv->value = tNilAtom ;
	iv->unit = CurrUnit() ;
	iv->isBuiltin = false ;
	iv->nextHash = HashTableAdd(ivHashTable, iv) ; /* Must be at end */
	return iv ;
}

static VoidPt getNextFun(VoidPt pt)
{
	return cImperativeVarPt(pt)->nextHash ;
}

static AtomPt getAtomFun(VoidPt pt)
{
	return cImperativeVarPt(pt)->atom ;
}

static int filterFun(VoidPt pt)
{
	return cImperativeVarPt(pt)->value != nil ? 1 : 2 ;
	if( cImperativeVarPt(pt)->unit == CurrUnit() )
		return 1 ;
	elif( cImperativeVarPt(pt)->unit == builtinUnit )
		return 2 ;
	else return 0 ;
}

static void InitImperativeVarsTable()
{
	ivHashTable = NewHashTable(32, getNextFun, getAtomFun, filterFun) ;
}

void DefineImperativeVar(AtomPt atom)
{
	ImperativeVarPt iv ;
	
	if( (iv = HashTableFind(ivHashTable, atom)) != nil ) {
		if( IVarIsBuiltin(iv) )
			Error("Attempt to redefine builtin ivar '%s'", AtomName(atom)) ;
		if( iv->value == nil )
			iv->value = tNilAtom ;
	}
	else NewImperativeVar(atom) ;
}
 
Bool IsDefinedImperativeVar(AtomPt atom)
{
	ImperativeVarPt iv ;
	
	return (iv = HashTableFind(ivHashTable, atom)) != nil && iv->value != nil ;

}
 
void UndefineImperativeVar(AtomPt atom)
{
	ImperativeVarPt iv ;
	
	if( (iv = HashTableFind(ivHashTable, atom)) != nil ) {
		if( IVarIsBuiltin(iv) )
			Error("Attempt to undefine builtin ivar '%s'", AtomName(atom)) ;
		iv->value = nil ;
	}
}

void ImperativeVarSet(AtomPt atom, Pt value)
{
	ImperativeVarPt iv = HashTableFind(ivHashTable, atom) ;
	if( iv == nil ) {
#if 0
		Error("Accessing undefined variable '%s:%s'",
								UnitName(CurrUnit()), AtomName(atom)) ;
#else
		iv = NewImperativeVar(atom) ;
#endif
	}
	if( iv->value != nil )
		ReleaseTerm(iv->value) ;
	iv->value = AllocateTermForAssign(value) ;
}
 
Pt ImperativeVarGet(AtomPt atom)
{
	ImperativeVarPt iv = HashTableFind(ivHashTable, atom) ;
	if( iv == nil || iv->value == nil )
		Error("Accessing undefined variable '%s:%s'",
								UnitName(CurrUnit()), AtomName(atom)) ;
	return PushTerm(iv->value) ;
}

ImperativeVarPt FirstImperativeVar()
{
	return HashTableFirst(ivHashTable) ;
}

ImperativeVarPt NextImperativeVar(ImperativeVarPt iv)
{
	return HashTableNext(ivHashTable, iv) ;
}


/* CXPROLOG C'BUILTINS */

static void PCreateImperativeVar()
{
	DefineImperativeVar(XTestAtom(X0)) ;
	JumpNext()
}

static void PDefinedVar()
{
	if( IsDefinedImperativeVar(XTestAtom(X0)) ) JumpNext()
	DoFail()
}

static void PUndefineVar()
{
	UndefineImperativeVar(XTestAtom(X0)) ;
	JumpNext()
}

static void PImperativeVarSet()
{
	ImperativeVarSet(XTestAtom(X0), X1) ;
	JumpNext()
}

static void PImperativeVarGet()
{
	if( Unify(X1, ImperativeVarGet(XTestAtom(X0))) ) JumpNext()
	DoFail()
}

static void PNDCurrentImperativeVar()
{
	ImperativeVarPt iv =
		A(1) == tNilAtom ? FirstImperativeVar() : NextImperativeVar(cImperativeVarPt(A(1))) ;
	A(1) = cPt(iv) ;
	if( iv == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagAtom(iv->atom)) ) JumpNext()
	DoFail()
}

void InitImperativeVars()
{
	InitImperativeVarsTable() ;
	InstallCBuiltinPred("create_ivar", 1, PCreateImperativeVar) ;
	InstallCBuiltinPred("defined_ivar", 1, PDefinedVar) ;
	InstallCBuiltinPred("undefine_ivar", 1, PUndefineVar) ;
	InstallCBuiltinPred(":=", 2, PImperativeVarSet) ;
	InstallCBuiltinPred("=:", 2, PImperativeVarGet) ;
	InstallNDeterCBuiltinPred("current_ivar", 1, PNDCurrentImperativeVar) ;
}
