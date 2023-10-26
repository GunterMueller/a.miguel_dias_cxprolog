/*
 *   This file is part of the CxProlog system

 *   IVar.c
 *   by A.Miguel Dias - 2000/04/02
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

typedef struct IVar
{
	struct IVar *next ;				/* Next imperative var in the ivar list */
	AtomPt atom ;					/* Variable name */
	Pt value ;						/* Value of imperative variable */
	Bool isConstant ;				/* ivar is a constant */
	Bool isBuiltin ;				/* ivar is builtin, cannot be locally redefined */
} IVar, *IVarPt ;

#define IVarIsBuiltin(iv)			(iv)->isBuiltin
#define IVarIsDefined(iv)			( (iv) != nil && (iv)->value != nil )

#define cIVarPt(p)					((IVarPt)(p))

static IVarPt ivarList = nil ;
static AtomPt reservedAtom ;

static IVarPt IVarNew(AtomPt atom)
{
	register IVarPt iv ;
	if( atom == reservedAtom )
		Error("Reserved name. Cannot define ivar '%s'", AtomName(reservedAtom)) ;
	iv = PermBlockAllocate(WordsOf(IVar)) ;
	iv->atom = atom ;
	iv->value = tNilAtom ;
	iv->isConstant = false ;
	iv->isBuiltin = false ;
	AtomToIVar(atom) = iv ;
	AtomPermanent(atom) = true ;
	iv->next = ivarList ;
	ivarList = iv ;
	return iv ;
}

static IVarPt LookupIVar(AtomPt atom)
{
	if( AtomToIVar(atom) != nil )
		return AtomToIVar(atom) ;
	else IVarNew(atom) ;
}

void IVarSet(AtomPt atom, Pt value)
{
	register IVarPt iv = LookupIVar(atom) ;
	if( iv->isConstant )
		ImperativeError("Attempt to change constant-ivar '%s'",
													AtomName(atom)) ;
	if( IVarIsDefined(iv) )
		ReleaseTerm(iv->value) ;
	iv->value = AllocateTermForAssign(value) ;
}
 
void IVarConstSet(AtomPt atom, Pt value)
{
	register IVarPt iv = LookupIVar(atom) ;
	if( iv->isConstant )
		ImperativeError("Attempt to change constant-ivar '%s'",
												AtomName(atom)) ;
	if( IVarIsDefined(iv) )
		ReleaseTerm(iv->value) ;
	iv->value = AllocateTermForAssign(value) ;
	iv->isConstant = true ;
}
 
void IVarForceSet(AtomPt atom, Pt value, Bool cons)
{
	register IVarPt iv = LookupIVar(atom) ;
	if( IVarIsDefined(iv) )
		ReleaseTerm(iv->value) ;
	iv->value = AllocateTermForAssign(value) ;
	iv->isConstant = cons ;
}
 
Pt IVarGet(AtomPt atom)
{
	register IVarPt iv = AtomToIVar(atom) ;
	if( !IVarIsDefined(iv) )
		ImperativeError("Accessing undefined ivar '%s:%s'",
								UnitName(CurrUnit()), AtomName(atom)) ;
	return iv->value ;
}

static Pt IVarGetNoErr(AtomPt atom)
{
	register IVarPt iv = AtomToIVar(atom) ;
	if( !IVarIsDefined(iv) )
		return nil ;
	return iv->value ;
}

AtomPt IVarWith(Pt t)
{
	register IVarPt iv ;
	doseq(iv, ivarList, iv->next)
		if( IVarIsDefined(iv) && Equal(t, iv->value) )
			return iv->atom ;
	return nil ;
}

static void IVarsAtomGCMark()
{
	register IVarPt iv ;
	doseq(iv, ivarList, iv->next)
		if( IVarIsDefined(iv) )
			TermAtomGCMark(iv->value) ; /* too much ??? @@@ */
}


/* CXPROLOG C'BUILTINS */

static void PIVar()
{
	AtomPt atom = XTestAtom(X0) ;
	register IVarPt iv = AtomToIVar(atom) ;
	if( IVarIsDefined(iv) ) JumpNext()
	DoFail()
}

static void PIVarDelete()
{
	AtomPt atom = XTestAtom(X0) ;
	register IVarPt iv = AtomToIVar(atom) ;
	if( iv != nil ) {
		if( IVarIsBuiltin(iv) )
			ImperativeError("Attempt to undefine builtin ivar '%s'",
										AtomName(atom)) ;
		iv->value = nil ;
		iv->isConstant = false ;
	}
	JumpNext()
}

static void PIVarCondSet()
{
	AtomPt atom = XTestAtom(X0) ;
	register IVarPt iv = AtomToIVar(atom) ;
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
	Pt t ;
	if( (t = IVarGetNoErr(XTestAtom(X0))) != nil ) {
		t = ZPushTerm(t) ; /* stacks may grow */
		if( Unify(X1, t) ) JumpNext()
	}
	DoFail()
}

static void PNDCurrentIVar()
{
	IVarPt iv = A(2) == tNilAtom
				? ivarList
				: cIVarPt(A(2))->next ;
	A(2) = cPt(iv) ;
	if( iv == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagAtom(iv->atom)) ) {
		Pt t = ZPushTerm(iv->value) ; /* stacks may grow */
		if( Unify(X1, t) ) JumpNext()
	}
	DoFail()
}

static void PIVars()
{
	register IVarPt iv ;
	VersionShow() ;
	Write("IVars:\n") ;
	doseq(iv, ivarList, iv->next)
		Write(" %s %16.16s -> %1.50s \n",
						iv->isConstant ? "CONST" : "     ",
						AtomName(iv->atom),
						TermAsStr(iv->value)) ;
	JumpNext()
}

void IVarsInit()
{
	reservedAtom = XAtom(tUserAtom) ;
	InstallAtomGCHandler(IVarsAtomGCMark) ;
	/* add "ivars." to CxProlog.c/PShow */

	InstallCBuiltinPred("ivar", 1, PIVar) ;
	InstallCBuiltinPred("ivar_delete", 1, PIVarDelete) ;
	InstallCBuiltinPred("?:=", 2, PIVarCondSet) ;
	InstallCBuiltinPred("#:=", 2, PIVarConstSet) ;
	InstallCBuiltinPred(":=", 2, PIVarSet) ;
	InstallCBuiltinPred("=:", 2, PIVarGet) ;
	InstallNDeterCBuiltinPred("current_ivar", 2, PNDCurrentIVar) ;
	InstallNDeterCBuiltinPred("ivars", 0, PIVars) ;
}
