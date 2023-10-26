/*
 *   This file is part of the CxProlog system

 *   IVar.c
 *   by A.Miguel Dias - 2000/04/02
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

typedef struct IVar {
	struct IVar *next ;		/* Next imperative var in the ivar list */
	AtomPt atom ;			/* Variable name */
	Pt value ;				/* Value of imperative variable */
	Bool isConstant ;		/* ivar is a constant */
	Bool isBuiltin ;		/* ivar is built-in, cannot be locally redefined */
	Bool isRef ;			/* ivar contains a ref or a copy */
} IVar, *IVarPt ;

#define IVarIsConstant(iv)	(iv)->isConstant
#define IVarIsBuiltin(iv)	(iv)->isBuiltin
#define IVarIsRef(iv)		(iv)->isRef

#define cIVarPt(p)			((IVarPt)(p))

static IVarPt ivarList = nil ;
static AtomPt reservedAtom ;
static Pt tUndefinedIVarAtom ;

static IVarPt IVarNew(AtomPt atom)
{
	register IVarPt iv ;
	if( atom == reservedAtom )
		Error("Reserved name. Cannot define ivar '%s'", AtomName(reservedAtom)) ;
	iv = Allocate(WordsOf(IVar), false) ;
	iv->atom = atom ;
	iv->value = tUndefinedIVarAtom ;
	IVarIsConstant(iv) = false ;
	IVarIsBuiltin(iv) = false ;
	IVarIsRef(iv) = false ;
	AtomToIVar(atom) = iv ;
	ExtraPermanent(atom) ;
	iv->next = ivarList ;
	ivarList = iv ;
	return iv ;
}

static IVarPt LookupIVar(AtomPt atom)
{
	return AtomToIVar(atom) == nil ? IVarNew(atom) : AtomToIVar(atom) ;
}

static void IVarChangeValue(AtomPt atom, Pt value, Bool isRef)
{
	register IVarPt iv = LookupIVar(atom) ;
	if( IVarIsConstant(iv) )
		ImperativeError("Attempt to change constant-ivar '%s'",
											AtomName(iv->atom)) ;
	if( IVarIsRef(iv) )
		;
	else
		ReleaseTerm(iv->value) ;
	if( isRef )
		iv->value = value ;
	else
		iv->value = AllocateTermForAssign(value) ;
	IVarIsRef(iv) = isRef ;
}

void IVarSetAtomic(AtomPt atom, Pt atomic)
{	
	IVarSetRef(atom, TestAtomic(atomic)) ;
}

void IVarSetCopy(AtomPt atom, Pt value)
{
	IVarChangeValue(atom, value, false) ;
}

void IVarSetRef(AtomPt atom, Pt value)
{
	IVarChangeValue(atom, value, true) ;
}

static Bool IVarIsDefined(IVarPt iv)
{
	return iv != nil && iv->value != tUndefinedIVarAtom ;
}

static void IVarNowConstant(AtomPt atom)
{
	register IVarPt iv = LookupIVar(atom) ;
	if( IVarIsDefined(iv) )
		IVarIsConstant(iv) = true ;
	else
		ImperativeError("Attempt to make constant the undefined ivar '%s'",
															AtomName(atom)) ;
}

static void IVarCondSet(AtomPt atom, Pt value)
{
	if( !IVarIsDefined(LookupIVar(atom)) )
		IVarSetCopy(atom, value) ;
}

static void IVarReversibleSet(AtomPt atom, Pt value)
{
	Pt old = LookupIVar(atom)->value ;
	IVarSetCopy(atom, value) ;
/* Push a trailed ivar, saving old value, which is not deleted */
	TrailIVar(atom, old) ;
}
void IVarReversibleRestore(AtomPt atom, Pt oldValue) /* Called from Machine.c */
{
	IVarSetCopy(atom, oldValue) ;
}

void IVarDelete(AtomPt atom)
{
	register IVarPt iv = AtomToIVar(atom) ;
	if( IVarIsDefined(iv) ) {
		if( iv->isBuiltin )
			ImperativeError("Attempt to delete built-in ivar '%s'",
										AtomName(atom)) ;
		IVarIsConstant(iv) = false ;
		IVarSetAtomic(atom, tUndefinedIVarAtom) ;
	}
}

Pt IVarGet(AtomPt atom)
{
	register IVarPt iv = AtomToIVar(atom) ;
	if( IVarIsDefined(iv) )
		return iv->value ;
	else return nil ;
}

static AtomPt IVarWith(Pt t)
{
	register IVarPt iv ;
	doseq(iv, ivarList, iv->next)
		if( Identical(t, iv->value) )
			return iv->atom ;
	return nil ;
}

void IVarsWithWrite(Pt t)
{
	if( IVarWith(t) != nil ) {
		register IVarPt iv ;
		doseq(iv, ivarList, iv->next)
			if( Identical(t, iv->value) )
				Write(", ivar(%s)", AtomName(iv->atom)) ;
	}
}

static void IVarsBasicGCMark(void)
{
	register IVarPt iv ;
	doseq(iv, ivarList, iv->next)
		TermBasicGCMark(iv->value) ;
}


/* CXPROLOG C'BUILTINS */

static void PIVar(void)
{
	MustBe( IVarIsDefined(AtomToIVar(XTestAtom(X0))) ) ;
}

static void PIVarDelete(void)
{
	IVarDelete(XTestAtom(X0)) ;
	JumpNext() ;
}

static void PIVarSetCopy(void)
{
	IVarSetCopy(XTestAtom(X0), X1) ;
	JumpNext() ;
}

static void PIVarSetRef(void)
{
	IVarSetRef(XTestAtom(X0), X1) ;
	JumpNext() ;
}

static void PIVarConstSet(void)
{
	AtomPt a = XTestAtom(X0);
	IVarSetCopy(a, X1) ;
	IVarNowConstant(a) ;
	JumpNext() ;
}

static void PIVarCondSet(void)
{
	IVarCondSet(XTestAtom(X0), X1) ;
	JumpNext() ;
}

static void PIVarReversibleSet(void)
{
	IVarReversibleSet(XTestAtom(X0), X1) ;
	JumpNext() ;
}

static void PIVarGet(void)
{
	Pt t = IVarGet(XTestAtom(X0)) ;
	Ensure( t != nil ) ;
	t = ZPushTerm(t) ; /* stacks may grow */
	MustBe( Unify(X1, t) ) ;
}

static void PIVarGetU(void)
{
	Pt t = IVarGet(XTestAtom(X0)) ;
	if( t == nil )
		t = tUndefinedIVarAtom ;
	t = ZPushTerm(t) ; /* stacks may grow */
	MustBe( Unify(X1, t) ) ;
}

static void PNDCurrentIVar(void)
{
	Pt t ;
	register IVarPt iv = A(2) == tNilAtom
				? ivarList
				: cIVarPt(A(2))->next ;
	doseq(iv, iv, iv->next)
		if( IVarIsDefined(iv) ) break ;
	A(2) = cPt(iv) ;
	if( iv == nil ) Jump(DiscardAndFail) ;
	Ensure( UnifyWithAtomic(X0, TagAtom(iv->atom)) ) ;
	t = ZPushTerm(iv->value) ; /* stacks may grow */
	MustBe( Unify(X1, t) ) ;

}

static void PIVars(void)
{
	register IVarPt iv ;
	ShowVersion() ;
	Write("IVARS:\n") ;
	doseq(iv, ivarList, iv->next)
		if( IVarIsDefined(iv) )
			Write(" %s %16.16s -> %1.50s\n",
						IVarIsConstant(iv) ? "CONST" : "     ",
						AtomName(iv->atom),
						TermAsStr(iv->value)) ;
	JumpNext() ;
}

void IVarsInit(void)
{
	reservedAtom = XAtom(tUserAtom) ;
	tUndefinedIVarAtom = MakeAtom("$$_undefined_ivar") ;
	ExtraGCHandlerInstall("IVAR", IVarsBasicGCMark) ;
	/* add "ivars." to CxProlog.c/PShow */

	InstallCBuiltinPred("ivar", 1, PIVar) ;
	InstallCBuiltinPred("ivar_delete", 1, PIVarDelete) ;
	InstallCBuiltinPred("?:=", 2, PIVarCondSet) ;
	InstallCBuiltinPred("&:=", 2, PIVarReversibleSet) ;
	InstallCBuiltinPred("#:=", 2, PIVarConstSet) ;
	InstallCBuiltinPred(":=", 2, PIVarSetCopy) ;
	InstallCBuiltinPred("::=", 2, PIVarSetRef) ;
	InstallCBuiltinPred("=:", 2, PIVarGet) ;
	InstallCBuiltinPred("$$_ivar_get", 2, PIVarGetU) ;

	InstallNDeterCBuiltinPred("current_ivar", 2, PNDCurrentIVar) ;
	InstallCBuiltinPred("ivars", 0, PIVars) ;
}
