/*
 *   This file is part of the CxProlog system

 *   Unify.c
 *   by A.Miguel Dias - 1989/11/14
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

Bool UnifyWithNumber(register Pt t, Pt numb)
{
	VarValue(t) ;
	if( t == numb ) return true ;
	if( IsVar(t) ) {
		Assign(t, numb) ;
		return true ;
	}	
	return false ;
}

Bool UnifyWithAtomic(register Pt t, Pt at)
{
	VarValue(t) ;
	if( t == at ) return true ;
	if( IsVar(t) ) {
		Assign(t, at) ;
		return true ;
	}

	if( IsText(t) )
		if( IsAtomOrText(at) )
			return EqualStr(XAtomOrTextName(t), XAtomOrTextName(at)) ;
		else return false ;

	if( IsAtom(t) )
		if( IsText(at) )
			return EqualStr(XAtomOrTextName(t), XAtomOrTextName(at)) ;
		else return false ;

	return false ;
}

Bool Unify(register Pt t1, register Pt t2)
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return true ;
	
	if( IsVar(t1) ) {
		if( IsVar(t2) ) Bind(t1, t2) ;
		else Assign(t1, t2) ;
		return true ;
	}
	
	if( IsVar(t2) ) {
		Assign(t2, t1) ;
		return true ;
	}

	if( IsStruct(t1) ) {
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) ) {
			register n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
			do {
				if( not Unify(*arg1++, *arg2++) ) return false ;
			} while( --n ) ;
			return true ;
		}
		else return false ;
	}
	
	if( IsList(t1) )
		if( IsList(t2) )
			return Unify(XListHead(t1), XListHead(t2))
					& Unify(XListTail(t1), XListTail(t2)) ;
		else return false ;
	
	if( IsText(t1) )
		if( IsAtomOrText(t2) )
			return EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
		else return false ;

	if( IsAtom(t1) )
		if( IsText(t2) )
			return EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
		else return false ;

	if( IsExtra(t1) )
		if( IsExtra(t2) )
			return EqExtra(t1, t2) ;
		else return false ;
	
	return false ;
}

Bool Equal(register Pt t1, register Pt t2)
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return true ;
	
	if( IsStruct(t1) )
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) ) {
			register n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
			do {
				if( not Equal(*arg1++, *arg2++) ) return false ;
			} while( --n ) ;
			return true ;
		}
		else return false ;
	
	if( IsList(t1) )
		if( IsList(t2) )
			return Equal(XListHead(t1), XListHead(t2))
					& Equal(XListTail(t1), XListTail(t2)) ;
		else return false ;
	
	if( IsText(t1) )
		if( IsAtomOrText(t2) )
			return EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
		else return false ;

	if( IsAtom(t1) )
		if( IsText(t2) )
			return EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
		else return false ;

	if( IsExtra(t1) )
		if( IsExtra(t2) )
			return EqExtra(t1, t2) ;
		else return false ;

	return false ;
}

Bool RawUnify(register Pt t1, register Pt t2)
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return true ;

	if( IsVar(t1) || IsVar(t2) ) return true ;

	if( IsStruct(t1) )
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) ) {
			register n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
			do {
				if( not RawUnify(*arg1++, *arg2++) ) return false ;
			} while( --n ) ;
			return true ;
		}
		else return false ;
	
	if( IsList(t1) )
		if( IsList(t2) )
			return RawUnify(XListHead(t1), XListHead(t2))
					& RawUnify(XListTail(t1), XListTail(t2)) ;
		else return false ;
		
	if( IsText(t1) )
		if( IsAtomOrText(t2) )
			return EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
		else return false ;

	if( IsAtom(t1) )
		if( IsText(t2) )
			return EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
		else return false ;

	if( IsExtra(t1) )
		if( IsExtra(t2) )
			return EqExtra(t1, t2) ;
		else return false ;

	return false ;
}


/* CXPROLOG C'BUILTINS */

static void PTrue()
{
	JumpNext()
}

static void PFail()
{
	DoFail()
}

static void PUnify()
{
	if( Unify(X0, X1) ) JumpNext()
	DoFail()
}

static void PNoUnify(void)
{
	Hdl saveTrail = TR, h ;
	if( Unify(X0, X1) ) DoFail()
	RestoreTrail(saveTrail, h) ;
	JumpNext()
}

static void PEqual()
{
	if( Equal(X0, X1) ) JumpNext()
	DoFail()
}

static void PNoEqual()
{
	if( Equal(X0, X1) ) DoFail()
	JumpNext()
}

void InitUnify()
{
	InstallCBuiltinPred("true", 0, PTrue) ;
	InstallCBuiltinPred("fail", 0, PFail) ;
	InstallCBuiltinPred("=", 2, PUnify) ;
	InstallCBuiltinPred("\\=", 2, PNoUnify) ;
	InstallCBuiltinPred("==", 2, PEqual) ;
	InstallCBuiltinPred("\\==", 2, PNoEqual) ;
}
