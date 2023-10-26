/*
 *   This file is part of the CxProlog system

 *   Unify.c
 *   by A.Miguel Dias - 1989/11/14
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

static int CompareFunctors(FunctorPt f1, FunctorPt f2)
{
	return
		FunctorArity(f1) != FunctorArity(f2)
			? CompareInt(FunctorArity(f1), FunctorArity(f2))
			: strcmp(FunctorName(f1), FunctorName(f2)) ;
}

int Compare(register Pt t1, register Pt t2)
{
	int res ;

	VarValue(t1) ;
	VarValue(t2) ;

	if( GetTag(t1) == GetTag(t2) ) {

		if( t1 == t2 ) return 0 ;
		
		elif( IsAtomOrText(t1) )
			return strcmp(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
			
		elif( IsNumber(t1) )
			return CompareNumber(t1, t2) ;

		elif( IsList(t1) ) {
			if( (res = Compare(XListHead(t1), XListHead(t2))) != 0 )
				return res  ;
			return Compare(XListTail(t1), XListTail(t2)) ;
		}

		elif( IsStruct(t1) ) {
			if( XStructFunctor(t1) == XStructFunctor(t2) ) {
				register int n = XStructArity(t1) ;
				Hdl arg1 = XStructArgs(t1),
					arg2 = XStructArgs(t2) ;
				while( --n )
					if( (res = Compare(*arg1++, *arg2++)) != 0 )
						return( res ) ;
				return Compare(*arg1, *arg2) ;
			}
			else
				return CompareFunctors(XStructFunctor(t1), XStructFunctor(t2)) ;
		}
		
		elif( IsVar(t1) ) {
			if( IsLocalVarStrong(t1) ) {
				Assign(t1, PushVar(H)) ; /* globalize t1 */
				DrfVar(t1) ;
			}
			if( IsLocalVarStrong(t2) ) {
				Assign(t2, PushVar(H)) ; /* globalize t2 */
				DrfVar(t2) ;
			}
			return Gt(t1, t2) ? 1 : -1 ;
		}

		elif( IsExtra(t1) )
			return Gt(t1, t2) ? 1 : -1 ;

		InternalError("Compare") ;
		return 0 ;
	}
	else {
		if( IsAtomOrText(t1) && IsAtomOrText(t2) )
			return strcmp(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
		elif( IsNumber(t1) && IsNumber(t2) )
			return CompareNumber(t1, t2) ;
		else return GetTag(t1) > GetTag(t2) ? 1 : -1 ;
	}
}

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
		return IsAtomOrText(at)
			&& EqualStr(XAtomOrTextName(t), XAtomOrTextName(at)) ;

	if( IsAtom(t) )
		return IsText(at)
			&& EqualStr(XAtomOrTextName(t), XAtomOrTextName(at)) ;

	return false ;
}

Bool Unify(register Pt t1, register Pt t2)
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return true ;
	
	if( IsVar(t1) ) {
		if( IsVar(t2) ) Bind(t1, t2)
		else Assign(t1, t2) ;
		return true ;
	}
	
	if( IsVar(t2) ) {
		Assign(t2, t1) ;
		return true ;
	}

	if( IsStruct(t1) ) {
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) ) {
			register int n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
			while( --n )
				if( not Unify(*arg1++, *arg2++) )
					return false ;
			return Unify(*arg1, *arg2) ;
		}
		else return false ;
	}
	
	if( IsList(t1) )
		return IsList(t2)
				&& Unify(XListHead(t1), XListHead(t2))
				&& Unify(XListTail(t1), XListTail(t2)) ;
	
	if( IsText(t1) )
		return IsAtomOrText(t2)
			&& EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;

	if( IsAtom(t1) )
		return IsText(t2)
			&& EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;
	
	return false ;
}

Bool Equal(register Pt t1, register Pt t2)
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return true ;
	
	if( IsStruct(t1) )
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) ) {
			register int n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
			while( --n )
				if( not Equal(*arg1++, *arg2++) )
					return false ;
			return Equal(*arg1, *arg2) ;
		}
		else return false ;
	
	if( IsList(t1) )
		return IsList(t2)
				&& Equal(XListHead(t1), XListHead(t2))
				&& Equal(XListTail(t1), XListTail(t2)) ;
	
	if( IsText(t1) )
		return IsAtomOrText(t2)
				&& EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;

	if( IsAtom(t1) )
		return IsText(t2)
				&& EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;

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
			register int n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
			while( --n )
				if( not RawUnify(*arg1++, *arg2++) )
					return false ;
			return RawUnify(*arg1, *arg2) ;
		}
		else return false ;
	
	if( IsList(t1) )
		return IsList(t2)
				&& RawUnify(XListHead(t1), XListHead(t2))
				&& RawUnify(XListTail(t1), XListTail(t2)) ;
		
	if( IsText(t1) )
		return IsAtomOrText(t2)
				&& EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;

	if( IsAtom(t1) )
		return IsText(t2)
				&& EqualStr(XAtomOrTextName(t1), XAtomOrTextName(t2)) ;

	return false ;
}

#define DefineQuickSort(SortName, Comp, type)	\
static void SortName(type *l, type *r)			\
{												\
	type *ll = l ;								\
	type *rr = r ;								\
	type x, z ;									\
												\
	x = ll[(rr - ll) / 2] ;						\
	do											\
	{											\
		while( Comp((*ll), x) < 0 ) ll++ ;		\
		while( Comp(x, (*rr)) < 0 ) rr-- ;		\
		if( ll <= rr )							\
		{										\
			z = *ll ;							\
			*ll++ = *rr ;						\
			*rr-- = z ;							\
		}										\
	}											\
	while( ll <= rr ) ;							\
	if( l < rr ) SortName(l, rr) ;				\
	if( ll < r ) SortName(ll, r) ;				\
}

DefineQuickSort(TermQuickSort, Compare, Pt)

static int CompareKeyTerms(Pt t1, Pt t2) /* pre: key-terms */
{
	return Compare(XStructArg(t1, 0), XStructArg(t2, 0)) ;
}

DefineQuickSort(KeyTermQuickSort, CompareKeyTerms, Pt)

static void RemoveDuplicates(Hdl array, Size *len)	/* pre: *len > 1 */
{
	Hdl get, put, stop ;
	stop = array + *len ;
	put = array + 1 ;
	for( get = array + 1 ; get < stop ; get++ )
		if( Compare(put[-1], *get) != 0 )
			*put++ = *get ;
	*len = put - array ;
}

static void CheckAllKey(register Hdl array, Size len)
{
	register Hdl stop ;
	for( stop = array + len ; array < stop ; array++ )
		if( not IsThisStruct(*array, hifenFunctor) )
			Error("Not a proper key-list") ;
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

static void PFalse()
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
	TrailSave() ;
	if( Unify(X0, X1) ) DoFail()
	TrailRestore() ;
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

static void PLessThan(void)
{
	if( Compare(X0, X1) < 0 ) JumpNext()
	DoFail()
}

static void PGreaterThan(void)
{
	if( Compare(X0, X1) > 0 ) JumpNext()
	DoFail()
}

static void PLessOrEqualThan(void)
{
	if( Compare(X0, X1) <= 0 ) JumpNext()
	DoFail()
}

static void PGreaterOrEqualThan(void)
{
	if( Compare(X0, X1) >= 0 ) JumpNext()
	DoFail()
}

static void PSort(void)
{
	Size n ;
	Hdl array = ListToArray(X0, &n) ;
	if( n > 1 ) {
		TermQuickSort(array, array + n - 1) ;
		RemoveDuplicates(array, &n) ;
	}
	if( Unify(X1, ArrayToList(array, n)) ) JumpNext()
	DoFail()
}

static void PQuickSort(void)
{
	Size n ;
	Hdl array = ListToArray(X0, &n) ;
	if( n > 1 )
		TermQuickSort(array, array + n - 1) ;
	if( Unify(X1, ArrayToList(array, n)) ) JumpNext()
	DoFail()
}

static void PKeySort(void)
{
	Size n ;
	Hdl array = ListToArray(X0, &n) ;
	CheckAllKey(array, n) ;
	if( n > 1 )
		KeyTermQuickSort(array, array + n - 1) ;
	if( Unify(X1, ArrayToList(array, n)) ) JumpNext()
	DoFail()
}

void InitUnify()
{
	InstallCBuiltinPred("true", 0, PTrue) ;
	InstallCBuiltinPred("fail", 0, PFail) ;
	InstallCBuiltinPred("false", 0, PFalse) ;
	InstallCBuiltinPred("=", 2, PUnify) ;
	InstallCBuiltinPred("\\=", 2, PNoUnify) ;
	InstallCBuiltinPred("==", 2, PEqual) ;
	InstallCBuiltinPred("\\==", 2, PNoEqual) ;
	InstallCBuiltinPred("@<", 2, PLessThan) ;
	InstallCBuiltinPred("@>", 2, PGreaterThan) ;
	InstallCBuiltinPred("@=<", 2, PLessOrEqualThan) ;
	InstallCBuiltinPred("@>=", 2, PGreaterOrEqualThan) ;
	InstallCBuiltinPred("sort", 2, PSort) ;
	InstallCBuiltinPred("msort", 2, PQuickSort) ;
	InstallCBuiltinPred("keysort", 2, PKeySort) ;
}
