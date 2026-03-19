/*
 *   This file is part of the CxProlog system

 *   Unify.c
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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
	return false ;
}

Bool Unifiable(Pt t1, Pt t2)
{
	Bool b ;
	TrailSave() ;
	b = Unify(t1, t2) ;
	TrailRestore() ;
	return b ;
}

Bool UnifiableN(Hdl h1, Hdl h2, int n)
{
	Bool b ;
	TrailSave() ;
	b = UnifyN(h1, h2, n) ;
	TrailRestore() ;
	return b ;
}

#if 1
Bool Unify(register Pt t1, register Pt t2)
{
	UseScratch() ;
	for(;;) {
		VarValue(t1) ; VarValue(t2) ;

		if( t1 == t2 ) goto nextPairL ;

		if( IsVar(t1) ) {
			if( IsVar(t2) ) {
				if( Lt(t1,t2) )
					if( IsLocalVar(t1) ) Assign(t1,t2)
					else Assign(t2,t1)
				else
					if( IsLocalVar(t2) ) Assign(t2,t1)
									else Assign(t1,t2)
			}
			else Assign(t1, t2) ;
			goto nextPairL ;
		}
		
		if( IsVar(t2) ) {
			Assign(t2, t1) ;
			goto nextPairL ;
		}

		if( IsStruct(t1) && IsStruct(t2)
		 && XStructFunctor(t1)==XStructFunctor(t2) ) {
				int arity = XStructArity(t1) ;
				Hdl arg1 = XStructArgs(t1),
					arg2 = XStructArgs(t2) ;
				t1 = *arg1 ;
				t2 = *arg2 ;
				while( --arity ) {
					ScratchPush(*++arg1) ;
					ScratchPush(*++arg2) ;
				}
				continue ;
			}
		
		if( IsList(t1) && IsList(t2) ) {
				ScratchPush(XListTail(t1)) ;
				ScratchPush(XListTail(t2)) ;
				t1 = XListHead(t1) ;
				t2 = XListHead(t2) ;
				continue ;
		}

		FreeScratch() ;	/* failure */
		return false ;

nextPairL:
		if( ScratchUsed() == 0 ) break ;
		else {
			t2 = ScratchPop() ;
			t1 = ScratchPop() ;
		}

	}		
	FreeScratch() ;	/* success */
	return true ;
}

#else
Bool Unify(register Pt t1, register Pt t2)
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return true ;
	
	if( IsVar(t1) ) {
		if( IsVar(t2) ) {
			if( Lt(t1,t2) )
				if( IsLocalVar(t1) ) Assign(t1,t2)
				else Assign(t2,t1)
			else
				if( IsLocalVar(t2) ) Assign(t2,t1)
								else Assign(t1,t2)
		}
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
				if( !Unify(*arg1++, *arg2++) )
					return false ;
			return Unify(*arg1, *arg2) ;
		}
		else return false ;
	}
	
	if( IsList(t1) )
		return IsList(t2)
				&& Unify(XListHead(t1), XListHead(t2))
				&& Unify(XListTail(t1), XListTail(t2)) ;
	
	return false ;
}

#endif

Bool UnifyN(Hdl h1, Hdl h2, int n)
{
	while( n-- )
		if( !Unify(h1[n], h2[n]) )
			return false ;
	return true ;
}

Bool Equal(register Pt t1, register Pt t2)
{
	UseScratch() ;
	for(;;) {
		VarValue(t1) ; VarValue(t2) ;

		if( t1 == t2 ) goto nextPairL ;

		if( IsStruct(t1) && IsStruct(t2)
		 && XStructFunctor(t1)==XStructFunctor(t2) ) {
				int arity = XStructArity(t1) ;
				Hdl arg1 = XStructArgs(t1),
					arg2 = XStructArgs(t2) ;
				t1 = *arg1 ;
				t2 = *arg2 ;
				while( --arity ) {
					ScratchPush(*++arg1) ;
					ScratchPush(*++arg2) ;
				}
				continue ;
			}
		
		if( IsList(t1) && IsList(t2) ) {
				ScratchPush(XListTail(t1)) ;
				ScratchPush(XListTail(t2)) ;
				t1 = XListHead(t1) ;
				t2 = XListHead(t2) ;
				continue ;
		}

		FreeScratch() ;	/* failure */
		return false ;

nextPairL:
		if( ScratchUsed() == 0 ) break ;
		else {
			t2 = ScratchPop() ;
			t1 = ScratchPop() ;
		}

	}		
	FreeScratch() ;	/* success */
	return true ;
}

Bool RawUnify(register Pt t1, register Pt t2)
{
	UseScratch() ;
	for(;;) {
		VarValue(t1) ; VarValue(t2) ;

		if( t1 == t2 ) goto nextPairL ;

		if( IsVar(t1) || IsVar(t2) ) goto nextPairL ;

		if( IsStruct(t1) && IsStruct(t2)
		 && XStructFunctor(t1)==XStructFunctor(t2) ) {
				int arity = XStructArity(t1) ;
				Hdl arg1 = XStructArgs(t1),
					arg2 = XStructArgs(t2) ;
				t1 = *arg1 ;
				t2 = *arg2 ;
				while( --arity ) {
					ScratchPush(*++arg1) ;
					ScratchPush(*++arg2) ;
				}
				continue ;
			}
		
		if( IsList(t1) && IsList(t2) ) {
				ScratchPush(XListTail(t1)) ;
				ScratchPush(XListTail(t2)) ;
				t1 = XListHead(t1) ;
				t2 = XListHead(t2) ;
				continue ;
		}

		FreeScratch() ;	/* failure */
		return false ;

nextPairL:
		if( ScratchUsed() == 0 ) break ;
		else {
			t2 = ScratchPop() ;
			t1 = ScratchPop() ;
		}

	}		
	FreeScratch() ;	/* success */
	return true ;
}

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
	UseScratch() ;
	for(;;) {
		VarValue(t1) ; VarValue(t2) ;

		if( GetTag(t1) == GetTag(t2) ) {
	
			if( t1 == t2 ) {
				res = 0 ;
				goto nextPairL ;
			}
			
			if( IsAtom(t1) ) {
				res = strcmp(XAtomName(t1), XAtomName(t2)) ;
				goto nextPairL ;
			}
				
			if( IsNumber(t1) ) {
				res = CompareNumber(t1, t2) ;
				goto nextPairL ;
			}
	
			if( IsStruct(t1) ) {
				if( XStructFunctor(t1) == XStructFunctor(t2) ) {
					int arity = XStructArity(t1) ;
					Hdl arg1 = XStructArgs(t1),
						arg2 = XStructArgs(t2) ;
						t1 = *arg1 ;
						t2 = *arg2 ;
						while( --arity ) {
							ScratchPush(*++arg1) ;
							ScratchPush(*++arg2) ;
						}
						continue ;
				}
				else {
					res = CompareFunctors(XStructFunctor(t1),
											XStructFunctor(t2)) ;
					goto nextPairL ;
				}
			}
			
			if( IsList(t1) ) {
				ScratchPush(XListTail(t1)) ;
				ScratchPush(XListTail(t2)) ;
				t1 = XListHead(t1) ;
				t2 = XListHead(t2) ;
				continue ;
			}
	
			if( IsVar(t1) ) {
				if( IsLocalRef(t1) ) { /* globalize t1 @@ */
					Assign(t1, MakeVar()) ;
					DrfVar(t1) ;
				}
				if( IsLocalRef(t2) ) {
					Assign(t2, MakeVar()) ; /* globalize t2 @@ */
					DrfVar(t2) ;
				}
				res = Gt(t1, t2) ? 1 : -1 ;
				goto nextPairL ;
			}
	
			if( IsExtra(t1) ) {
				res = Gt(t1, t2) ? 1 : -1 ;
				goto nextPairL ;
			}
	
			InternalError("Compare (1)") ;
		}
		else {
			if( IsNumber(t1) && IsNumber(t2) ) {
				res = CompareNumber(t1, t2) ;
				goto nextPairL ;
			}
			else {
				res = GetTag(t1) > GetTag(t2) ? 1 : -1 ;
				goto nextPairL ;
			}
		}

		InternalError("Compare (2)") ;
		goto nextPairL ;

nextPairL:
		if( res != 0 || ScratchUsed() == 0 ) break ;
		else {
			t2 = ScratchPop() ;
			t1 = ScratchPop() ;
		}
	}		
	FreeScratch() ;
	return res ;
}

static Bool VarOccurs(register Pt v, register Pt t)
{
	VarValue(v) ;
	VarValue(t) ;
	
	if( IsVar(t) )
		return t == v ;
	if( IsAtomic(t) )
		return false ;
	if( IsList(t) )
		return VarOccurs(v, XListHead(t)) || VarOccurs(v, XListTail(t)) ;
	if( IsStruct(t) ) {
		int i = XStructArity(t) ;
		while( i-- )
			if( VarOccurs(v, XStructArg(t,i)) )
				return true ;
		return false ;
	}
	InternalError("VarOccurs") ;
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
		if( !IsThisStruct(*array, hifenFunctor) )
			TypeError2("KEY-LIST", nil) ;
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
	if( Unifiable(X0, X1) ) DoFail()
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
	ZEnsureFreeSpaceOnStacks(2 * n, "sort/2") ; /* stacks may grow */
	if( Unify(X1, ArrayToList(array, n)) ) JumpNext()
	DoFail()
}

static void PQuickSort(void)
{
	Size n ;
	Hdl array = ListToArray(X0, &n) ;
	if( n > 1 )
		TermQuickSort(array, array + n - 1) ;
	ZEnsureFreeSpaceOnStacks(2 * n, "msort/2") ; /* stacks may grow */
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
	ZEnsureFreeSpaceOnStacks(2 * n, "keysort/2") ; /* stacks may grow */
	if( Unify(X1, ArrayToList(array, n)) ) JumpNext()
	DoFail()
}

void UnifyInit()
{
	InstallCBuiltinPred("true", 0, PTrue) ;
	InstallCBuiltinPred("fail", 0, PFail) ;
	InstallCBuiltinPred("false", 0, PFail) ;
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
