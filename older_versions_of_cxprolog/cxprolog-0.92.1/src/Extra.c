/*
 *   This file is part of the CxProlog system

 *   Extra.c
 *   by A.Miguel Dias - 2002/01/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define ExtraTypeTag(e)			((e)->tag)
#define ExtraTypeName(e)		((e)->name)
#define ExtraTypeSize(e)		((e)->size)
#define ExtraTypeNamingFun(e)	((e)->namingFun)
#define ExtraTypeDeleteFun(e)	((e)->deleteFun)
#define ExtraTypeFirst(e)		((e)->first)
#define ExtraTypeLast(e)		((e)->last)
#define ExtraTypeFreeList(e)	((e)->freeList)

typedef struct Extra
{
	ExtraDef(Extra) ;
} Extra, *ExtraPt ;

#define cExtraPt(x)				((ExtraPt)(x))

#define ExtraTag(x)				(cExtraPt(x)->tag)
#define ExtraInUse(x)			(cExtraPt(x)->inUse)
#define ExtraIsHidden(x)		(cExtraPt(x)->hidden)
#define ExtraIsPermanent(x)		(cExtraPt(x)->permanent)
#define ExtraIsMarked(x)		(cExtraPt(x)->marked)
#define ExtraNext(x)			(cExtraPt(x)->next)

#define XExtraTag(t)			ExtraTag(XExtra(t))
#define XExtraInUse(x)			ExtraInUse(XExtra(t))


/* EXTRA-TYPES TABLE */

#define maxExtraTypes 10

static ExtraType allExtraTypes[maxExtraTypes] ;
static int nExtraTypes = 0 ;

ExtraTypePt ExtraTypeNew(CharPt name, Size size, VFunV deleteFun, CFunV namingFun)
{
#if 0
	Mesg("%d %s", nExtraTypes, name) ;
#endif
	if( nExtraTypes < maxExtraTypes ) {
		ExtraTypePt e = allExtraTypes + nExtraTypes ;
		ExtraTypeTag(e) = nExtraTypes ;
		ExtraTypeName(e) = name ;
		ExtraTypeSize(e) = size ;
		ExtraTypeNamingFun(e) = namingFun ;
		ExtraTypeDeleteFun(e) = deleteFun ;
		ExtraTypeFirst(e) = nil ;
		ExtraTypeFreeList(e) = nil ;
		nExtraTypes++ ;
		return e ;
	}
	return InternalError("ExtraTypeNew") ;
}


/* EXTRA-TERMS MANAGEMENT */

Bool IsThisExtra(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	return IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) && XExtraInUse(e) ;
}

static void ExtraLink(ExtraTypePt e, ExtraPt x)
{
	ExtraTag(x) = ExtraTypeTag(e) ;
/* Link to list of existing */
	ExtraNext(x) = nil ;
	if( ExtraTypeFirst(e) == nil )
		ExtraTypeFirst(e) = x ;
	else
		ExtraNext(ExtraTypeLast(e)) = x ;
	ExtraTypeLast(e) = x ;
/* Mark "in use" */
	ExtraInUse(x) = true ;
}

static void ExtraUnlink(ExtraTypePt e, ExtraPt x)
{
/* Unlink from list of existing */
	if( ExtraTypeFirst(e) == x ) /* first is removed */
		ExtraTypeFirst(e) = ExtraNext(x) ;
	else {
		register ExtraPt y ;
		for( y = ExtraTypeFirst(e) ; ExtraNext(y) != x ; y = ExtraNext(y) ) ;
		ExtraNext(y) = ExtraNext(x) ;
		if( ExtraNext(y) == nil ) /* last is removed */
			ExtraTypeLast(e) = y ;
	}
/* Link to free list */
	ExtraNext(x) = ExtraTypeFreeList(e) ;
	ExtraTypeFreeList(e) = x ;
/* Mark "not in use" */
	ExtraInUse(x) = false ;
}

VoidPt ExtraNew(ExtraTypePt e)
{
/* Reuse extra, if available in free list */
	ExtraPt x ;
	if( ExtraTypeFreeList(e) != nil ) {
		x = ExtraTypeFreeList(e) ;
		ExtraTypeFreeList(e) = ExtraNext(x) ;
	}
/* Otherwise, allocate one more extra */
	else {
		x = PermAllocate(ExtraTypeSize(e)) ; /* never deleted */
	}
/* Link to existing */
	ExtraLink(e, x) ;
/* Initialize new */
	ExtraIsHidden(x) = false ;
	ExtraIsPermanent(x) = false ;
	BasicGCAddDelta(ExtraTypeSize(e)) ;
	return x ;
}

void ExtraDelete(ExtraTypePt e, VoidPt x)
{
	ExtraUnlink(e, x) ;
}

void ExtraSetHidden(VoidPt x)
{
	ExtraIsHidden(x) = true ;
}

void ExtraSetPermanent(VoidPt x)
{
	ExtraIsPermanent(x) = true ;
}

void ExtraSetMarked(VoidPt x)
{
	ExtraIsMarked(x) = true ;
}

Size ExtraUnsetMarked(VoidPt x)
{
    ExtraIsMarked(x) = false ;
    return 0 ;
}

Size ExtraForEach(ExtraTypePt e, ExtraFun fun)
{
	if( e != nil ) {	/* Do it for one */
		register ExtraPt x, next ;
		Size n = 0 ;
		doseq(x, ExtraTypeFirst(e), next) {
			next = ExtraNext(x) ;	/* This allows fun to be ExtraDelete */
			n += fun(x) ;
		}
		return n ;
	}
	else {	/* Do it for all */
		int i ;
		Size n = 0 ;
		dotimes(i, nExtraTypes)
			n += ExtraForEach(allExtraTypes + i, fun) ;
		return n ;
	}
}

static Size ExtraForEachNotHidden(ExtraTypePt e, ExtraFun fun)
{
	register ExtraPt x, next ;
	Size n = 0 ;
	doseq(x, ExtraTypeFirst(e), next) {
		next = ExtraNext(x) ;	/* This allows fun to be ExtraDelete */
		if( !ExtraIsHidden(x) )
			n += fun(x) ;
	}
	return n ;
}

VoidPt ExtraFindFirst(ExtraTypePt e, ExtraCond cond, VoidPt arg)
{
	register ExtraPt x ;
	doseq(x, ExtraTypeFirst(e), ExtraNext(x))
		if( cond(x, arg) )
			return x ;
	return nil ;
}

void ExtraPNDCurrent(ExtraTypePt e, BFunV bfun, int arity, int resPos)
{
	ExtraPt x ;
	if( A(arity) == tNilAtom ) {
		if( !IsVar(Drf(Xc(resPos))) ) {
			x = XTestExtra(e,Xc(resPos)) ;
			Discard() ;
			MustBe( !ExtraIsHidden(x) && (bfun == nil || bfun(x)) ) ;
		}
		else x = ExtraTypeFirst(e) ;
	}
	else x = ExtraNext(A(arity)) ;
	doseq(x, x, ExtraNext(x))
		if( !ExtraIsHidden(x) && (bfun == nil || bfun(x)) ) break ;
	A(arity) = cPt(x) ;
	if( x == nil ) Jump(DiscardAndFail) ;
	MustBe( Unify(Xc(resPos), TagExtra(x)) ) ;
}

void ExtraShow(ExtraTypePt e, ExtraFun fun)
{
	Size n ;
	ShowVersion() ;
	Write("%sS:\n", ExtraTypeName(e)) ;
	n = ExtraForEachNotHidden(e, fun) ;
	if( n == 0 ) Write("    %% None\n") ;
	JumpNext() ;
}

static Bool ExtraIsLinked(ExtraTypePt e, VoidPt ref)
{
	register ExtraPt x ;
	doseq(x, ExtraTypeFirst(e), ExtraNext(x))
		if( x == ref )
			return true ;
	return false ;
}

int ExtraGCClearNotMarked(ExtraTypePt e)
{
	if( e != nil ) {	/* Do it for one */
		int n = 0 ;
		if( ExtraTypeDeleteFun(e) != nil ) {
			register ExtraPt x, next ;
			doseq(x, ExtraTypeFirst(e), next) {
				next = ExtraNext(x) ;
				if( !ExtraIsPermanent(x) && !ExtraIsMarked(x) ) {
					ExtraTypeDeleteFun(e)(x) ;
					n++ ;
				}
			}
		}
		return n ;
	}
	else {	/* Do it for all */
		int n = 0, i ;
		dotimes(i, nExtraTypes)
			n += ExtraGCClearNotMarked(allExtraTypes + i) ;
		if( n > 0 && !testGCollection_flag )
			MemoryWarning("%d temporary extra%s removed by garbage collector",
														n, n>1 ? "s" : "") ;
		return n ;
	}
}


/* PT OPERATIONS */

VoidPt XTestExtra(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) )
		t = IVarGet(XAtom(t), true) ;
	if( IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) ) {
		if( !XExtraInUse(t) )
			ImperativeError("Invalid operation over deleted %s %s",
								ExtraTypeName(e), XExtraAsStr(t)) ;
		return XExtra(t) ;
	}
	return ExtraTypeError(e, nil, t) ;
}

Bool XExtraCheck(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) )
		t = IVarGet(XAtom(t), true) ;
	return IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) && XExtraInUse(t) ;
}

CharPt XExtraTypeName(register Pt t) 
{
	VarValue(t) ;
	if( IsExtra(t) ) {
		ExtraTypePt e = allExtraTypes + XExtraTag(t) ;
		return ExtraTypeName(e) ;
	}
	return InternalError("XExtraTypeName") ;
}

CharPt XExtraAsStr(register Pt t) 
{
	VarValue(t) ;
	if( IsExtra(t) ) {
		CharPt n ;
		ExtraTypePt e = allExtraTypes + XExtraTag(t) ;
		if( ExtraTypeNamingFun(e) != nil
			&& (n = ExtraTypeNamingFun(e)(XExtra(t))) != nil )
				return n ;
		else return GStrFormat("1'%s_%lx", ExtraTypeName(e), cWord(XExtra(t))) ;
	}
	return InternalError("XExtraAsStr") ;
}

Pt MakeExtraFromStr(CharPt s) 
{
	CharPt name = s ;
	CharPt sref = nil ;
	Pt ref ;
	ExtraTypePt e = nil ;
	register int i ;

/* Split type-name and ref */
	for( ; *s ; s++ )
		if( *s == '_' ) {
			*s = '\0' ;
			sref = s + 1 ;
			break ;
		}
	if( sref == nil || sscanf(sref, "%lx", cPt(&ref)) != 1 )
		return nil ;

/* Get type decriptor from name */
	dotimes(i, nExtraTypes)
		if( StrEqual(allExtraTypes[i].name, name) )
			e = allExtraTypes + i ;

/* Final tests */
	if( e == nil || !ExtraIsLinked(e, ref) )
		return nil ;

	return TagExtra(ref) ;
}

void BindVarWithExtra(register Pt t, VoidPt ref)
{
	VarValue(t) ;
	if( IsVar(t) ) {
		if( !UnifyWithAtomic(t, TagExtra(ref)) )
			InternalError("BindVarWithExtra") ;
	}
	elif( IsAtom(t) )
		IVarSet(XAtom(t), TagExtra(ref), false) ;
	else TypeError2("VAR or IVAR", t) ;
}

VoidPt ExtraTypeError(ExtraTypePt e, CharPt alt, Pt t)
{
	if( alt == nil ) alt = "IVAR" ;
	return TypeError3(ExtraTypeName(e), alt, t) ;
}
