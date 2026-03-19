/*
 *   This file is part of the CxProlog system

 *   Extra.c
 *   by A.Miguel Dias - 2002/01/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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
#define ExtraTypeFirst(e)		((e)->first)
#define ExtraTypeLast(e)		((e)->last)
#define ExtraTypeFreeList(e)	((e)->freeList)

typedef struct Extra
{
	ExtraDef(Extra) ;
} Extra, *ExtraPt ;

#define cExtraPt(x)			((ExtraPt)(x))
#define ExtraTag(x)			(cExtraPt(x)->tag)
#define ExtraInUse(x)		(cExtraPt(x)->inUse)
#define ExtraVersion(x)		(cExtraPt(x)->version)
#define ExtraNext(x)		(cExtraPt(x)->next)

#define XExtraTag(t)		ExtraTag(XExtra(t))
#define XExtraInUse(x)		ExtraInUse(XExtra(t))


/* EXTRA-TYPES TABLE */

#define maxExtraTypes 10

static ExtraType allExtraTypes[maxExtraTypes] ;
static int nExtraTypes = 0 ;

ExtraTypePt ExtraTypeNew(CharPt name, Size size)
{
	ExtraTypePt e = allExtraTypes + nExtraTypes++ ;
	ExtraTypeTag(e) = 7 + nExtraTypes ;
	ExtraTypeName(e) = name ;
	ExtraTypeSize(e) = size ;
	ExtraTypeFirst(e) = nil ;
	ExtraTypeFreeList(e) = nil ;
	return e ;
}


/* EXTRA-TERMS MANAGEMENT */

Bool IsThisExtra(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	return IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) && XExtraInUse(e) ;
}

static void ExtraLink(ExtraTypePt e, VoidPt x)
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

static void ExtraUnlink(ExtraTypePt e, VoidPt x)
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
	else
		x = Allocate(ExtraTypeSize(e)) ; /* never deleted */
/* Link to existing */
	ExtraLink(e, x) ;
	return x ;
}

void ExtraDelete(ExtraTypePt e, VoidPt x)
{
	ExtraUnlink(e, x) ;
}

Size ForEachExtra(ExtraTypePt e, ExtraFun fun)
{
	register ExtraPt x, next ;
	Size n = 0 ;
	doseq(x, ExtraTypeFirst(e), next) {
		next = ExtraNext(x) ;	/* This allows fun to be ExtraDelete */
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

void PNDCurrentExtra(ExtraTypePt e, BoolV bfun, int arity)
{
	ExtraPt x ;
	if( A(arity) == tNilAtom ) {
		if( !IsVar(Drf(Xc(arity-1))) ) {
			Discard() ;
			MustBe( bfun == nil || bfun(XTestExtra(e,Xc(arity-1))) )
		}
		else x = ExtraTypeFirst(e) ;
	}
	else x = ExtraNext(A(arity)) ;
	doseq(x, x, ExtraNext(x))
		if( bfun == nil || bfun(x) ) break ;
	A(arity) = cPt(x) ;
	if( x == nil ) Jump(DiscardAndFail) ;
	MustBe( Unify(Xc(arity-1), TagExtra(x)) )
}

static Bool ExtraIsLinked(ExtraTypePt e, VoidPt ref)
{
	register ExtraPt x ;
	doseq(x, ExtraTypeFirst(e), ExtraNext(x))
		if( x == ref )
			return true ;
	return false ;
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

CharPt XExtraTypeName(Pt t) 
{
	register int i ;
	Word tag ;
	t = Drf(t) ;
	if( !IsExtra(t) )
		InternalError("XExtraTypeName") ;
	tag = XExtraTag(t) ;
	dotimes(i, nExtraTypes)
		if( allExtraTypes[i].tag == tag )
			return allExtraTypes[i].name ;
	return InternalError("XExtraTypeName") ;
}

CharPt XExtraAsStr(Pt t) 
{
	t = Drf(t) ;
	sprintf(retString, "1'%s_%lx", XExtraTypeName(t) , cWord(XExtra(t))) ;
	return retString ;
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
		if( EqualStr(allExtraTypes[i].name, name) )
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
