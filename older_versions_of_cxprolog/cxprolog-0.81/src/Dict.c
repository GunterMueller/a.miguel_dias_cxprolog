/*
 *   This file is part of the CxProlog system

 *   Dict.c
 *   by A.Miguel Dias - 2000/09/29
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

typedef struct Entry {
	Pt key ;
	Pt value ;
} Entry, *EntryPt ;

typedef struct Dict {
	ExtraDef(Dict) ;
	EntryPt begin, end, first, last  ;
} Dict, *DictPt ;

#define cDictPt(x)				((DictPt)(x))

#define DictBegin(d)			((d)->begin)
#define DictEnd(d)				((d)->end)
#define DictFirst(d)			((d)->first)
#define DictLast(d)				((d)->last)

static ExtraTypePt dictType ;

static Size DictSize(DictPt d)
{
	return DictLast(d) - DictFirst(d) ;
}

static Size DictCapacity(DictPt d)
{
	return DictEnd(d) - DictBegin(d) ;
}

static void DictInit(DictPt d, Size capacity)
{
	DictBegin(d) = TempBlockAllocate(capacity * WordsOf(Entry)) ;
	DictEnd(d) = DictBegin(d) + capacity ;
	DictFirst(d) = DictBegin(d) ;
	DictLast(d) = DictBegin(d) ;
}

static void DictExpand(DictPt d, EntryPt *rel)
{
	EntryPt b = DictBegin(d) ;
	EntryPt f = DictFirst(d) ;
	EntryPt l = DictLast(d) ;
	EntryPt h ;
	DictInit(d, 2 * DictCapacity(d)) ;
	for( h = DictBegin(d) ; f < l ; *h++ = *f++ ) ;
	DictLast(d) = h ;
	BlockRelease(b) ;
	*rel = DictBegin(d) + (*rel - b) ;
}

static void DictOpenSpace(DictPt d, EntryPt *here)
{
	register EntryPt h ;
	if( DictLast(d) == DictEnd(d) )
		DictExpand(d, here) ;
	DictLast(d)++ ;
	for( h = DictLast(d) - 1 ; h > *here ; h-- )
		h[0] = h[-1] ;
}

static void DictCloseSpace(DictPt d, EntryPt here)
{
	register EntryPt h ;
	for( h = here + 1 ; h < DictLast(d) ; h++ )
		h[-1] = h[0] ;
	DictLast(d)-- ;
}

static Bool DictFind(DictPt d, Pt k, EntryPt *here)
{
	register EntryPt lo, hi, middle ;
	int res ;
	lo = DictFirst(d) ;
	hi = DictLast(d) - 1 ;
	while( lo <= hi ) {
		middle = lo + (hi - lo) / 2 ;
		if( (res = Compare(middle->key, k)) == 0 )
			{ *here = middle ; return true ; }
		elif( res < 0 )
			lo = middle + 1 ;
		else
			hi = middle - 1 ;
	}
	*here = lo ;
	return false ;
}

static DictPt DictNew(void)
{
	DictPt d = ExtraNew(dictType) ;
	DictInit(d, 4) ;
	return d ;
}

static void DictClear(DictPt d)
{
	register EntryPt h ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h++ ) {
		ReleaseTerm(h->key) ;
		ReleaseTerm(h->value) ;
	}
	DictLast(d) = DictFirst(d) ;
}

static void DictDelete(DictPt d)
{
	DictClear(d) ;
	BlockRelease(DictBegin(d)) ;
	ExtraDelete(dictType, d) ;
}

static void DictSet(DictPt d, Pt k, Pt t)
{
	EntryPt here ;
	if( DictFind(d, k, &here) ) {
		ReleaseTerm(here->value) ;
		here->value = AllocateTermForAssign(t) ;
	}
	else {
		DictOpenSpace(d, &here) ;
		here->key = AllocateTermForAssign(k) ;
		here->value = AllocateTermForAssign(t) ;
	}
}

static Bool DictGet(DictPt d, Pt k, Pt *t)
{
	EntryPt here ;
	if( DictFind(d, k, &here) ) {
		*t = here->value ;
		return true ;
	}
	return false ;
}

static Bool DictDeleteItem(DictPt d, Pt k)
{
	EntryPt here ;
	if( DictFind(d, k, &here) ) {
		ReleaseTerm(here->key) ;
		ReleaseTerm(here->value) ;
		DictCloseSpace(d, here) ;
		return true ;
	}
	return false ;
}

static void DictWrite(StreamPt stm, DictPt d)
{
	register EntryPt h ;
	StreamWrite(stm, "%s", XExtraAsStr(TagExtra(d))) ;
	StreamWrite(stm, "     (current capacity %ld)\n", DictCapacity(d)) ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h++ ) {
		StreamWrite(stm, "\t%s -- ", TermAsStr(h->key)) ;
		StreamWrite(stm, "%s\n", TermAsStr(h->value)) ;
	}
}

static void DictsAtomGCMarkAux(VoidPt x)
{
	DictPt d = cDictPt(x) ;
	register EntryPt h ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h++ ) {
		TermAtomGCMark(h->key) ;
		TermAtomGCMark(h->value) ;
	}
}
void DictsAtomGCMark()
{
	ExtraForEach(dictType, DictsAtomGCMarkAux) ;
}


/* CXPROLOG C'BUILTINS */

static void PDictCheck()
{
	if( XExtraCheck(dictType, X0) ) JumpNext()
	DoFail()
}

static void PDictNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		DictPt d = DictNew() ;
		if( UnifyWithAtomic(t, TagExtra(d)) ) JumpNext()
		InternalError("PDictNew") ;
	}
	if( IsAtom(t) ) {
		DictPt d = DictNew() ;
		IVarSet(XAtom(t), TagExtra(d)) ;
		JumpNext()
	}
	TypeError2("VAR or IVAR", t) ;
}

static void PDictClear()
{
	DictClear(XTestExtra(dictType,X0)) ;
	JumpNext()
}

static void PDictDelete()
{
	DictDelete(XTestExtra(dictType,X0)) ;
	JumpNext()
}

static void PDictSet()
{
	DictSet(XTestExtra(dictType,X0), XTestNonVar(X1), XTestNonVar(X2)) ;
	JumpNext()
}

static void PDictGet()
{
	Pt t ;
	if( DictGet(XTestExtra(dictType,X0), XTestNonVar(X1), &t) ) {
		t = ZPushTerm(t) ;
		if( Unify(X2,t) ) JumpNext()
	}
	DoFail()
}

static void PDictDeleteItem()
{
	if( DictDeleteItem(XTestExtra(dictType,X0), XTestNonVar(X1)) )
		JumpNext()
	DoFail()
}

static void PDictAsList()
{
	DictPt d = XTestExtra(dictType,X0) ;
	EntryPt h ; Pt t ;
	Word w[3] ;
	w[0] = cWord(hifenFunctor) ;	
	Z = tNilAtom ;
	for( h = DictLast(d) - 1 ; h >= DictFirst(d) ; h-- ) {
		w[1] = cWord(h->key) ;
		w[2] = cWord(h->value) ;
		t = ZPushTerm(TagStruct(w)) ;
		Z = MakeBinStruct(listFunctor, t, Z) ;
	}
	if( Unify(Z, X1) ) JumpNext()
	DoFail()
}

static void PDictWrite()
{
	DictWrite(currOut, XTestExtra(dictType,X0)) ;
	JumpNext()
}

static void PSDictWrite()
{
	DictWrite(XTestStream(X0, mWrite), XTestExtra(dictType,X1)) ;
	JumpNext()
}

static void PNDCurrentDict()
{
	PNDCurrentExtra(dictType) ;
	JumpNext()
}

static void DictsAux(VoidPt x)
{
	DictPt d = cDictPt(x) ;
	AtomPt at = IVarWith(TagExtra(d)) ;
	Write("  %16s -> size =%2ld, capacity =%2ld", 
				TermAsStr(TagExtra(d)),
				DictSize(d),
				DictCapacity(d)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
}
static void PDicts()
{
	VersionShow() ;
	Write("Dicts:\n") ;
	ExtraForEach(dictType, DictsAux) ;
	JumpNext()
}

void DictsInit()
{
	dictType = ExtraTypeNew("dict", WordsOf(Dict)) ;

	IVarConstSet(LookupAtom("$dict"), TagExtra(DictNew())) ;

	InstallCBuiltinPred("dict", 1, PDictCheck) ;
	InstallCBuiltinPred("dict_new", 1, PDictNew) ;
	InstallCBuiltinPred("dict_clear", 1, PDictClear) ;
	InstallCBuiltinPred("dict_delete", 1, PDictDelete) ;
	InstallCBuiltinPred("dict_set", 3, PDictSet) ;
	InstallCBuiltinPred("dict_get", 3, PDictGet) ;
	InstallCBuiltinPred("dict_delete_item", 2, PDictDeleteItem) ;
	InstallCBuiltinPred("dict_as_list", 2, PDictAsList) ;
	InstallCBuiltinPred("dict_write", 1, PDictWrite) ;
	InstallCBuiltinPred("dict_write", 2, PSDictWrite) ;
	InstallNDeterCBuiltinPred("current_dict", 1, PNDCurrentDict) ;
	InstallNDeterCBuiltinPred("dicts", 0, PDicts) ;
}
