/*
 *   This file is part of the CxProlog system

 *   Dictionary.c
 *   by A.Miguel Dias - 2000/09/29
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

typedef struct Dict
{
	Word tagHolder ;
	Hdl begin, end, first, last  ;
	struct Dict *next ;
} Dict, *DictPt ;

#define DictTagHolder(d)		((d)->tagHolder)
#define DictBegin(d)			((d)->begin)
#define DictEnd(d)				((d)->end)
#define DictFirst(d)			((d)->first)
#define DictLast(d)				((d)->last)
#define DictNext(d)				((d)->next)

#define cDictPt(x)				((DictPt)(x))

static DictPt dictList = nil ;
static DictPt freeDictList = nil ;

static void DictInit(DictPt d, long capacity)
{
	DictBegin(d) = TemporaryAllocate(capacity * 2) ;
	DictEnd(d) = DictBegin(d) + capacity * 2 ;
	DictFirst(d) = DictBegin(d) ;
	DictLast(d) = DictBegin(d) ;
}

static long DictSize(DictPt d)
{
	return (DictLast(d) - DictFirst(d)) / 2 ;
}

static long DictCapacity(DictPt d)
{
	return (DictEnd(d) - DictBegin(d)) / 2 ;
}

static void DictGrow(DictPt d)
{
	Hdl b = DictBegin(d) ;
	Hdl f = DictFirst(d) ;
	Hdl l = DictLast(d) ;
	Hdl h ;
	
	DictInit(d, 2 * DictCapacity(d)) ;
	for( h = DictBegin(d) ; f < l ; *h++ = *f++ ) ;
	DictLast(d) = h ;
	Release(b) ;
}

static DictPt DictNew(void)
{
	DictPt d = TemporaryAllocate(WordsOf(Dict)) ;
	DictTagHolder(d) = dictSubTag ;
	DictNext(d) = dictList ;
	dictList = d ;
	DictInit(d, 4) ;
	return d ;
}

static void DictClear(DictPt d)
{
	register Hdl h ;

	for( h = DictFirst(d) ; h < DictLast(d) ; h += 2 ) {
		ReleaseTerm(h[0]) ;
		ReleaseTerm(h[1]) ;
	}
	DictLast(d) = DictFirst(d) ;
}

static void DictDelete(DictPt d)
{
	if( dictList == d )	/* first is removed */
		dictList = DictNext(d) ;
	else {
		register DictPt x ;
		for( x = dictList ; DictNext(x) != d ; x = DictNext(x) ) ;
		DictNext(x) = DictNext(d) ;
	}
	DictClear(d) ;
	Release(DictBegin(d)) ;

	DictBegin(d) = nil ;	/* Mark "dictionary not in use" */
	DictNext(d) = freeDictList ;
	freeDictList = d ;
}

static void DictSet(DictPt d, Pt k, Pt t)
{
	register Hdl h ;

	k = Drf(k) ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h += 2 )
		if( Compare(*h, k) == 0 ) {
			ReleaseTerm(h[1]) ;
			h[1] = AllocateTermForAssign(t) ;
			return ;
		}
	if( DictLast(d) == DictEnd(d) )
		DictGrow(d) ;
	*DictLast(d)++ = AllocateTermForAssign(k) ;
	*DictLast(d)++ = AllocateTermForAssign(t) ;
}

static Bool DictGet(DictPt d, Pt k, Pt *t)
{
	register Hdl h ;

	k = Drf(k) ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h += 2 )
		if( Compare(*h, k) == 0 ) {
			*t = PushTerm(h[1]) ;
			return true ;
		}
	return false ;
}

static Bool DictDeleteItem(DictPt d, Pt k)
{
	register Hdl h ;

	k = Drf(k) ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h += 2 )
		if( Compare(*h, k) == 0 ) {
			ReleaseTerm(h[0]) ;
			ReleaseTerm(h[1]) ;
			for( ; h < DictLast(d) - 2 ; h += 2 ) {
				h[0] = h[2] ;
				h[1] = h[3] ;
			}
			DictLast(d) -= 2 ;
			return true ;
		}
	return false ;
}

static Pt DictAsList(DictPt d)
{
	Hdl h ;	
	Pt list = tNilAtom ;
	CheckGlobalStackOverflow() ;
	for( h = DictLast(d) - 2 ; h >= DictFirst(d) ; h -= 2 ) {
		Pt t = MakeBinStruct(hifenFunctor, PushTerm(h[0]), PushTerm(h[1])) ;
		list = MakeBinStruct(listFunctor, t, list) ;
	}
	CheckGlobalStackOverflow() ;
	return list ;
}

static void DictWrite(StreamPt stm, DictPt d)
{
	Hdl h ;
	WriteStream(stm, "%s", XExtraAsStr(TagExtra(d))) ;
	WriteStream(stm, "     (current capacity %ld)\n", DictCapacity(d)) ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h += 2 ) {
		WriteStream(stm, "\t%s -- ", TermAsStr(h[0])) ;
		WriteStream(stm, "%s\n", TermAsStr(h[1])) ;
	}
}

static DictPt UsingDict(DictPt d)
{
	if( DictBegin(d) == nil )
		Error("Invalid operation over deleted dictionary %s", XExtraAsStr(TagExtra(d))) ;
	return d ;
}

Bool DictCheck(VoidPt ref)
{
	register DictPt d ;

	dolist(d, dictList, DictNext(d))
		if( d == ref )
			return DictBegin(d) != nil ;
	return false ;
}


/* CXPROLOG C'BUILTINS */

static DictPt XTestDict(register Pt t)
{
	VarValue(t) ;
	if( IsAtomOrText(t) )
		t = IVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == dictSubTag )
		return UsingDict(cDictPt(XPt(t))) ;
	TypeError("dict or ivar", t) ;
	return nil ;
}

static void PDictCheck()
{
	Pt t = Drf(X0) ;
	if( IsAtomOrText(t) )
		t = IVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == dictSubTag && DictCheck(XPt(t)) ) JumpNext()
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
	if( IsAtomOrText(t) ) {
		DictPt d = DictNew() ;
		IVarSet(XAtomOrTextAsAtom(t), TagExtra(d)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PDictClear()
{
	DictClear(XTestDict(X0)) ;
	JumpNext()
}

static void PDictDelete()
{
	DictDelete(XTestDict(X0)) ;
	JumpNext()
}

static void PDictSet()
{
	DictSet(XTestDict(X0),XTestNonVar(X1),X2) ;
	JumpNext()
}

static void PDictGet()
{
	Pt t ;
	if( DictGet(XTestDict(X0),XTestNonVar(X1),&t) && Unify(X2,t) ) JumpNext()
	DoFail()
}

static void PDictDeleteItem()
{
	if( DictDeleteItem(XTestDict(X0),XTestNonVar(X1)) ) JumpNext()
	DoFail()
}

static void PDictAsList()
{
	if( Unify(DictAsList(XTestDict(X0)), X1) ) JumpNext()
	DoFail()
}

static void PDictWrite()
{
	DictWrite(currOut, XTestDict(X0)) ;
	JumpNext()
}

static void PSDictWrite()
{
	DictWrite(XTestStream(X0, mWrite), XTestDict(X1)) ;
	JumpNext()
}

static void PNDCurrentDict()
{
	DictPt d =
		A(1) == tNilAtom ? dictList : DictNext(cDictPt(A(1))) ;
	A(1) = cPt(d) ;
	if( d == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagExtra(d)) ) JumpNext()
	DoFail()
}

static void PDicts()
{
	DictPt d ;
	ShowVersion() ;
	Write("Dicts:\n") ;
	dolist(d, dictList, DictNext(d))
		Write("  %16s -> size =%2d, capacity =%2d\n", 
					TermAsStr(TagExtra(d)),
					DictSize(d),
					DictCapacity(d)) ;
	JumpNext()
}

void InitDicts()
{
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
