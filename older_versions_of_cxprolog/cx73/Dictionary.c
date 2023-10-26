/*
 *   This file is part of the CxProlog system

 *   Dicttionary.c
 *   by A.Miguel Dias - 2000/09/29
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

static void InitDict(DictPt d, long capacity)
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

static void GrowDict(DictPt d)
{
	Hdl b = DictBegin(d) ;
	Hdl f = DictFirst(d) ;
	Hdl l = DictLast(d) ;
	Hdl h ;
	
	InitDict(d, 2 * DictCapacity(d)) ;
	for( h = DictBegin(d) ; f < l ; *h++ = *f++ ) ;
	DictLast(d) = h ;
	Release(b) ;
}

static DictPt DictNew(void)
{
	DictPt d = TemporaryAllocate(WordsOf(Dict)) ;
	DictTagHolder(d) = 0 ;
	DictNext(d) = dictList ;
	dictList = d ;
	InitDict(d, 4) ;
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

static void DictFree(DictPt d)
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
	Release(d) ;
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
		GrowDict(d) ;
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

static Bool DictRemove(DictPt d, Pt k)
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

static void DictWrite(DictPt d)
{
	Hdl h ;
	
	Write("%s", XExtraAsStr(TagExtra(d, dictSubTag))) ;
	Write("     (current capacity %ld)\n", DictCapacity(d)) ;
	for( h = DictFirst(d) ; h < DictLast(d) ; h += 2 ) {
		Write("\t") ;
		WriteTerm(h[0]) ;
		Write(" -- ") ;
		WriteTerm(h[1]) ;
		Nl() ;
	}
}


/* CXPROLOG C'BUILTINS */

static DictPt XTestDict(register Pt t)
{
	VarValue(t) ;
	if( IsAtomOrText(t) )
		t = ImperativeVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == dictSubTag )
		return cDictPt(XPt(t)) ;
	TypeError("dict or ivar", t) ;
	return nil ;
}

static void PDictCheck()
{
	Pt t = Drf(X0) ;
	if( IsAtomOrText(t) )
		t = ImperativeVarGet(XAtomOrTextAsAtom(t)) ;
	if( IsExtra(t) && XExtraSubTag(t) == dictSubTag ) JumpNext()
	DoFail()
}

static void PDictNew()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		DictPt d = DictNew() ;
		if( UnifyWithAtomic(t, TagExtra(d, dictSubTag)) ) JumpNext()
		InternalError("PDictNew") ;
	}
	if( IsAtomOrText(t) ) {
		DictPt d = DictNew() ;
		ImperativeVarSet(XAtomOrTextAsAtom(t), TagExtra(d, dictSubTag)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PDictClear()
{
	DictClear(XTestDict(X0)) ;
	JumpNext()
}

static void PDictFree()
{
	DictFree(XTestDict(X0)) ;
	JumpNext()
}

static void PDictSet()
{
	DictSet(XTestDict(X0),X1,X2) ;
	JumpNext()
}

static void PDictGet()
{
	Pt t ;
	if( DictGet(XTestDict(X0),X1,&t) && Unify(X2,t) ) JumpNext()
	DoFail()
}

static void PDictRemove()
{
	if( DictRemove(XTestDict(X0),X1) ) JumpNext()
	DoFail()
}

static void PDictWrite()
{
	DictWrite(XTestDict(X0)) ;
	JumpNext()
}

static void PNDCurrentDict()
{
	DictPt d =
		A(1) == tNilAtom ? dictList : DictNext(cDictPt(A(1))) ;
	A(1) = cPt(d) ;
	if( d == nil ) Jump(DiscardAndFail) ;
	if( Unify(X0, TagExtra(d, dictSubTag)) ) JumpNext()
	DoFail()
}

Bool DictCheck(Pt t)
{
	register DictPt d ;

	dolist(d, dictList, DictNext(d))
		if( cPt(d) == t )
			return true ;
	return false ;
}

void InitDicts()
{
	InstallCBuiltinPred("dict", 1, PDictCheck) ;
	InstallCBuiltinPred("dict_new", 1, PDictNew) ;
	InstallCBuiltinPred("dict_clear", 1, PDictClear) ;
	InstallCBuiltinPred("dict_free", 1, PDictFree) ;
	InstallCBuiltinPred("dict_set", 3, PDictSet) ;
	InstallCBuiltinPred("dict_get", 3, PDictGet) ;
	InstallCBuiltinPred("dict_remove", 2, PDictRemove) ;
	InstallCBuiltinPred("dict_write", 1, PDictWrite) ;
	InstallNDeterCBuiltinPred("current_dict", 1, PNDCurrentDict) ;
}
