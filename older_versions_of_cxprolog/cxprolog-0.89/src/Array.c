/*
 *   This file is part of the CxProlog system

 *   Array.c
 *   by A.Miguel Dias - 2002/12/30
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

static ExtraTypePt arrayType ;


/* PRIVATE FUNCTIONS */

static Size ArrayCapacity(ArrayPt a)
{
	return ArrayEnd(a) - ArrayBegin(a) ;
}

static void ArrayInit(ArrayPt a, Size capacity)
{
	register AEntryPt h ;
	ArrayBegin(a) = TempAllocate(capacity * WordsOf(AEntry)) ;
	ArrayEnd(a) = ArrayBegin(a) + capacity ;
	for( h = ArrayBegin(a) ; h < ArrayEnd(a) ; h++ )
		h->value = nil ;
}

static void ArrayExpand(ArrayPt a, PInt minIdx)
{
	PInt cap ;
	AEntryPt b = ArrayBegin(a) ;
	AEntryPt e = ArrayEnd(a) ;
	register AEntryPt s, h ;
	for( cap = 2 * ArrayCapacity(a) ; cap <= minIdx ; cap *= 2 ) ;
	ArrayInit(a, cap) ;
	for( h = ArrayBegin(a), s = b ; s < e ; *h++ = *s++ ) ;
	Release(b) ;
}

static void ArrayWrite(StreamPt stm, ArrayPt a)
{
	register AEntryPt h ;
	StreamWrite(stm, "%s", XExtraAsStr(TagExtra(a))) ;
	StreamWrite(stm, "     (current capacity %ld)\n", ArrayCapacity(a)) ;
	for( h = ArrayBegin(a) ; h < ArrayEnd(a) ; h++ )
		if( h->value != nil ) {
            StreamWrite(stm, "\t%ld -- ", h - ArrayBegin(a) + 1) ;
			StreamWrite(stm, "%s\n", TermAsStr(h->value)) ;
		}
}

static Size ArraysAtomGCMarkAux(VoidPt x)
{
	ArrayPt a = cArrayPt(x) ;
	register AEntryPt h ;
	for( h = ArrayBegin(a) ; h < ArrayEnd(a) ; h++ )
		TermAtomGCMark(h->value) ;
	return 0 ;
}
static void ArraysAtomGCMark()
{
	ForEachExtra(arrayType, ArraysAtomGCMarkAux) ;
}


/* MAIN OPERATIONS */

Size ArraySize(ArrayPt a)
{
	return ArrayEnd(a) - ArrayBegin(a) ;
}

ArrayPt ArrayNew(void)
{
	ArrayPt a = ExtraNew(arrayType) ;
	ArrayInit(a, 4) ;
	return a ;
}

void ArrayClear(ArrayPt a)
{
	register AEntryPt h ;
	for( h = ArrayBegin(a) ; h < ArrayEnd(a) ; h++ ) {
		ReleaseTerm(h->value) ;
		h->value = nil ;
	}
}

void ArrayDelete(ArrayPt a)
{
	ArrayClear(a) ;
	Release(ArrayBegin(a)) ;
	ExtraDelete(arrayType, a) ;
}

Bool ArrayGet(ArrayPt a, PInt idx, Pt *t)
{
	if( idx < ArrayEnd(a) - ArrayBegin(a) && ArrayBegin(a)[idx].value != nil ) {
		*t = ArrayBegin(a)[idx].value ;
		return true ;
	}
	else return false ;
}

void ArraySet(ArrayPt a, PInt idx, Pt t)
{
	if( idx >= ArrayEnd(a) - ArrayBegin(a) )
		ArrayExpand(a, idx) ;
	ReleaseTerm(ArrayBegin(a)[idx].value) ;
	ArrayBegin(a)[idx].value = AllocateTermForAssign(t) ;
}

Bool ArrayDeleteItem(ArrayPt a, PInt idx)
{
	if( idx < ArrayEnd(a) - ArrayBegin(a) && ArrayBegin(a)[idx].value != nil ) {
		ReleaseTerm(ArrayBegin(a)[idx].value) ;
		ArrayBegin(a)[idx].value = nil ;
		return true ;
	}
	return false ;
}


/* CXPROLOG C'BUILTINS */

static void PArrayCheck()
{
	MustBe( XExtraCheck(arrayType, X0) )
}

static void PArrayNew()
{
	BindVarWithExtra(X0, ArrayNew()) ;
	JumpNext()
}

static void PArrayClear()
{
	ArrayClear(XTestExtra(arrayType,X0)) ;
	JumpNext()
}

static void PArrayDelete()
{
	ArrayDelete(XTestExtra(arrayType,X0)) ;
	JumpNext()
}

static void PArraySet()
{
	ArraySet(XTestExtra(arrayType,X0), XTestPosInt(X1)-1, XTestNonVar(X2)) ;
	JumpNext()
}

static void PArrayGet()
{
	Pt t ;
	Ensure( ArrayGet(XTestExtra(arrayType,X0), XTestPosInt(X1)-1, &t) )
	t = ZPushTerm(t) ; /* stacks may grow */
	MustBe( Unify(X2, t) )
}

static void PArrayDeleteItem()
{
	MustBe( ArrayDeleteItem(XTestExtra(arrayType,X0), XTestPosInt(X1)-1) )
}

static void PArrayAsList()
{
	ArrayPt a = XTestExtra(arrayType,X0) ;
	AEntryPt h ; Pt t ;
	termSegm[0] = cPt(hifenFunctor) ;	
	Z = tNilAtom ;
	for( h = ArrayEnd(a) - 1 ; h >= ArrayBegin(a) ; h-- )
		if( h->value != nil ) {
			termSegm[1] = MakeInt(h - ArrayBegin(a) + 1) ;
			termSegm[2] = h->value ;
			t = ZPushTerm(TagStruct(termSegm)) ; /* stacks may grow */
			Z = MakeList(t, Z) ;
		}
	MustBe( Unify(Z, X1) )
}

static void PArrayWrite()
{
	ArrayWrite(currOut, XTestExtra(arrayType,X0)) ;
	JumpNext()
}

static void PSArrayWrite()
{
	ArrayWrite(XTestStream(X0, mWrite), XTestExtra(arrayType,X1)) ;
	JumpNext()
}

static void PNDCurrentArray()
{
	PNDCurrentExtra(arrayType, nil, 1) ;
	JumpNext()
}

static Size ArraysAux(VoidPt x)
{
	ArrayPt a = cArrayPt(x) ;
	AtomPt at = IVarWith(TagExtra(a)) ;
	Write("  %16s -> size = %ld, capacity = %ld", 
				TermAsStr(TagExtra(a)),
				ArraySize(a),
				ArrayCapacity(a)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
	return 0 ;
}
static void PArrays()
{
	ShowVersion() ;
	Write("Arrays:\n") ;
	ForEachExtra(arrayType, ArraysAux) ;
	JumpNext()
}


/* TEST, EXTRACT & INIT */

Bool IsArray(Pt t)
{
	return IsThisExtra(arrayType, t) ;
}

ArrayPt XTestArray(Pt t)
{
	return XTestExtra(arrayType, t) ;
}

void ArraysInit()
{
	arrayType = ExtraTypeNew("ARRAY", WordsOf(Array)) ;
	InstallAtomGCHandler(ArraysAtomGCMark) ;
	/* add "arrays." to CxProlog.c/PShow */

	InstallCBuiltinPred("array", 1, PArrayCheck) ;
	InstallCBuiltinPred("array_new", 1, PArrayNew) ;
	InstallCBuiltinPred("array_clear", 1, PArrayClear) ;
	InstallCBuiltinPred("array_delete", 1, PArrayDelete) ;
	InstallCBuiltinPred("array_set", 3, PArraySet) ;
	InstallCBuiltinPred("array_get", 3, PArrayGet) ;
	InstallCBuiltinPred("array_delete_item", 2, PArrayDeleteItem) ;
	InstallCBuiltinPred("array_as_list", 2, PArrayAsList) ;
	InstallCBuiltinPred("array_write", 1, PArrayWrite) ;
	InstallCBuiltinPred("array_write", 2, PSArrayWrite) ;
	InstallNDeterCBuiltinPred("current_array", 1, PNDCurrentArray) ;
	InstallCBuiltinPred("arrays", 0, PArrays) ;
}
