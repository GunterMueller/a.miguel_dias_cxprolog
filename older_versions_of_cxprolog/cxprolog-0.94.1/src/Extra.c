/*
 *   This file is part of the CxProlog system

 *   Extra.c
 *   by A.Miguel Dias - 2002/01/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define ExtraTypeTag(e)				((e)->tag)
#define ExtraTypeName(e)			((e)->name)
#define ExtraTypeSizeFun(e)			((e)->sizeFun)
#define ExtraTypeGCMarkFun(e)		((e)->gcMarkFun)
#define ExtraTypeGCDeleteFun(e)		((e)->gcDeleteFun)
#define ExtraTypeHashTable(e)		((e)->hashTable)
#define ExtraTypeHashTableSize(e)	((e)->hashTableSize)
#define ExtraTypeHashTableSlot(e,s)	(*(ExtraPt *)(&ExtraTypeHashTable(e)[s]))
#define ExtraTypeOverrideTag(e)		((e)->overrideTag)

#define ExtraIsDisabled(x)			(cExtraPt(x)->disabled)
#define ExtraIsHidden(x)			(cExtraPt(x)->hidden)
#define ExtraIsPermanent(x)			(cExtraPt(x)->permanent)
#define ExtraIsGCMarked(x)			(cExtraPt(x)->gcMarked)
#define ExtraNext(x)				(cExtraPt(x)->next)

#define XExtraIsHidden(x)			ExtraIsHidden(XExtra(t))

/* EXTRA-TYPES TABLE */

#define maxExtraTypes				12

static ExtraType allExtraTypes[maxExtraTypes] ;
static int nExtraTypes = 0 ;

ExtraTypePt ExtraTypeNew(CharPt name, ExtraFun sizeFun,
							FunV gcMarkFun, BFunV gcDeleteFun, Size htSize)
{
#if 0
	Mesg("%d %s", nExtraTypes, name) ;
#endif
	if( nExtraTypes < maxExtraTypes ) {
		register ExtraTypePt e = allExtraTypes + nExtraTypes ;
		register int i ;
		ExtraTypeTag(e) = nExtraTypes ;
		ExtraTypeName(e) = name ;
		ExtraTypeSizeFun(e) = sizeFun ;
		ExtraTypeGCMarkFun(e) = gcMarkFun ;
		ExtraTypeGCDeleteFun(e) = gcDeleteFun ;
		ExtraTypeHashTable(e) = Allocate(htSize, false) ;
		dotimes(i, htSize)
			ExtraTypeHashTable(e)[i] = nil ;
		ExtraTypeHashTableSize(e) = htSize ;
		nExtraTypes++ ;
		return e ;
	}
	return InternalError("ExtraTypeNew") ;
}


/* EXTRA-TERMS MANAGEMENT */

#define doextra_slot(e, s, x)									\
		doseq(x, ExtraTypeHashTableSlot(e,s), ExtraNext(x))

#define doextra(e, i, x)										\
	dotimes(i, ExtraTypeHashTableSize(e))						\
		doextra_slot(e, i, x)

#define doextra2(e, i, z, x)									\
	dotimes(i, ExtraTypeHashTableSize(e))						\
		for( z = &ExtraTypeHashTableSlot(e,i) ; (x = *z) != nil ; )

static Bool ExtraCheck(ExtraTypePt e, VoidPt ref)
{
	int i ;
	register ExtraPt x ;
	doextra(e, i, x)
		if( x == ref )
			return true ;
	return false ;
}

Bool IsThisExtra(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	return IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) && !XExtraIsHidden(e) ;
}

VoidPt ExtraNewWithSize(ExtraTypePt e, Size size, int slot)
{
	ExtraPt x = Allocate(size, false) ;
	ExtraPt *z = (ExtraPt *)(&ExtraTypeHashTable(e)[slot]) ;
/* Link in the apropriate slot */
	x->next = *z ;
	*z = x ;
/* Initialize */
	ExtraTag(x) = ExtraTypeTag(e) ;
	ExtraIsDisabled(x) = false ;
	ExtraIsHidden(x) = false ;
	ExtraIsPermanent(x) = false ;
	ExtraGCAddDelta(size) ;
	return x ;
}

VoidPt ExtraNew(ExtraTypePt e, int slot)
{
	return ExtraNewWithSize(e, ExtraTypeSizeFun(e)(nil), slot) ;
}

static void ExtraRelease(ExtraTypePt e, VoidPt x)
{
#if 0
	Mesg("%lx released %s '%s'", x, ExtraAsStr(x),
			IsAtom(TagAtom(x)) ? XAtomName(TagAtom(x)) :
			IsFloat(TagFloat(x)) ? XNumberAsStr(TagFloat(x)) : "") ;
#endif	
	Release(x, ExtraTypeSizeFun(e)(x)) ;
}

Bool ExtraIsAlive(VoidPt x)
{
	return !ExtraIsDisabled(x) ;
}

void ExtraSetDisabled(VoidPt x)
{
	ExtraIsDisabled(x) = true ;
	ExtraIsHidden(x) = true ;
}

void ExtraSetHidden(VoidPt x)
{	/* An hidden extra is not seen by Prolog. Such an extra is intended
       to be used only in C-code or it can be a disabled extra. */
	ExtraIsHidden(x) = true ;
}

void ExtraSetPermanent(VoidPt x)
{
	ExtraIsPermanent(x) = true ;
}

CharPt ExtraAsStr(VoidPt x) 
{
	ExtraTypePt e = allExtraTypes + ExtraTag(x) ;
	return GStrFormat("1'%s_%lx", ExtraTypeName(e), cWord(x)) ;
}

Size ExtraForEach(ExtraTypePt e, ExtraFun fun)
{		/* Apply to the non-disabled */
	if( e == nil ) {	/* Do it for all extra types */
		int i ;
		Size n = 0 ;
		dotimes(i, nExtraTypes)
			n += ExtraForEach(allExtraTypes + i, fun) ;
		return n ;
	}
	else {				/* Do it only for the extra type e */
		int i ;
		Size n = 0 ;
		register ExtraPt x ;
		doextra(e, i, x)
			if( !ExtraIsDisabled(x) )
				n += fun(x) ;
		return n ;
	}
}

VoidPt ExtraFindFirst(ExtraTypePt e, int slot, ExtraCond cond, VoidPt arg)
{		/* Apply to the non-disabled */
	if( slot == -1 ) {
		int i ;
		register ExtraPt x ;
		doextra(e, i, x)
			if( !ExtraIsDisabled(x) && cond(x, arg) )
				return x ;
	}
	else {
		register ExtraPt x ;
		doextra_slot(e, slot, x)
			if( !ExtraIsDisabled(x) && cond(x, arg) )
				return x ;
	}
	return nil ;
}

static VoidPt XTestExtraNoIVar(ExtraTypePt e, Pt t) ;

void ExtraPNDCurrent(ExtraTypePt e, BFunV bfun, int arity, int resPos)
{		/* Apply to the non-hidden. Immediate-update semantics. */
	int i ;
	ExtraPt x ;
	if( A(arity) == tNilAtom ) {
		Xc(resPos) = Drf(Xc(resPos)) ;
		if( IsVar(Xc(resPos)) ) {
			i = 0 ;
			x = ExtraTypeHashTableSlot(e, 0) ;
		}
		else {
			x = XTestExtraNoIVar(e, Xc(resPos)) ;
			Discard() ;
			MustBe( !ExtraIsHidden(x) && (bfun == nil || bfun(x)) ) ;
		}
	}
	else {
		i = XInt(A(arity)) ;
		x = ExtraNext(A(arity+1)) ;
	}
	for(;;) {
/* if x == nil then must find next non-nil */
		while( x == nil && i < ExtraTypeHashTableSize(e) - 1 )
			x = ExtraTypeHashTableSlot(e, ++i) ;
/* in case of unsuccess, stop generation */
		if( x == nil )
			Jump(DiscardAndFail) ;
/* found OK element */
		if( !ExtraIsHidden(x) && (bfun == nil || bfun(x)) )
			break ;
/* last seen is not OK, so advance and start all over again */
		x = ExtraNext(x) ;
	}
	A(arity) = MakeInt(i) ;
	A(arity+1) = cPt(x) ;
	MustBe( Unify(Xc(resPos), TagExtra(e, x)) ) ;
}

void ExtraShow(ExtraTypePt e, ExtraFun fun)
{		/* Apply to the non-hidden */
	Size n = 0, temps = 0, perms = 0 ;
	int tableSize = ExtraTypeHashTableSize(e) ;
	int i ;
	register ExtraPt x ;
	ShowVersion() ;

/* show as list */
	Write("%sS:\n", ExtraTypeName(e)) ;
	doextra(e, i, x)
		if( !ExtraIsHidden(x) ) {
			Write("  %16s %c> ",
				ExtraAsStr(x),
				ExtraIsPermanent(x) ? '=' : '-') ;
			if( fun != nil )
				n += fun(x) ;
			IVarsWithWrite(TagExtra(e, x)) ;
			Write("\n") ;
		}
	if( n == 0 ) Write("    %% None\n") ;

/* show as hash table sumary */
	if( ExtraTypeHashTableSize(e) == 1 ) JumpNext() ;
	Write("---\n%sS HASH-TABLE SUMMARY:\n", ExtraTypeName(e)) ;
	doextra(e, i, x)
		if( !ExtraIsHidden(x) ) {
			if( ExtraIsPermanent(x) ) perms++ ;
			else temps++ ;
		}
	Write("  %sS are stored in a %d-entry hash table:\n",
										ExtraTypeName(e), tableSize) ;
	Write("    Current number of permanent %sS in the hash table -> %7ld\n",
										ExtraTypeName(e), perms) ;
	Write("    Current number of temporary %sS in the hash table -> %7ld\n",
										ExtraTypeName(e), temps) ;
	Write("    Current average length of the hash chains ->           %.5f\n",
										(perms + temps)/(double)tableSize) ;
	Write("    Current length of the individual hash chains ->") ;
	dotimes(i, tableSize) {
		Size n = 0 ;
		doextra_slot(e, i, x)
			if( !ExtraIsHidden(x) ) n++ ;
		Write(" %d", n) ;
	}
	Write("\n") ;

#if 0
/* show as hash table details */
	Write("---\n%sS HASH-TABLE ENTRIES:\n", ExtraTypeName(e)) ;
	dotimes(i, ExtraTypeHashTableSize(e)) {
		Write("%d - ", i) ;
		doextra_slot(e, i, x)
			if( !ExtraIsHidden(x) )
				Write("%s%s ", ExtraIsPermanent(x) ? "=" : " ", ExtraAsStr(x)) ;
		Write("\n") ;
	}
#endif

	JumpNext() ;
}


/* BASIC GARBAGE COLLECTION */

#define deltaThreshold		(18 K)
#define maxGCHandlers		20

static CharPt GCHdlNames[maxGCHandlers] ;
static Fun GCHdl[maxGCHandlers] ;
static int nGCHdl = 0 ;
static Size gcDelta = 0 ;

void ExtraGCHandlerInstall(CharPt name, Fun p)
{
	if( nGCHdl == maxGCHandlers )
		InternalError("Too many GC handlers: increase the table capacity") ;
	GCHdlNames[nGCHdl] = name ;
	GCHdl[nGCHdl] = p ;
	nGCHdl++ ;;
#if 0
	Mesg("ExtraGCHandlerInstall -> %s", name) ;
#endif
}

void ExtraGCAddDelta(Size size)
{
	if( (gcDelta += size) >= deltaThreshold ) {
		Attention() = true ;
		gcDelta = deltaThreshold ;
	}
#if 0
	Mesg("gcDelta = %d  (%d)", gcDelta, size) ;
#endif
}

void ExtraGCMark(VoidPt x)
{
	if( !ExtraIsGCMarked(x) ) {
		ExtraTypePt e = allExtraTypes + ExtraTag(x) ;
#if 0
		Mesg("marking %s", ExtraAsStr(x)) ;
#endif
		ExtraIsGCMarked(x) = true ;
		if( ExtraTypeGCMarkFun(e) != nil && !ExtraIsDisabled(x) )
			ExtraTypeGCMarkFun(e)(x) ;
	}
}

void ExtraGCMarkRange(register Hdl a, register Hdl z)
{
	for( ; a < z ; a++ ) {
		if( IsEmpty(*a) )
			/* Skip */ ;
		elif( IsExtra(*a) )
			ExtraGCMark(XExtra(*a)) ;
	}
}

int ExtraGCClearNotMarked(ExtraTypePt e)
{		/* Apply to all extras, even to the disabled */	
	if( e == nil ) {	/* Do it for all extra types */
		Size n = 0 ;
		int i ;
		dotimes(i, nExtraTypes)
			n += ExtraGCClearNotMarked(allExtraTypes + i) ;
		return n ;
	}
	else {				/* Do it only for the extra type e */
		Size n = 0 ;
		if( ExtraTypeGCDeleteFun(e) != nil ) {
			int i ;
			register ExtraPt *z, x ;
			doextra2(e, i, z, x) {
				if( !ExtraIsGCMarked(x) && ExtraTypeGCDeleteFun(e)(x) ) {
					*z = ExtraNext(x) ;
					ExtraRelease(e, x) ;
					n++ ;
				}
				else z = &ExtraNext(x) ;
			}
		}
		if( n > 0 && !testGCollection_flag )
			MemoryWarning("%d temporary %s%s removed by the garbage collector",
										n, ExtraTypeName(e), n>1 ? "S" : "") ;
		return n ;
	}
}

static void ExtraGCDoIt()
{
	int i, j ;
	register ExtraPt x ;
/* Unmark all */
	dotimes(i, nExtraTypes)
		doextra(allExtraTypes + i, j, x)
			ExtraIsGCMarked(x) = false ;
/* Mark roots */
	dotimes(i, nGCHdl)
		GCHdl[i]() ;
/* Mark permanent extras */
	dotimes(i, nExtraTypes) {
		doextra(allExtraTypes + i, j, x)
			if( ExtraIsPermanent(x) )
				ExtraGCMark(x) ;
	}
/* Clear extras not marked */
	ExtraGCClearNotMarked(nil) ;
/* Reset gcDelta */
	gcDelta = 0 ;
}

void ExtraGC()
{
	if( testGCollection_flag ) {
		Attention() = true ;	/* Force gc again and again */
		ExtraGCDoIt() ;	
	}
	elif( gCollection_flag && gcDelta >= deltaThreshold )
		ExtraGCDoIt() ;
}

/* PT OPERATIONS */

VoidPt XTestExtra(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) )
		t = IVarGet(XAtom(t), true) ;
	if( IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) ) {
		if( XExtraIsHidden(t) )
			ImperativeError("%s object does not exist", XExtraAsStr(t)) ;
		return XExtra(t) ;
	}
	return ExtraTypeError(e, nil, t) ;
}

static VoidPt XTestExtraNoIVar(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	if( IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) ) {
		if( XExtraIsHidden(t) )
			ImperativeError("%s object does not exist", XExtraAsStr(t)) ;
		return XExtra(t) ;
	}
	return ExtraTypeError(e, nil, t) ;
}

Bool XExtraCheck(ExtraTypePt e, register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) )
		t = IVarGet(XAtom(t), true) ;
	return IsExtra(t) && XExtraTag(t) == ExtraTypeTag(e) && !XExtraIsHidden(t) ;
}

CharPt XExtraTypeName(Pt t) 
{
	t = Drf(t) ;
	if( IsExtra(t) ) {
		ExtraTypePt e = allExtraTypes + XExtraTag(t) ;
		return ExtraTypeName(e) ;
	}
	return InternalError("XExtraTypeName") ;
}

CharPt XExtraAsStr(Pt t) 
{
	return ExtraAsStr(XExtra(Drf(t))) ;
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
			sref = s + 1 ;
			break ;
		}
	if( sref == nil ) return nil ;
	ref = cPt(strtoul(sref, nil, 16)) ;

/* Get type decriptor from name */
	sref[-1] = '\0' ;
	dotimes(i, nExtraTypes)
		if( StrEqual(allExtraTypes[i].name, name) ) {
			e = allExtraTypes + i ;
			break ;
		}
	sref[-1] = '_' ;

/* Final tests */
	if( e == nil )
		return nil ;
	if( !ExtraCheck(e, ref) || ExtraIsHidden(ref) )
		return tNilAtom ;
	return TagExtra(e, ref) ;
}

void BindVarWithExtra(register Pt t, VoidPt x)
{
	VarValue(t) ;
	if( IsVar(t) ) {
		if( !UnifyWithAtomic(t, TagExtra(allExtraTypes + ExtraTag(x), x)) )
			InternalError("BindVarWithExtra") ;
	}
	elif( IsAtom(t) )
		IVarSet(XAtom(t), TagExtra(allExtraTypes + ExtraTag(x), x), false) ;
	else TypeError("VAR or IVAR", t) ;
}

VoidPt ExtraTypeError(ExtraTypePt e, CharPt alt, Pt found)
{
	if( alt == nil ) alt = "IVAR" ;
	return TypeError(GStrFormat("%s or %s", ExtraTypeName(e), alt), found) ;
}

void ExtraInit()
{
}

/* CXPROLOG C'BUILTINS */

static void PBasicGC()
{
	ExtraGCDoIt() ;
	JumpNext() ;
}

void ExtraInit2()
{
	InstallCBuiltinPred("basic_gc", 0, PBasicGC) ;
}
