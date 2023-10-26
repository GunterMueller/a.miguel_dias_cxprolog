/*
 *   This file is part of the CxProlog system

 *   Term.c
 *   by A.Miguel Dias - 1989/11/14
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


/* GLOBAL STACK CONTROL */

#define PushHVar()			( ResetVar(H), cPt(H++) )
#define PushH(v)			Push(H, v) 
#define PopH()				Pop(H)
#define GrowH(n)			Grow(H, n)


/* EMPTY */

void EmptyRange(register Hdl a, register Hdl z)
{
	while( a < z )
		*a++ = EmptyCell ;
}

void EmptyRangeN(register Hdl a, register Size n)
{
	while( n-- )
		*a++ = EmptyCell ;
}

void CopyUntilEmpty(register Hdl z, register Hdl a)
{
	while( !IsEmpty(*z++ = *a++) ) ;
}

Hdl FindEmpty(register Hdl a)
{
	while( !IsEmpty(*a++) ) ;
	return a - 1 ;
}

VoidPt AllocateSegmentEmpty(Size nWords, VoidPt end)
{
	VoidPt mem = AllocateSegment(nWords, end) ;
	EmptyRangeN(mem, nWords) ;
	return mem ;
}


/* VARS */

static Pt freeVar ; /* free var outside stacks */
static Pt limA, limB ;

Pt Drf(register Pt t)
{
	VarValue(t) ;
	return t ;
}

void PrepareDrfChecked(register Pt term)
{
	VarValue(term) ;
	if( IsAllocCompound(term) ) {
		limA = XPt(term) ;
		limB = limA + MemBlockSize(limA) ;
	}
	else {
		limA = cPt(stacksBegin) ;
		limB = cPt(stacksEnd) ;
	}
}

Pt DrfChecked(register Pt t)
{
	while( IsVar(t) && Le(limA,t) && Le(t,limB) && IsLink(t) )
		DrfVar(t) ;
	if( IsVar(t) && !(Le(limA,t) && Le(t,limB)) )
		t = Eq(t,freeVar) ? freeVar : tBadTermAtom ;
	return t ;
}

Pt MakeVar()
{
	CheckFreeSpaceOnStacks(1) ;
	return PushHVar() ;
}

CharPt VarName(Pt t)
{
	if( IsVar(t) ) {
		if( IsLocalRef(t) )
			return GStrFormat("_L%d", Df(stacksEnd, t) ) ;
		elif( IsGlobalRef(t) )
			return GStrFormat("_G%d", Df(t, stacksBegin) ) ;
		elif( Eq(t,freeVar) )
			return GStrFormat("_0") ;
		else /* is in alloc term */
			return GStrFormat("_0%d", Df(t,limA)) ;
	}
	else return InternalError("VarName") ;
}

Bool IsVarName(CharPt s)
{
	return( s[0] == '_' || InRange(s[0],'A','Z') ) ;
}


/* STRUCTS */

Pt MakeStruct(FunctorPt functor, Hdl args) /* pre: args in stacks */
{
	if( functor == listFunctor ) {
		CheckFreeSpaceOnStacks(2) ;
		PushH(args[0]) ;
		PushH(args[1]) ;
		return TagList(H - 2) ;
	}
	elif( FunctorArity(functor) == 0 )
		return TagAtom(FunctorAtom(functor)) ;
	else {
		register int i, arity = FunctorArity(functor) ;
		CheckFreeSpaceOnStacks(arity + 1) ;
		PushH(functor) ;
		dotimes(i, arity)
			PushH(args[i]) ;
		return TagStruct(H - arity - 1) ;
	}
}

Pt MakeCleanStruct(FunctorPt functor)
{
	if( functor == listFunctor ) {
		CheckFreeSpaceOnStacks(2) ;
		Ignore(PushHVar()) ;
		Ignore(PushHVar()) ;
		return TagList(H - 2) ;
	}
	elif( FunctorArity(functor) == 0 )
		return TagAtom(FunctorAtom(functor)) ;
	else {
		register int i, arity = FunctorArity(functor) ;
		CheckFreeSpaceOnStacks(arity + 1) ;
		PushH(functor) ;
		dotimes(i, arity)
			Ignore(PushHVar()) ;
		return TagStruct(H - arity - 1) ;
	}
}

Pt MakeUnStruct(FunctorPt functor, Pt arg) /* pre: arg in stacks */
{
	CheckFreeSpaceOnStacks(2) ;
	PushH(functor) ;
	PushH(arg) ;
	return TagStruct(H - 2) ;
}

Pt MakeBinStruct(FunctorPt functor, Pt arg0, Pt arg1) /* pre: args in stacks */
{
	if( functor == listFunctor )
			return MakeList(arg0, arg1) ;

	CheckFreeSpaceOnStacks(3) ;
	PushH(functor) ;
	PushH(arg0) ;
	PushH(arg1) ;
	return TagStruct(H - 3) ;
}

Pt MakeSlashTerm(FunctorPt f)
{
	return MakeBinStruct(slashFunctor,
					TagAtom(FunctorAtom(f)),
					MakeInt(FunctorArity(f))) ;
}

Pt MakeList(Pt head, Pt tail) /* pre: args in stacks */
{
	CheckFreeSpaceOnStacks(2) ;
	PushH(head) ;
	PushH(tail) ;
	return TagList(H - 2) ;
}

Pt MakeTriStruct(FunctorPt functor, Pt arg0, Pt arg1, Pt arg2) /* pre: args in stacks */
{
	CheckFreeSpaceOnStacks(4) ;
	PushH(functor) ;
	PushH(arg0) ;
	PushH(arg1) ;
	PushH(arg2) ;
	return TagStruct(H - 4) ;
}

CharPt XStructNameArity(Pt t)
{
	return GStrFormat("%s/%d", XStructName(t), XStructArity(t)) ;
}

void SplitNeckTerm(Pt c, Hdl parts)
{
	c = Drf(c) ;
	if( IsThisStruct(c, neckFunctor) ) {
		parts[0] = Drf(XStructArg(c,0)) ;
		parts[1] = Drf(XStructArg(c,1)) ;
	}
	else {
		parts[0] = c ;
		parts[1] = tTrueAtom ;
	}
}

int XUnitParam(register Pt t)
{
	int i ;
	t = Drf(t) ;
	if( !IsUnitParam(t) )
		TypeError("UNIT-PARAMETER", nil) ;
	i = XTestInt(XStructArg(t, 0)) ;
	if( !InRange(i,1,UnitArity(CurrUnit())) )
		TypeError("UNIT-PARAMETER", nil) ;
	return i-1 ;
}


/* LISTS */

Pt ArrayToOpenList(register Hdl array, register Size n) /* pre: n > 0 */
{
	register Pt list ;	
	CheckFreeSpaceOnStacks(2 * n) ;
	list = array[ --n ] ;	
	while( n-- ) {
		PushH(array[ n ]) ;
		PushH(list) ;
		list = TagList(H - 2) ;
	}
	return list ;
}

Pt ArrayToList(register Hdl array, register Size n)
{
	register Pt list ;
	CheckFreeSpaceOnStacks(2 * n) ;
	list = tNilAtom ;
	while( n-- ) {
		PushH(array[ n ]) ;
		PushH(list) ;
		list = TagList(H - 2) ;
	}
	return list ;
}

Pt ArrayToListRev(register Hdl array, register Size n)
{
	register Pt list ;
	CheckFreeSpaceOnStacks(2 * n) ;
	list = tNilAtom ;
	while( n-- ) {
		PushH(*array++) ;
		PushH(list) ;
		list = TagList(H - 2) ;
	}
	return list ;
}

Hdl ListToArray(Pt list, Size *len)
{
	UseScratch() ;
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) )
		ScratchPush(Drf(XListHead(list))) ;
	if( list != tNilAtom )
		TypeError("PROPERLY-TERMINATED-LIST", nil) ;
	*len = ScratchUsed() ;
	return VFreeScratch() ;
}

Size ListLength(register Pt list)
{
	Size n = 0 ;
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) )
		n++ ;
	if( list != tNilAtom )
		TypeError("PROPERLY-TERMINATED-LIST", nil) ;
	return n ;
}

Bool ListCheck(register Pt list)
{
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) ) ;
	return list == tNilAtom ;
}

static Bool Belongs(Pt t, register Pt list)	/* pre: t already deref */
{
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) )
		if( t == Drf(XListHead(list)) ) return true ;
	if( list != tNilAtom )
		TypeError("PROPERLY-TERMINATED-LIST", nil) ;
	return false ;
}


/* PSTRINGS */

Pt StringToPString(CharPt s)
{
	Pt list = tNilAtom ;
	Hdl h = &list + 1 ;
	while( *s ) {
		h[-1] = MakeList(MakeCode(CharDecode(s)), tNilAtom) ;
		h = H ;
	}
	return list ;
}

static CharPt PStringToString(Pt l)
{
	BigStrOpen() ;
	for( l = Drf(l) ; IsList(l) ; l = Drf(XListTail(l)) )
		BigStrAddChar(XTestCode(XListHead(l))) ;
	if( l != tNilAtom )
		TypeError("PROPERLY-TERMINATED-LIST", nil) ;
	return BigStrClose() ;
}

			
/* ASTRINGS */

Pt StringToAString(CharPt s)
{
	Pt list = tNilAtom ;
	Hdl h = &list + 1 ;
	while( *s ) {
		h[-1] = MakeList(MakeChar(CharDecode(s)), tNilAtom) ;
		h = H ;
	}
	return list ;
}

static CharPt AStringToString(Pt l)
{
	BigStrOpen() ;
	for( l = Drf(l) ; IsList(l) ; l = Drf(XListTail(l)) )
		BigStrAddChar(XTestChar(XListHead(l))) ;
	if( l != tNilAtom )
		TypeError("PROPERLY-TERMINATED-LIST", nil) ;
	return BigStrClose() ;
}


/* TERMS */

Hdl termSegm ;  /* Small buffer used to build terms */

CharPt TermTypeStr(Pt t)
{
	t = Drf(t) ;
	return
		t == nil	? "NIL-POINTER" :
		IsVar(t)	? "VAR" :
		IsStruct(t)	? "STRUCT" :
		IsList(t)	? "LIST" :
		IsAtom(t)	? "ATOM" :
		IsInt(t)	? "INT" :
		IsFloat(t)	? "FLOAT" :
		IsExtra(t)	? (char *)XExtraTypeName(t) :
					  "UNKNOWN"
	;
}

void TermBasicGCMark(register Pt t)
{
	if( t == nil ) return ;
	UseScratch() ;
	for(;;) {
		VarValue(t) ;
		if( IsExtra(t) )
			ExtraGCMark(XExtra(t)) ;
		elif( IsStruct(t) ) {
			register int arity = XStructArity(t) ;
			register Hdl args = XStructArgs(t) ;
			t = *args ;
			while( --arity ) ScratchPush(*++args) ;
			continue ;
		}
		elif( IsList(t) ) {
			ScratchPush(XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		if( ScratchUsed() == 0 ) break ;
		else t = ScratchPop() ;
	}
	FreeScratch() ;
}


/* TermSize is called each time a term is about to be asserted,
	assigned ou copied */

Size TermSize(register Pt t)
{
	register Size size = 0 ;
	UseScratch() ;
	for(;;) {
		VarValue(t) ;
		if( IsStruct(t) ) {
			register int arity = XStructArity(t) ;
			register Hdl args = XStructArgs(t) ;
			if( (size += arity + 1) > maxTermSize ) /* avoids infinite loop */
				DatabaseError("Term too large") ;
			t = *args ;
			while( --arity ) ScratchPush(*++args) ;
			continue ;
		}
		elif( IsList(t) ) {
			if( (size += 2) > maxTermSize ) /* avoids infinite loop */
				DatabaseError("Term too large") ;
			ScratchPush(XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		if( ScratchUsed() == 0 ) break ;
		else t = ScratchPop() ;
	}
	FreeScratch() ;
	return size ;
}

static Pt CopyTerm(register Pt t, register Hdl to, Pt env, Bool asserting, Bool convUnitParams)
{
	Pt res ;
	register Hdl where = &res ;	
	Hdl bottom = to ;
	TrailAllVarsStart() ; /* trick: trail is instrumental to refreshing vars */
	UseScratch() ;
	for(;;) {
		VarValue(t) ;
		if( IsStruct(t) ) {
			if( convUnitParams && IsUnitParam(t) ) {
				t = Z(OutParam(XUnitParam(t))) ;
				continue ;
			}
			else {
				FunctorPt f = XStructFunctor(t) ;
				int arity = FunctorArity(f) ;
				register Hdl args = XStructArgs(t) ;
				*where = TagStruct(to) ;
				Push(to, f) ;
				where = to++ ;
				t = *args ;
				while( --arity ) {
					ScratchPush(to) ;
					Push(to, *++args) ;
				}
				continue ;
			}
		}
		elif( IsList(t) ) {
			*where = TagList(to) ;
			where = to++ ;
			ScratchPush(to) ;
			Push(to, XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		elif( IsVar(t) ) {
			if( Le(bottom,t) && Lt(t,to) ) /* var already refreshed */
				*where = t ;
			elif( env == nil || Belongs(t, env) ) { /* var to refresh */
				ResetVar(where) ;
				Assign(t, where) ;
			}
			else *where = t ; /* var not to refresh */
		}
		else
			*where = t ;

		if( asserting ) {
			if( IsExtra(t) )
				ExtraSetPermanent(XExtra(t)) ;
		}

		if( ScratchUsed() == 0 ) break ;
		else {
			where = cHdl(ScratchPop()) ;
			t = *where ;
		}	
	}
	FreeScratch() ;
	TrailAllVarsRestore() ;
	return res ;
}

Bool HasFreeVars(register Pt t)
{
	UseScratch() ;
	for(;;) {
		VarValue(t) ;
		if( IsVar(t) ) {
			FreeScratch() ;
			return true ;
		}
		elif( IsStruct(t) ) {
			register int arity = XStructArity(t) ;
			register Hdl args = XStructArgs(t) ;
			t = *args ;
			while( --arity ) ScratchPush(*++args) ;
			continue ;
		}
		elif( IsList(t) ) {
			ScratchPush(XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		if( ScratchUsed() == 0 ) break ;
		else t = ScratchPop() ;
	}
	FreeScratch() ;
	return false ;
}

static Pt GetFreeVars(register Pt t, Size *nVars)
{
	Pt list = tNilAtom ;
	if( nVars != nil ) *nVars = 0 ;
	TrailAllVarsStart() ; /* trick: trail is instrumental in not repeating vars */
	UseScratch() ;
	for(;;) {
		VarValue(t) ;
		if( IsVar(t) ) {
			if( nVars != nil )
				(*nVars)++ ;
			else
				list = MakeList(t, list) ;
			Assign(t, tNilAtom) ; /* mark var as "already seen var" */
		}
		elif( IsStruct(t) ) {
			register int arity = XStructArity(t) ;
			register Hdl args = XStructArgs(t) ;
			t = *args ;
			while( --arity ) ScratchPush(*++args) ;
			continue ;
		}
		elif( IsList(t) ) {
			ScratchPush(XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		if( ScratchUsed() == 0 ) break ;
		else t = ScratchPop() ;
	}
	FreeScratch() ;
	TrailAllVarsRestore() ;
	return list ;
}

static Bool SubTerm(register Pt s, register Pt t)
{
	Bool res ;
	Hdl trailLevel = TR ; /* Cannot use Scratch here because it is not reentrant */
	for(;;) {
		VarValue(t) ;
		if( Equal(t, s) ) {
			res = true ;
			break ;
		}
		if( IsStruct(t) ) {
			register int arity = XStructArity(t) ;
			register Hdl args = XStructArgs(t) ;
			t = *args ;
			while( --arity ) PushTrail(*++args) ;
			continue ;
		}
		elif( IsList(t) ) {
			PushTrail(XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		if( TR == trailLevel ) {
			res = false ;
			break ;
		}
		else t = Pop(TR) ;
	}
	TR = trailLevel ;
	return res ;
}

Pt AllocateTermForAssert(register Pt t)
{
	VarValue(t) ;
	if( IsCompound(t) )
		return CopyTerm(t, Allocate(TermSize(t), true), nil, true, false) ;
	elif( IsExtra(t) ) {
		ExtraSetPermanent(XExtra(t)) ;
		return t ;
	}
	else
		return t ;
}
		
Pt AllocateTermForAssign(register Pt t)
{
	VarValue(t) ;
	if( IsCompound(t) )
		return CopyTerm(t, Allocate(TermSize(t), true), nil, false, false) ;
	elif( IsVar(t) )
		return freeVar ;
	else
		return t ;
}

void ReleaseTerm(register Pt t)
{
	if( t == nil ) return ;
	VarValue(t) ;
	if( IsCompound(t) )
		Release(XPt(t), -1) ;
}

static Pt ZPushTermWithEnv(Pt env, register Pt t, Bool convUnitParams)
{
	VarValue(t) ;
	if( IsCompound(t) ) {
        extern Pt ZT ;
		Size size = TermSize(t) ;
		ZT = t ; /* save on ZT because of possible relocation */
		ZEnsureFreeSpaceOnStacks(size, "ZPushTermWithEnv/Compound") ;
		GrowH(size) ;
		return CopyTerm(ZT, H - size, env, false, convUnitParams) ;
	}
	elif( IsVar(t) ) {
		ZEnsureFreeSpaceOnStacks(1, "ZPushTermWithEnv/Var") ;
		if( env == nil || Belongs(t, env) )
			return PushHVar() ;
		else return t ;
	}
	else
		return t ;
}

Pt ZPushTerm(Pt t)
{
	return ZPushTermWithEnv(nil, t, false) ;
}

Pt ZPushTerm_ConvUnitParams(Pt t)
{
	return ZPushTermWithEnv(nil, t, true) ;
}


/* NUMBER VARS */

static Size numberVarsN ;

static void DoNumberVars(register Pt t)
{
	VarValue(t) ;
	if( IsAtomic(t) ) ;
	elif( IsVar(t) ) {
		Assign(t, MakeUnStruct(varFunctor, MakeInt(numberVarsN++))) ;
	}
	elif( IsList(t) ) {
		DoNumberVars(XListHead(t)) ;
		DoNumberVars(XListTail(t)) ;
	}
	elif( IsStruct(t) ) {
		int i, n = XStructArity(t) ;
		dotimes(i, n)
			DoNumberVars(XStructArg(t, i)) ;
	}
	else InternalError("DoNumberVars") ;
}

static Size NumberVars(Pt t, Size start)
{
	numberVarsN = start ;
	DoNumberVars(t) ;
	return( numberVarsN ) ;
}



/* TYPE ERRORS */

Pt TestAtomic(register Pt t)
{
	VarValue(t) ;
	if( IsAtomic(t) ) return t ;
	return TypeError("ATOMIC", t) ;
}

Pt TestAtom(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return t ;
	return TypeError("ATOM", t) ;
}

Pt TestList(register Pt t)
{
	VarValue(t) ;
	if( IsList(t) ) return t ;
	return TypeError("LIST", t) ;
}

AtomPt XTestAtom(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtom(t) ;
	return TypeError("ATOM", t) ;
}

CharPt XTestAtomName(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtomName(t) ;
	return TypeError("ATOM", t) ;
}

CharPt XTestFileName(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtomName(t) ;
	return TypeError("FILENAME", t) ;
}

PInt XTestInt(register Pt t)
{
	VarValue(t) ;
	if( IsInt(t) ) return XInt(t) ;
	return ITypeError("INT", t) ;
}

Pt XTestIntOrVar(register Pt t)
{
	VarValue(t) ;
	if( IsVar(t) || IsInt(t) ) return t ;
	return TypeError("INT or VAR", t) ;
}

LLInt XTestLLInt(register Pt t)  /* used in the java interface */
{
	VarValue(t) ;
	if( IsNumber(t) ) return XAsLLInt(t) ;
	return ITypeError("NUMBER", t) ;
}

PInt XTestPosInt(register Pt t)
{
	VarValue(t) ;
	if( IsPos(t) ) return XInt(t) ;
	return ITypeError("POSITIVE-INT", t) ;
}

PInt XTestNat(register Pt t)
{
	VarValue(t) ;
	if( IsNat(t) ) return XInt(t) ;
	return ITypeError("NATURAL-NUMBER", t) ;
}

PInt XTestCode(register Pt t)
{
	VarValue(t) ;
	if( IsCode(t) ) return XCode(t) ;
	return ITypeError("CODE", t) ;
}

PInt XTestByte(register Pt t)
{
	VarValue(t) ;
	if( IsByte(t) ) return XByte(t) ;
	return ITypeError("BYTE", t) ;
}

PInt XTestChar(Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) {
		int c = XChar(t) ;
		if( c != -1 ) return c ;
	}
	return ITypeError("CHAR", t) ;
}

PInt XTestCharOrCode(Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) {
		int c = XChar(t) ;
		if( c != -1 ) return c ;
	}
	elif( IsCode(t) )
		return XCode(t) ;
	return ITypeError("CHAR or CODE", t) ;
}

PInt XTestIntRange(register Pt t, int a, int z)
{
	CharPt s ;
	VarValue(t) ;
	if( IsInt(t) && InRange(XInt(t), a, z) ) return XInt(t) ;
	s = GStrFormat("RANGE(%d..%d)", a, z) ;
	return ITypeError(s, t) ;
}

PFloat XTestFloat(register Pt t)
{
	VarValue(t) ;
	if( IsInt(t) ) return XInt(t) ;
	if( IsFloat(t) ) return XFloat(t) ;
	return ITypeError("NUMBER", t) ;
}

Bool XTestBool(register Pt t)
{
	VarValue(t) ;
	if( t == tTrueAtom ) return true ;
	if( t == tFalseAtom ) return false ;
	return ITypeError("BOOL", t) ;
}

Bool XTestOnOff(register Pt t)
{
	VarValue(t) ;
	if( t == tOnAtom ) return true ;
	if( t == tOffAtom ) return false ;
	return ITypeError("'on/off'", t) ;
}

int XTestAtomAlt(Pt t, ...)
{
	int i ;
	va_list va ;
	va_start(va, t) ;
	i = StrSeqIdxV(XTestAtomName(t), va) ;
	va_end(va) ;
	if( i != -1 ) return i ;
	return ITypeError(StrSeqFormatV("\'", "/", "\'", va), t) ;
}

Pt XTestVar(register Pt t)
{
	VarValue(t) ;
	if( IsVar(t) ) return t ;
	return TypeError("VAR", t) ;
}

Pt XTestNonVar(register Pt t)
{
	VarValue(t) ;
	if( !IsVar(t) ) return t ;
	return TypeError("NON-VAR", t) ;
}

AtomPt XTermAtomOrNil(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtom(t) ;
	if( IsStruct(t) ) return XStructAtom(t) ;
	if( IsList(t) ) return XAtom(tDotAtom) ;
	return nil ;
}

CharPt XTestTermName(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtomName(t) ;
	if( IsStruct(t) ) return XStructName(t) ;
	if( IsList(t) ) return "." ;
	return TypeError("STRUCT, LIST or ATOM", t) ;
}

Hdl XTermArgs(register Pt t, int *arity)
{
	VarValue(t) ;
	if( IsStruct(t) ) {
		*arity = XStructArity(t) ;
		return XStructArgs(t) ;
	}
	elif( IsList(t) ) {
		*arity = 2 ;
		return XListArgs(t) ;
	}
	else {
		*arity = 0 ;
		return nil ;
	}
}

FunctorPt XTestFunctor(register Pt t)
{
	VarValue(t) ;
	if( IsStruct(t) ) return XStructFunctor(t) ;
	if( IsList(t) ) return listFunctor ;
	if( IsAtom(t) ) return LookupFunctor(XAtom(t), 0) ;
	return TypeError("STRUCT, LIST or ATOM", t) ;
}

FunctorPt XTestFunctor2(Pt t1, Pt t2)
{
	return( LookupFunctor(XTestAtom(t1), XTestNat(t2)) ) ;
}

FunctorPt XTestSlash(register Pt t)
{
	VarValue(t) ;
    if( IsThisStruct(t, slashFunctor) )
        return XTestFunctor2(XStructArg(t,0), XStructArg(t,1)) ;
	return TypeError("PREDICATE INDICATOR", t) ;
}

void XTestSlashArgs(register Pt t, Hdl a0, Hdl a1)
{
	VarValue(t) ;
    if( !IsThisStruct(t, slashFunctor) )
		TypeError("PREDICATE INDICATOR", t) ;
	*a0 = Drf(XStructArg(t, 0)) ;
	if( !IsVar(*a0) && !IsAtom(*a0) )
		TypeError("PREDICATE INDICATOR", t) ;
	*a1 = Drf(XStructArg(t, 1)) ;
	if( !IsVar(*a1) && !IsNat(*a1) )
		TypeError("PREDICATE INDICATOR", t) ;
}

FunctorPt XTestStruct(register Pt t, Hdl *args)
{
	VarValue(t) ;
	if( IsStruct(t) ) { *args = XStructArgs(t) ; return XStructFunctor(t) ; }
	if( IsList(t) ) { *args = XListArgs(t) ; return listFunctor ; }
	if( IsAtom(t) ) { *args = H ; return LookupFunctor(XAtom(t), 0) ; }
	return TypeError("STRUCT, LIST or ATOM", t) ;
}



/* CXPROLOG C'BUILTINS */

static void PVar()
{
	MustBe( IsVar(Drf(X0)) ) ;
}

static void PNonVar()
{
	MustBe( !IsVar(Drf(X0)) ) ;
}

static void PAtom()
{
	MustBe( IsAtom(Drf(X0)) ) ;
}

static void PPInt()
{
	MustBe( IsInt(Drf(X0)) ) ;
}

static void PPFloat()
{
	MustBe( IsFloat(Drf(X0)) ) ;
}

static void PNumber()
{
	MustBe( IsNumber(Drf(X0)) ) ;
}

static void PAtomic()
{
	Pt t = Drf(X0) ;
	MustBe( IsAtomicStrict(t) ) ;
}

static void PGround()
{
	MustBe( !HasFreeVars(X0) ) ;
}

static void PCompound()
{
	MustBe( IsCompound(Drf(X0)) ) ;
}

static void PCallable()
{
	Pt t = Drf(X0) ;
	while( IsUnitParam(t) )
		t = Drf(Z(OutParam(XUnitParam(t)))) ;
	MustBe( IsCompound(t) || IsAtom(t) ) ;
}

static void PIsList(void)
{
	MustBe( ListCheck(X0) ) ;
}

static void PFunctor()
{
	Pt t0 = Drf(X0) ;
	if( IsStruct(t0) )
		MustBe( UnifyWithAtomic(X1, TagAtom(XStructAtom(t0))) &&
				UnifyWithNumber(X2, MakeInt(XStructArity(t0))) ) ;
	elif( IsList(t0) )
		MustBe( UnifyWithAtomic(X1, tDotAtom) &&
				UnifyWithNumber(X2, MakeInt(2)) ) ;
	elif( IsAtomic(t0) )
		MustBe( UnifyWithAtomic(X1, t0) && UnifyWithNumber(X2, zeroIntPt) ) ;
	elif( IsVar(t0) ) {
		PInt n = XTestNat(X2) ;
		if( n == 0 )
			MustBe( UnifyWithAtomic(t0, TestAtomic(X1)) ) ;
		else
			MustBe( Unify(t0, MakeCleanStruct(LookupFunctor(XTestAtom(X1), n))) ) ;
	}
	InternalError("PFunctor") ;
}

static void PArg()
{
	PInt n = XTestInt(X0) ;
	Pt t1 = Drf(X1) ;
	if( IsStruct(t1) )
		MustBe( InRange(n,1,XStructArity(t1)) && Unify(X2, XStructArg(t1, n-1)) ) ;
	elif( IsList(t1) )
		MustBe( InRange(n,1,2) && Unify(X2, XListArg(t1, n-1)) ) ;
	else TypeError("STRUCT or LIST", t1) ;
}

static void PIns()
{
	PInt pos = XTestInt(X0) ;
	Pt t, t1 = Drf(X1) ;
	Hdl args, h ;
	FunctorPt f ;
	int newArity, n ;
	if( IsVar(t1) ) {
		f = XTestStruct(X3, &args) ;
		newArity = FunctorArity(f) - 1 ;
		Ensure( InRange(pos,1,newArity+1) ) ;
		t = MakeStruct(LookupFunctor(FunctorAtom(f), newArity), args) ;
		if( (n = newArity - pos) >= 0 ) {
			for( h = H - n ; n-- ; h++ )
				h[-1] = h[0] ;
			h[-1] = args[newArity] ;
		}
		MustBe( Unify(X1, t) && Unify(X2, args[pos-1]) ) ;
	}
	else {
		f = XTestStruct(t1, &args) ;
		newArity = FunctorArity(f) + 1 ;
		Ensure( InRange(pos,1,newArity) ) ;
		t = MakeStruct(LookupFunctor(FunctorAtom(f), newArity), args) ;
		n = newArity - pos ;
		for( h = H ; n-- ; h-- )
			h[-1] = h[-2] ;
		h[-1] = X2 ;
		MustBe( Unify(X3, t) ) ;
	}
}

static void PInsStart()
{
	Hdl args, h ;
	FunctorPt f = XTestStruct(X0, &args) ;
	int newArity = FunctorArity(f) + 1 ;
	Pt t = MakeStruct(LookupFunctor(FunctorAtom(f), newArity), args) ;
	int n = newArity - 1 ;
	for( h = H ; n-- ; h-- )
		h[-1] = h[-2] ;
	h[-1] = X1 ;
	MustBe( Unify(X2, t) ) ;
}

static void PInsEnd()
{
	Hdl args ;
	FunctorPt f = XTestStruct(X0, &args) ;
	int newArity = FunctorArity(f) + 1 ;
	Pt t = MakeStruct(LookupFunctor(FunctorAtom(f), newArity), args) ;
	H[-1] = X1 ;
	MustBe( Unify(X2, t) ) ;
}

static void PCopyTerm()
{
	Pt t = ZPushTerm(X0) ; /* stacks may grow */
	MustBe( Unify(X1, t) ) ;
}

static void PCopyTermWithEnv()
{
	Pt t = ZPushTermWithEnv(X0, X1, false) ; /* stacks may grow */
	MustBe( Unify(X2, t) ) ;
}

static void PFreeVars()
{
	Size nVars ;
	GetFreeVars(X0, &nVars) ;
	ZEnsureFreeSpaceOnStacks(2 * nVars, nil) ; /* stacks may grow */
	MustBe( Unify(X1, GetFreeVars(X0, nil)) ) ;
}

static void PSubTerm(void)
{
	MustBe( SubTerm(X0, X1) ) ;
}

static Bool AtomOrNumber_CodesOrChars(Bool any, Bool atom, Bool codes)
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) && (any || atom) ) {
		CharPt s = XAtomName(t0) ;
		ZEnsureFreeSpaceOnStacks(2 * CharLen(s), nil) ; /* stacks may grow */
		return Unify(X1, codes ? StringToPString(s) : StringToAString(s)) ;
	}
	if( IsNumber(t0) && (any || !atom) ) {
		CharPt s = XNumberAsStr(t0) ;
		ZEnsureFreeSpaceOnStacks(2 * CharLen(s), nil) ; /* stacks may grow */
		return Unify(X1, codes ? StringToPString(s) : StringToAString(s)) ;
	}
	elif( IsVar(t0) ) {
		Pt t1 = Drf(X1) ;
		if( IsList(t1) || t1 == tNilAtom ) {
			CharPt s = codes ? PStringToString(t1) : AStringToString(t1) ;
			if( any ) {
				t1 = NumberFromStr(s) ;
				if( t1 == nil ) t1 = MakeTempAtom(s) ;
			}
			else t1 = atom ? MakeTempAtom(s) : NumberFromStr(s) ;
			return t1 != nil && UnifyWithAtomic(t0, t1) ;
		}
		else return ITypeError("LIST", t1) ;
	}
	else return false ;
}

static void PName()
{
	MustBe( AtomOrNumber_CodesOrChars(true, true, true) ) ;
}

static void PAtomCodes()
{
	MustBe( AtomOrNumber_CodesOrChars(false, true, true) ) ;
}

static void PNumberCodes()
{
	MustBe( AtomOrNumber_CodesOrChars(false, false, true) ) ;
}

static void PAtomChars()
{
	MustBe( AtomOrNumber_CodesOrChars(false, true, false) ) ;
}

static void PNumberChars()
{
	MustBe( AtomOrNumber_CodesOrChars(false, false, false) ) ;
}

static void PNumberVars()
{
	MustBe( UnifyWithAtomic(X2, MakeInt(NumberVars(X0, XTestInt(X1)))) ) ;
}

static void PSlice()
{
	CharPt s = XTestAtomName(X0) ;
	PInt i0 = XTestInt(X1) ;
	PInt i1 = XTestInt(X2) ;
	if( i0 > 0 && i1 > 0 ) ;
	elif( i0 == 0 || i1 == 0 )
		ArithError("The two integer arguments of slice/4 must be non-zero") ;
	else {
		PInt len = CharLen(s) ;
		if( i0 < 0 ) i0 += len + 1 ;
		if( i1 < 0 ) i1 += len + 1 ;
		if( i0 <= 0 && i1 <= 0 )
			MustBe( UnifyWithAtomic(X3, tEmptyAtom) ) ;	
		if( i0 <= 0 ) i0 = 1 ;
		if( i1 <= 0 ) i1 = 1 ;
	}
	BigStrOpen() ;
	BigStrAddStrSlice(s, i0-1, i1-1) ;
	MustBe( Unify(X3, MakeTempAtom(BigStrClose())) ) ;
}

static void PSubAtom() /* preliminary */
{
	CharPt s = XTestAtomName(X0) ;
	PInt before = XTestInt(X1) ;
	PInt lenght = XTestInt(X2) ;
	PInt after = CharLen(s) - before - lenght ;
	if( before < 0 )
		ArithError("The second argument is less than be zero") ;
	if( lenght < 0 )
		ArithError("The third argument is less than be zero") ;
	Ensure( after >= 0 ) ;
	BigStrOpen() ;
	BigStrAddStrSlice(s, before, before + lenght - 1) ;
	MustBe( UnifyWithNumber(X3, MakeInt(after))
			&& Unify(X4, MakeTempAtom(BigStrClose())) ) ;
}

static void PConcat()
{
	MustBe( Unify(X1, MakeTempAtom(TermsAsStr(X0))) ) ;
}

static void PConcatRev()
{
	MustBe( Unify(X0, MakeTempAtom(TermsAsStr(X1))) ) ;
}

#if COMPAT_0_90_3
static void PUnique()
{
	static Word w = -2 ;
	CharPt s = GStrFormat("$%%u%lu", ++w) ;
	MustBe( Unify(X0, MakeTempAtom(s)) ) ;
}
#endif

void TermsInit()
{
	termSegm = Allocate(termSegmSize, false) ; /* must have alloc addr */
	freeVar = Allocate(1, false) ; /* must have alloc addr too */
	ResetVar(freeVar) ;

	InstallCBuiltinPred("var", 1, PVar) ;
	InstallCBuiltinPred("nonvar", 1, PNonVar) ;
	InstallCBuiltinPred("atom", 1, PAtom) ;
	InstallCBuiltinPred("integer", 1, PPInt) ;
	InstallCBuiltinPred("float", 1, PPFloat) ;
	InstallCBuiltinPred("number", 1, PNumber) ;
	InstallCBuiltinPred("atomic", 1, PAtomic) ;
	InstallCBuiltinPred("ground", 1, PGround) ;
	InstallCBuiltinPred("compound", 1, PCompound) ;
	InstallCBuiltinPred("callable", 1, PCallable) ;
	InstallCBuiltinPred("is_list", 1, PIsList) ;

	InstallCBuiltinPred("functor", 3, PFunctor) ;
	InstallCBuiltinPred("arg", 3, PArg) ;
	InstallCBuiltinPred("ins", 4, PIns) ;
	InstallCBuiltinPred("ins_start", 3, PInsStart) ;
	InstallCBuiltinPred("ins_end", 3, PInsEnd) ;
	InstallCBuiltinPred("copy_term", 2, PCopyTerm) ;
	InstallCBuiltinPred("copy_term", 3, PCopyTermWithEnv) ;
	InstallCBuiltinPred("free_vars", 2, PFreeVars) ;
	InstallCBuiltinPred("subterm", 2, PSubTerm) ;

	InstallCBuiltinPred("name", 2, PName) ;
	InstallCBuiltinPred("atom_codes", 2, PAtomCodes) ;
	InstallCBuiltinPred("number_codes", 2, PNumberCodes) ;
	InstallCBuiltinPred("atom_chars", 2, PAtomChars) ;
	InstallCBuiltinPred("number_chars", 2, PNumberChars) ;

	InstallCBuiltinPred("numbervars", 3, PNumberVars) ;
	InstallCBuiltinPred("slice", 4, PSlice) ;
	InstallCBuiltinPred("sub_atom", 5, PSubAtom) ;
	InstallCBuiltinPred("concat", 2, PConcat) ;
	InstallCBuiltinPred("===", 2, PConcatRev) ;

#if COMPAT_0_90_3
	InstallCBuiltinPred("unique", 1, PUnique) ;
#endif
}
