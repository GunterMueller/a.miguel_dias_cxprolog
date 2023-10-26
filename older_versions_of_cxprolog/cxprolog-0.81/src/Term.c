/*
 *   This file is part of the CxProlog system

 *   Term.c
 *   by A.Miguel Dias - 1989/11/14
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


/* GLOBAL STACK CONTROL */

/* Parameter of ResetVar is not a Pt, is a proper Var */
#define ResetVar(v)			( *cHdl(v) = cPt(v) )
#define PushHVar()			( ResetVar(H), cPt(H++) )
#define PushH(v)			Push(H, v)
#define PopH()				Pop(H)
#define GrowH(n)			Grow(H, n)


/* VARS */

Pt Drf(register Pt t)
{
	VarValue(t) ;
	return t ;
}

Pt MakeVar()
{
	CheckFreeSpaceOnStacks(1) ;
	return PushHVar() ;
}

CharPt VarName(Pt t)
{
	if( IsVar(t) )
		if( IsLocalVar(t) )
			sprintf(retBuffer, "L%ld", Df(stacksEnd, t) ) ;
		else sprintf(retBuffer, "_%ld", Df(t, stacksBegin) ) ;
	else strcpy(retBuffer, "<NOT A VAR>") ;
	return retBuffer ;
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
		PushHVar() ;
		PushHVar() ;
		return TagList(H - 2) ;
	}
	elif( FunctorArity(functor) == 0 )
		return TagAtom(FunctorAtom(functor)) ;
	else {
		register int i, arity = FunctorArity(functor) ;
		CheckFreeSpaceOnStacks(arity + 1) ;
		PushH(functor) ;
		dotimes(i, arity)
			PushHVar() ;
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
	if( functor == listFunctor ) {
		CheckFreeSpaceOnStacks(2) ;
		PushH(arg0) ;
		PushH(arg1) ;
		return TagList(H - 2) ;
	}
	else {
		CheckFreeSpaceOnStacks(3) ;
		PushH(functor) ;
		PushH(arg0) ;
		PushH(arg1) ;
		return TagStruct(H - 3) ;
	}
}

CharPt XStructNameArity(Pt t)
{
	sprintf(retBuffer, "%s/%d", XStructName(t), XStructArity(t)) ;
	return retBuffer ;
}

void SplitClauseTerm(Pt c, Pt *h, Pt *t)
{
	c = Drf(c) ;
	if( IsThisStruct(c, neckFunctor) ) {
		*h = Drf(XStructArg(c,0)) ;
		*t = Drf(XStructArg(c,1)) ;
	}
	else {
		*h = c ;
		*t = tTrueAtom ;
	}
}

int XUnitParam(register Pt t)
{
	int i ;
	t = Drf(t) ;
	if( not IsUnitParam(t) )
		TypeError2("UNIT-PARAMETER", nil) ;
	i = XTestInt(XStructArg(t, 0)) ;
	if( not InRange(i,1,UnitArity(CurrUnit())) )
		TypeError2("UNIT-PARAMETER", nil) ;
	return i ;
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
	UseBuffer() ;
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) )
		BufferPush(Drf(XListHead(list))) ;
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
	*len = BufferUsed() ;
	return cHdl(FreeBuffer()) ;
}

Size ListLength(register Pt list)
{
	Size n = 0 ;
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) )
		n++ ;
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
	return n ;
}

static Bool Belongs(Pt t, register Pt list)	/* pre: t already deref */
{
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) )
		if( t == Drf(XListHead(list)) ) return true ;
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
	return false ;
}


/* STRINGS */

static void DoConcatString(Pt list)
{
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) ) {
		Pt t = Drf(XListHead(list)) ;
		if( IsList(t) )
			DoConcatString(t) ;
		else
			BufferAddTerm(t) ;
	}
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
}

static CharPt ConcatString(Pt list)
{
	UseBuffer() ;
	DoConcatString(list) ;
	BufferAddCh('\0') ;
	return FreeBuffer() ;
}

Pt StringToPString(CharPt s)
{
	register Size n = strlen(s) ;
	register UCharPt u = cUCharPt(s) ;
	register Pt list ;
	CheckFreeSpaceOnStacks(2 * n) ;
	list = tNilAtom ;
	while( n-- ) {
		PushH(MakeInt(u[n])) ;
		PushH(list) ;
		list = TagList(H - 2) ;
	}
	return list ;
}


/* TERMS */

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
		IsExtra(t)	? StrUpper(XExtraTypeName(t)) :
					  "UNKNOWN"
	;
}

void TermAtomGCMark(register Pt t)
{
	UseBuffer() ;
	for(;;) {
		VarValue(t) ;
		if( IsAtom(t) )
			AtomGCMark(XAtom(t)) ;
		elif( IsStruct(t) ) {
			register int arity = XStructArity(t) ;
			register Hdl args = XStructArgs(t) ;
			t = *args ;
			while( --arity ) BufferPush(*++args) ;
			continue ;
		}
		elif( IsList(t) ) {
			BufferPush(XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
	/* Extras are ignored on purpose */	
		if( BufferUsed() == 0 ) break ;
		else t = BufferPop() ;
	}
	FreeBuffer() ;
}


/* TermSize is called each time a term is about to be asserted,
	assigned ou copied */

Size TermSize(register Pt t)
{
	register Size size = 0 ;
	UseBuffer() ;
	for(;;) {
		VarValue(t) ;
		if( IsStruct(t) ) {
			register int arity = XStructArity(t) ;
			register Hdl args = XStructArgs(t) ;
			if( (size += arity + 1) > maxTermSize )
				DatabaseError("Term too large") ;
			t = *args ;
			while( --arity ) BufferPush(*++args) ;
			continue ;
		}
		elif( IsList(t) ) {
			if( (size += 2) > maxTermSize )
				DatabaseError("Term too large") ;
			BufferPush(XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		if( BufferUsed() == 0 ) break ;
		else t = BufferPop() ;
	}
	FreeBuffer() ;
	return size ;
}

static Pt CopyTerm(register Pt t, register Hdl to, Pt env, Bool asserting)
{
	Pt res ;
	register Hdl where = &res ;	
	Hdl bottom = to ;
	TrailSave() ;	/* trick: trail is instrumental to refreshing vars */
	UseBuffer() ;
	for(;;) {
		VarValue(t) ;
		if( IsStruct(t) ) {
			FunctorPt f = XStructFunctor(t) ;
			int arity = FunctorArity(f) ;
			register Hdl args = XStructArgs(t) ;
			*where = TagStruct(to) ;
			Push(to, f) ;
			where = to++ ;
			t = *args ;
			while( --arity ) {
				BufferPush(to) ;
				Push(to, *++args) ;
			}
			continue ;
		}
		elif( IsList(t) ) {
			*where = TagList(to) ;
			where = to++ ;
			BufferPush(to) ;
			Push(to, XListTail(t)) ;
			t = XListHead(t) ;
			continue ;
		}
		elif( IsVar(t) ) {
			if( Le(bottom,t) && Lt(t,to) )
				*where = t ;
			elif( env == nil || Belongs(t, env) ) {
				ResetVar(where) ;
				TrailVar(t) ;
				SetVar(t, where) ;
			}
			else *where = t ;
		}
		else
			*where = t ;

		if( asserting ) {
			if( IsAtom(t) )
				AtomPermanent(XAtom(t)) = true ;
			elif( IsExtra(t) )
				DatabaseError("A clause cannot contain a '%s' literal",
							XExtraTypeName(t)) ;
		}

		if( BufferUsed() == 0 ) break ;
		else {
			where = cHdl(BufferPop()) ;
			t = *where ;
		}	
	}
	FreeBuffer() ;
	TrailRestore() ;
	return res ;
}

Pt AllocateTermForAssert(register Pt t)
{
	VarValue(t) ;
	if( IsRecord(t) )
		return CopyTerm(t, TempBlockAllocate(TermSize(t)), nil, true) ;
	else
		return t ;
}
		
Pt AllocateTermForAssign(register Pt t)
{
	VarValue(t) ;
	if( IsRecord(t) )
		return CopyTerm(t, TempBlockAllocate(TermSize(t)), nil, false) ;
	elif( IsVar(t) ) {
		static Pt freeVar = cPt(&freeVar) ;
		return freeVar ;
	}
	else
		return t ;
}

void ReleaseTerm(register Pt t)
{
	VarValue(t) ;
	if( IsRecord(t) )
		BlockRelease(XPt(t)) ;
}

static Pt ZPushTermWithEnv(Pt env, register Pt t)
{
	VarValue(t) ;
	if( IsRecord(t) ) {
		extern Pt ZT ;
		Size size = TermSize(t) ;
		Hdl to = H ;
		ZT = t ;
		ZEnsureFreeSpaceOnStacks(size) ;
		GrowH(size) ;
		return CopyTerm(ZT, to, env, false) ;
	}
	elif( IsVar(t) ) {
		ZEnsureFreeSpaceOnStacks(1) ;
		return PushHVar() ;
	}
	else
		return t ;
}

Pt ZPushTerm(Pt t)
{
	return ZPushTermWithEnv(nil, t) ;
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
	else Default("DoNumberVars") ;
}

static Size NumberVars(Pt t, Size start)
{
	numberVarsN = start ;
	DoNumberVars(t) ;
	return( numberVarsN ) ;
}


/* TYPE ERRORS */

AtomPt XTestAtom(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtom(t) ;
	TypeError2("ATOM", t) ;
	return nil ;
}

CharPt XTestAtomName(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtomName(t) ;
	TypeError2("ATOM", t) ;
	return nil ;
}

PInt XTestInt(register Pt t)
{
	VarValue(t) ;
	if( IsInt(t) ) return XInt(t) ;
	TypeError2("INT", t) ;
	return 0 ;
}

PInt XTestPosInt(register Pt t)
{
	VarValue(t) ;
	if( IsPos(t) ) return XInt(t) ;
	TypeError2("POSITIVE-INT", t) ;
	return 0 ;
}

PInt XTestNat(register Pt t)
{
	VarValue(t) ;
	if( IsNat(t) ) return XInt(t) ;
	TypeError2("NATURAL-NUMBER", t) ;
	return 0 ;
}

PInt XTestIntRange(register Pt t, int a, int z)
{
	VarValue(t) ;
	if( IsInt(t) && InRange(XInt(t), a, z) ) return XInt(t) ;
	sprintf(retBuffer, "RANGE(%d..%d)", a, z) ;
	TypeError2(retBuffer, t) ;
	return 0 ;
}

PFloat XTestFloat(register Pt t)
{
	VarValue(t) ;
	if( IsInt(t) ) return XInt(t) ;
	if( IsFloat(t) ) return XFloat(t) ;
	TypeError2("NUMBER", t) ;
	return 0 ;
}

Bool XTestBool(register Pt t)
{
	VarValue(t) ;
	if( t == tTrueAtom ) return true ;
	if( t == tFalseAtom ) return false ;
	TypeError2("BOOL", t) ;
	return false ;
}

Bool XTestOnOff(register Pt t)
{
	VarValue(t) ;
	if( t == tOnAtom ) return true ;
	if( t == tOffAtom ) return false ;
	TypeError2("'on/off'", t) ;
	return false ;
}

Pt XTestVar(register Pt t)
{
	VarValue(t) ;
	if( IsVar(t) ) return t ;
	TypeError2("VAR", t) ;
	return nil ;
}

Pt XTestNonVar(register Pt t)
{
	VarValue(t) ;
	if( not IsVar(t) ) return t ;
	TypeError2("NON-VAR", t) ;
	return nil ;
}

FunctorPt XTestFunctor(register Pt t)
{
	VarValue(t) ;
	if( IsStruct(t) ) return XStructFunctor(t) ;
	if( IsList(t) ) return listFunctor ;
	if( IsAtom(t) ) return LookupFunctor(XAtom(t), 0) ;
	TypeError2("ATOM or FUNCTOR", t) ;
	return nil ;
}

FunctorPt XTestFunctor2(Pt t1, Pt t2)
{
	return( LookupFunctor(XTestAtom(t1), XTestNat(t2)) ) ;
}

/* CXPROLOG C'BUILTINS */

static void PVar()
{
	if( IsVar(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PNonVar()
{
	if( IsVar(Drf(X0)) ) DoFail()
	JumpNext()
}

static void PAtom()
{
	if( IsAtom(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PPInt()
{
	if( IsInt(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PPFloat()
{
	if( IsFloat(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PNumber()
{
	if( IsNumber(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PAtomic()
{
	if( IsAtomic(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PFunctor()
{
	Pt t0 = Drf(X0) ;
	if( IsStruct(t0) ) {
		if( UnifyWithAtomic(X1, TagAtom(XStructAtom(t0))) &&
			UnifyWithNumber(X2, MakeInt(XStructArity(t0))) ) JumpNext()
		DoFail()
	}
	elif( IsList(t0) ) {
		if( UnifyWithAtomic(X1, tDotAtom) &&
			UnifyWithNumber(X2, MakeInt(2)) ) JumpNext()
		DoFail()
	}
	elif( IsAtomic(t0) || IsExtra(t0) ) {
		if( UnifyWithAtomic(X1, t0) &&
			UnifyWithNumber(X2, MakeInt(0)) ) JumpNext()
		DoFail()
	}
	elif( IsVar(t0) ) {
		Pt t1 = Drf(X1) ;
		Pt t2 = Drf(X2) ;
		if( IsAtomic(t1) && IsNat(t2) ) {
			if( XInt(t2) == 0 )
				if( UnifyWithAtomic(t0, t1) ) JumpNext()
				else DoFail()
			elif( IsAtom(t1) && 
				Unify(t0, MakeCleanStruct(LookupFunctor(XAtom(t1), XInt(t2)))) )
					JumpNext()
		}
		DoFail()
	}
	Default("PFunctor") ;
}

static void PArg()
{
	Pt t1 = Drf(X1) ;
	if( IsStruct(t1) ) {
		if( XTestPosInt(X0) > XStructArity(t1) )
			ArithError("Out of range") ;
		if( Unify(X2, XStructArg(t1, XTestPosInt(X0) - 1)) ) JumpNext()
		DoFail()
	}
	elif( IsList(t1) ) {
		if( XTestPosInt(X0) > 2 ) ArithError("Out of range") ;
		if( Unify(X2, XListArg(t1, XTestPosInt(X0) - 1)) ) JumpNext()
		DoFail()
	}
	else TypeError2("STRUCT or LIST", t1) ;
}

static void PCopyTerm()
{
	Pt t = ZPushTerm(X0) ;
	if( Unify(X1, t) ) JumpNext()
	DoFail()
}

static void PCopyTermWithEnv()
{
	Pt t = ZPushTermWithEnv(X0, X1) ;
	if( Unify(X2, t) ) JumpNext()
	DoFail()
}

static void PName()
{
	Pt t0 = Drf(X0) ;
	Pt t1 = Drf(X1) ;
	if( IsAtom(t0) ) {
		ZEnsureFreeSpaceOnStacks(2 * strlen(XAtomName(t0))) ;
		if( Unify(t1, StringToPString(XAtomName(t0))) ) JumpNext()
		DoFail()
	}
	elif( IsVar(t0) ) {
		if( IsList(t1) || t1 == tNilAtom ) {
			UseBuffer() ;
			BufferAddPString(t1) ;
			BufferAddCh('\0') ;
			if( UnifyWithAtomic(t0, MakeTempAtom(FreeBuffer())) ) JumpNext()
			DoFail()
		}
		else TypeError2("LIST", t1) ;
	}
	else TypeError2("ATOM or VAR", t0) ;
}

static void PNumberVars()
{
	if( UnifyWithAtomic(X2, MakeInt(NumberVars(X0, XTestNat(X1)))) )
		JumpNext()
	DoFail()
}

static void PSlice()
{
	CharPt s = XTestAtomName(X0) ;
	Size i = strlen(s) ;
	PInt i0 = XTestInt(X1) ;
	PInt i1 = XTestInt(X2) ;
	if( i0 >= 0 && i1 >= 0 ) {
		if( i0 < 1 ) i0 = 1 ;
		if( i0 > i ) i0 = i ;
		if( i1 < 1 ) i1 = 1 ;
		if( i1 > i ) i1 = i ;
		if( i1 < i0 ) DoFail()
		UseBuffer() ;
		BufferAddNStr(s + i0 - 1, i1 - i0 + 1) ;
		BufferAddCh('\0') ;
		if( Unify(X3, MakeTempAtom(FreeBuffer())) ) JumpNext()
		DoFail()
	}
	elif( i0 < 0 && i1 < 0 ) {
		if( i0 < -i ) i0 = -i ;
		if( i1 < -i ) i1 = -i ;
		if( i1 < i0 ) DoFail()
		UseBuffer() ;
		BufferAddNStr(s + i + i0, i1 - i0 + 1) ;
		BufferAddCh('\0') ;
		if( Unify(X3, MakeTempAtom(FreeBuffer())) ) JumpNext()
		DoFail()
	}
	else ArithError("The two integer arguments of slice/4 must have the same arithmetic signal") ;
}

static void PConcat()
{
	if( Unify(X1, MakeTempAtom(ConcatString(X0))) ) JumpNext()
	DoFail()
}

static void PConcatRev()
{
	if( Unify(X0, MakeTempAtom(ConcatString(X1))) ) JumpNext()
	DoFail()
}

void TermsInit()
{
	InstallCBuiltinPred("var", 1, PVar) ;
	InstallCBuiltinPred("nonvar", 1, PNonVar) ;
	InstallCBuiltinPred("atom", 1, PAtom) ;
	InstallCBuiltinPred("integer", 1, PPInt) ;
	InstallCBuiltinPred("float", 1, PPFloat) ;
	InstallCBuiltinPred("number", 1, PNumber) ;
	InstallCBuiltinPred("atomic", 1, PAtomic) ;

	InstallCBuiltinPred("functor", 3, PFunctor) ;
	InstallCBuiltinPred("arg", 3, PArg) ;
	InstallCBuiltinPred("copy_term", 2, PCopyTerm) ;
	InstallCBuiltinPred("copy_term", 3, PCopyTermWithEnv) ;
	InstallCBuiltinPred("name", 2, PName) ;
	InstallCBuiltinPred("numbervars", 3, PNumberVars) ;
	InstallCBuiltinPred("slice", 4, PSlice) ;
	InstallCBuiltinPred("concat", 2, PConcat) ;
	InstallCBuiltinPred("===", 2, PConcatRev) ;
}
