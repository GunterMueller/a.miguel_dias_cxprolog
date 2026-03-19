/*
 *   This file is part of the CxProlog system

 *   Term.c
 *   by A.Miguel Dias - 1989/11/14
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

/* VARS */

Pt Drf(register Pt t)
{
	VarValue(t) ;
	return t ;
}

Pt MakeVar()
{
	return PushVar(H) ;
}

CharPt VarName(Pt t)
{
	if (IsVar(t))
		if( IsLocalVar(t) )
			sprintf(retBuffer, "L%ld", stacksEnd - cHdl(t) ) ;
		else sprintf(retBuffer, "_%ld", cHdl(t) - stacksBegin ) ;
	else strcpy(retBuffer, "<NOT A VAR>") ;
	return retBuffer ;
}

Bool IsVarName(CharPt s)
{
	return( s[0] == '_' || InRange(s[0],'A','Z') ) ;
}


/* STRUCTS */

Pt MakeStruct(FunctorPt functor, Hdl args)
{
	CheckGlobalOverflow() ;
	if( functor == listFunctor ) {
		Push(H, args[0]) ;
		Push(H, args[1]) ;
		return TagList(H - 2) ;
	}
	elif( FunctorArity(functor) == 0 )
		return TagAtom(FunctorAtom(functor)) ;
	else {
		register int i, arity = FunctorArity(functor) ;
		Push(H, functor) ;
		dotimes(i, arity)
			Push(H, args[i]) ;
		return TagStruct(H - arity - 1) ;
	}
}

Pt MakeCleanStruct(FunctorPt functor)
{
	CheckGlobalOverflow() ;
	if( functor == listFunctor ) {
		PushVar(H) ;
		PushVar(H) ;
		return TagStruct(H - 2) ;
	}
	elif( FunctorArity(functor) == 0 )
		return TagAtom(FunctorAtom(functor)) ;
	else {
		register int i, arity = FunctorArity(functor) ;
		Push(H, functor) ;
		dotimes(i, arity)
			PushVar(H) ;
		return TagStruct(H - arity - 1) ;
	}
}

Pt MakeUnStruct(FunctorPt functor, Pt arg)
{
	CheckGlobalOverflow() ;
	Push(H, functor) ;
	Push(H, arg) ;
	return TagStruct(H - 2) ;
}

Pt MakeBinStruct(FunctorPt functor, Pt arg0, Pt arg1)
{
	CheckGlobalOverflow() ;
	if( functor == listFunctor ) {
		Push(H, arg0) ;
		Push(H, arg1) ;
		return TagList(H - 2) ;
	}
	else {
		Push(H, functor) ;
		Push(H, arg0) ;
		Push(H, arg1) ;
		return TagStruct(H - 3) ;
	}
}

CharPt StructNameArity(StructPt st)
{
	sprintf(retBuffer, "%s/%d", StructName(st), StructArity(st)) ;
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
		Error("Unit parameter expected") ;
	i = XTestInt(XStructArg(t, 0)) ;
	if( not InRange(i,1,UnitArity(CurrUnit())) )
		Error("Reference to a non existent parameter of unit '%s'", UnitSignature(CurrUnit())) ;
	return i ;
}


/* LISTS */

Pt ArrayToOpenList(register Hdl array, register Size n)
{
	Pt list = array[ --n ] ;	
	while( n )
		list = MakeBinStruct(listFunctor, array[ --n ], list) ;
	return list ;
}

Pt ArrayToList(register Hdl array, register Size n)
{
	Pt list = tNilAtom ;
	while( n )
		list = MakeBinStruct(listFunctor, array[ --n ], list) ;
	return list ;
}

Hdl ListToArray(Pt list, Size *len)
{
	register Pt l ;
	UseBuffer() ;
	for( l = Drf(list) ; IsList(l) ; l = Drf(XListTail(l)) )
		BufferPush(Drf(XListHead(l))) ;
	if( l != tNilAtom )
		Error("Not a proper list") ;
	*len = BufferUsed() ;
	return cHdl(FreeBuffer()) ;
}

Size ListLength(register Pt l)
{
	Size n = 0 ;
	for( l = Drf(l) ; IsList(l) ; l = Drf(XListTail(l)) )
		n++ ;
	if( l != tNilAtom )
		Error("Not a proper list") ;
	return n ;
}

Size KeyListLength(register Pt l)
{
	Size n = 0 ;
	for( l = Drf(l) ; IsList(l) ; l = Drf(XListTail(l)) ) {
		Pt t = Drf(XListHead(l)) ;
		if( not IsThisStruct(t, hifenFunctor) )
			Error("Not a proper key-list") ;
		n++ ;
	}
	if( l != tNilAtom )
		Error("Not a proper key-list") ;
	return n ;
}


/* STRINGS */

Pt StringToPString(CharPt s)
{
	register Size n = strlen(s) ;
	register UCharPt u = cUCharPt(s) ;
	Pt list = tNilAtom ;
	while( n )
		list = MakeBinStruct(listFunctor, MakeInt(u[--n]), list) ;
	return list ;
}

CharPt PConcatString(Pt list)
{
	register Pt l, t ;
	UseBuffer() ;
	for( l = Drf(list) ; IsList(l) ; l = Drf(XListTail(l)) ) {
		t = Drf(XListHead(l)) ;
		if( IsAtomOrText(t) )
			BufferAddStr(t == tNilAtom ? "" : XAtomOrTextName(t)) ;
		elif( IsNumber(t) || IsThisStruct(t, formatFunctor) )
			BufferAddStr(XNumberAsStr(t)) ;
		elif( IsList(t) )
			BufferAddPString(t) ;
		elif( IsExtra(t) )
			BufferAddStr(XExtraAsStr(t)) ;
		else
			Error("Invalid arguments") ;
	}
	if( l != tNilAtom )
		Error("Invalid list") ;
	BufferAddCh('\0') ;
	return FreeBuffer() ;
}


/* EXTRA */

static CharPt XExtraTypeStr(Pt t) 
{
	switch( XExtraSubTag(t) ) {
		case threadSubTag: return "thread" ;
		case queueSubTag: return "queue" ;
		case stackSubTag: return "stack" ;
		case dictSubTag: return "dict" ;
		case streamSubTag: return "stream" ;
		default: Default("XExtraTypeStr") ;
	}
}

static Bool ExtraCheck(CharPt type, Pt t) 
{
	if( EqualStr(type, "thread") ) return ThreadCheck(t) ;
	if( EqualStr(type, "queue") ) return QueueCheck(t) ;
	if( EqualStr(type, "stack") ) return StackCheck(t) ;
	if( EqualStr(type, "dict") ) return DictCheck(t) ;
	if( EqualStr(type, "stream") ) return StreamCheck(t) ;
	Default("ExtraCheck") ;
}

CharPt XExtraAsStr(Pt t) 
{
	sprintf(retBuffer, "1'%s_%lx", XExtraTypeStr(t) , XPt(t)) ;
	return retBuffer ;
}

Pt MakeExtraFromStr(CharPt s) 
{
	CharPt type = s, sref = nil ;
	Pt ref ;
	for( ; *s ; s++ )
		if( *s == '_' ) {
			*s = '\0' ;
			sref = s + 1 ;
			break ;
		}
	if( sref == nil || sscanf(sref, "%lx", &ref) != 1 ) return nil ;
	if( ExtraCheck(type, ref) ) return TagExtra(ref) ;
	return nil ;
}


/* TERMS */

CharPt TermTypeStr(Pt t)
{
	t = Drf(t) ;
	return
		t == nil	? "nil pointer" :
		IsVar(t)	? "var" :
		IsStruct(t)	? "struct" :
		IsList(t)	? "list" :
		IsText(t)	? "text" :
		IsAtom(t)	? "atom" :
		IsInt(t)	? "int" :
		IsReal(t)	? "real" :
		IsExtra(t)	? XExtraTypeStr(t) :
					  "unknown term"
	;
}

static Hdl termCurr ;
static Bool asserting, usingEnv ;

/* TermSize is called each time a term is about to be asserted, assigned ou copied */

Size TermSize(register Pt term)
{
	register Size size = 0 ;
	UseBuffer() ;
	for(;;) {
		VarValue(term) ;
		if( IsStruct(term) ) {
			register int arity = XStructArity(term) ;
			register Hdl args = XStructArgs(term) ;
			if( (size += arity + 1) > maxTermSize )
				Error("term too large") ;
			term = *args ;
			while( --arity )
				BufferPush(*++args) ;
		}
		elif( IsList(term) ) {
			if( (size += 2) > maxTermSize )
				Error("term too large") ;
			BufferPush(XListTail(term)) ;
			term = XListHead(term) ;
		}
		else {
			if( IsText(term) && not asserting ) {
				CharPt name = XAtomOrTextName(term) ;
				if( (size += Words(strlen(name)+1)) > maxTermSize )
					Error("term too large") ;
			}
			if( BufferUsed() != 0 )
				term = BufferPop() ;
			else break ;
		}
	}
	FreeBuffer() ;
	return size ;
}

static Pt CopyTerm(register Pt term, Hdl trVar)
{
	UseBuffer() ; FreeBuffer() ;
	if( IsVar(term) ) {
		if( usingEnv )
			return TranslateVarWithEnv(term, trVar) ;
		else
			return TranslateVar(term, trVar) ;
	}
	elif( IsStruct(term) ) {
		register FunctorPt f = XStructFunctor(term) ;
		register int arity = FunctorArity(f) ;
		register Pt *args0 = XStructArgs(term) ;

		StructPt st = cStructPt(termCurr) ;
		Hdl args1 = StructArgs(st) ;

		termCurr += WordsOf(Struct) + arity ;
		StructFunctor(st) = f ;
		while( arity-- ) {
			*args1 = CopyTerm(*args0++, args1) ;
			args1++ ;
		}
		return TagStruct(st) ;
	}
	elif( IsList(term) ) {
		register int arity = 2 ;
		register Pt *args0 = XListArgs(term) ;

		ListPt l = cListPt(termCurr) ;
		Hdl args1 = ListArgs(l) ;

		termCurr += WordsOf(List) ;
		while( arity-- ) {
			*args1 = CopyTerm(*args0++, args1) ;
			args1++ ;
		}
		return TagList(l) ;
	}
	elif( IsText(term) ) {
		if( asserting )
			return TextToAtom(term) ;
		else {
			CharPt name = XAtomOrTextName(term) ;
			CharPt s = cCharPt(termCurr) ;
			termCurr += Words(strlen(name)+1) ;
			strcpy(s, name) ;
			return TagText(s) ;
		}
	}
	elif( IsExtra(term) && asserting ) {
		Error("A clause cannot contain a '%s' literal", XExtraTypeStr(term)) ;
		return nil ;
	}
	else
		return term ;
}

Pt AllocateTermForAssert(register Pt term)
{
	Size size ;
	VarValue(term) ;
	asserting = true ;
	usingEnv = false ;
	if( (size = TermSize(term)) > 0 ) {
		ResetVarDic() ; UseBuffer() ; FreeBuffer() ;
		termCurr = TemporaryAllocate(size) ;
		return CopyTerm(term, nil) ;	/* Handles 'texts' too */
	}
	if( IsExtra(term) ) CopyTerm(term, nil) ;	/* Generates error */
	return term ;
}
		
Pt AllocateTermForAssign(register Pt term)
{
	Size size ;
	VarValue(term) ;
	asserting = false ;
	usingEnv = false ;
	if( (size = TermSize(term)) > 0 ) {
		ResetVarDic() ; UseBuffer() ; FreeBuffer() ;
		termCurr = TemporaryAllocate(size) ;
		return CopyTerm(term, nil) ;	/* Handles 'texts' too */
	}
	if( IsVar(term) ) {
		static Pt freeVar = cPt(&freeVar) ;
		return freeVar ;
	}
	else return term ;
}

void ReleaseTerm(register Pt term)
{
	VarValue(term) ;
	if( IsRecord(term) )
		Release(XPt(term)) ;
	elif( IsText(term) )
		Release(UntagText(term)) ;
}

Pt PushTerm(register Pt term)
{
	Size size ;
	VarValue(term) ;
	asserting = false ;
	usingEnv = false ;
	if( (size = TermSize(term)) > 0 ) {
		ResetVarDic() ; UseBuffer() ; FreeBuffer() ;
		termCurr = H ;
		 CheckGlobalOverflow() ;
		 Grow(H, size) ;
		 CheckGlobalOverflow() ;
		return CopyTerm(term, nil) ;	/* Handles 'texts' too */
	}
	if( IsVar(term) )
		return PushVar(H) ;
	else return term ;
}

static Pt PushTermWithEnv(register Pt env, register Pt term)
{
	Size size ;
	VarValue(term) ;
	asserting = false ;
	usingEnv = true ;
	if( (size = TermSize(term)) > 0 ) {
		ResetVarDicWithEnv(env) ; UseBuffer() ; FreeBuffer() ;
		termCurr = H ;
		 CheckGlobalOverflow() ;
		 Grow(H, size) ;
		 CheckGlobalOverflow() ;		
		return CopyTerm(term, nil) ;	/* Handles 'texts' too */
	}
	if( IsVar(term) )
		return PushVar(H) ;
	else return term ;
}

/* NUMBER VARS */

static Size numberVarsN ;

static void DoNumberVars(register Pt t)
{
	VarValue(t) ;
	if( IsAtomic(t) ) ;
	elif( IsVar(t) ) {
		Assign(t, TagStruct(H)) ;
		Push(H, varFunctor) ;
		Push(H, MakeInt(numberVarsN++)) ;
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
	if( IsAtomOrText(t) ) return XAtomOrTextAsAtom(t) ;
	TypeError("atom or text", t) ;
	return nil ;
}

CharPt XTestAtomName(register Pt t)
{
	VarValue(t) ;
	if( IsAtomOrText(t) ) return XAtomOrTextName(t) ;
	TypeError("atom or text", t) ;
	return nil ;
}

BigInt XTestInt(register Pt t)
{
	VarValue(t) ;
	if( IsInt(t) ) return XInt(t) ;
	TypeError("integer", t) ;
	return 0 ;
}

BigInt XTestPosInt(register Pt t)
{
	VarValue(t) ;
	if( IsPos(t) ) return XInt(t) ;
	TypeError("positive integer", t) ;
	return 0 ;
}

BigInt XTestNat(register Pt t)
{
	VarValue(t) ;
	if( IsNat(t) ) return XInt(t) ;
	TypeError("natural number", t) ;
	return 0 ;
}

double XTestReal(register Pt t)
{
	VarValue(t) ;
	if( IsInt(t) ) return XInt(t) ;
	if( IsReal(t) ) return XReal(t) ;
	TypeError("number", t) ;
	return 0 ;
}

Bool XTestBool(register Pt t)
{
	VarValue(t) ;
	if( t == tTrueAtom ) return true ;
	if( t == tFalseAtom ) return false ;
	TypeError("boolean", t) ;
	return false ;
}

Pt XTestFlag(register Pt t)
{
	VarValue(t) ;
	if( t == tOnAtom ) return tOnAtom ;
	if( t == tOffAtom ) return tOffAtom ;
	TypeError("on/off", t) ;
	return nil ;
}

Pt XTestVar(register Pt t)
{
	VarValue(t) ;
	if( IsVar(t) ) return t ;
	TypeError("var", t) ;
	return nil ;
}

Pt XTestNonVar(register Pt t)
{
	VarValue(t) ;
	if( not IsVar(t) ) return t ;
	TypeError("non-var", t) ;
	return nil ;
}

FunctorPt XTestFunctor(register Pt t)
{
	VarValue(t) ;
	if( IsStruct(t) ) return XStructFunctor(t) ;
	if( IsList(t) ) return listFunctor ;
	if( IsAtomOrText(t) ) return LookupFunctor(XAtomOrTextAsAtom(t), 0) ;
	TypeError("atom or functor", t) ;
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
	if( IsAtomOrText(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PInt()
{
	if( IsInt(Drf(X0)) ) JumpNext()
	DoFail()
}

static void PReal()
{
	if( IsReal(Drf(X0)) ) JumpNext()
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
			elif( IsAtomOrText(t1) && 
				Unify(t0, MakeCleanStruct(LookupFunctor(XAtomOrTextAsAtom(t1),
														XInt(t2)))) )
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
		if( XTestPosInt(X0) > XStructArity(t1) ) Error("Out of range") ;
		if( Unify(X2, XStructArg(t1, XTestPosInt(X0) - 1)) ) JumpNext()
		DoFail()
	}
	elif( IsList(t1) ) {
		if( XTestPosInt(X0) > 2 ) Error("Out of range") ;
		if( Unify(X2, XListArg(t1, XTestPosInt(X0) - 1)) ) JumpNext()
		DoFail()
	}
	else TypeError("structure or list", t1) ;
}

static void PCopyTerm()
{
	if( Unify(X1, PushTerm(X0)) ) JumpNext()
		DoFail()
}

static void PCopyTermWithEnv()
{
	if( Unify(X2, PushTermWithEnv(X0, X1)) ) JumpNext()
		DoFail()
}

static void PName()
{
	Pt t0 = Drf(X0) ;
	Pt t1 = Drf(X1) ;
	if( IsAtomOrText(t0) ) {
		if( Unify(t1, StringToPString(XAtomOrTextName(t0))) ) JumpNext()
		DoFail()
	}
	elif( IsList(t1) || t1 == tNilAtom ) {
		UseBuffer() ;
		BufferAddPString(t1) ;
		BufferAddCh('\0') ;
		if( UnifyWithAtomic(t0, MakeAtomOrText(FreeBuffer())) ) JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PNumberVars()
{
	if( UnifyWithAtomic(X2, MakeInt(NumberVars(X0, XTestNat(X1)))) ) JumpNext()
	DoFail()
}

static void PSlice()
{
	CharPt s = XTestAtomName(X0) ;
	Size i = strlen(s) ;
	BigInt i0 = XTestInt(X1) ;
	BigInt i1 = XTestInt(X2) ;
	if( i0 >= 0 && i1 >= 0 ) {
		if( i0 < 1 ) i0 = 1 ;
		if( i0 > i ) i0 = i ;
		if( i1 < 1 ) i1 = 1 ;
		if( i1 > i ) i1 = i ;
		if( i1 < i0 ) DoFail()
		UseBuffer() ;
		BufferAddNStr(s + i0 - 1, i1 - i0 + 1) ;
		BufferAddCh('\0') ;
		if( Unify(X3, MakeAtomOrText(FreeBuffer())) ) JumpNext()
		DoFail()
	}
	elif( i0 < 0 && i1 < 0 ) {
		if( i0 < -i ) i0 = -i ;
		if( i1 < -i ) i1 = -i ;
		if( i1 < i0 ) DoFail()
		UseBuffer() ;
		BufferAddNStr(s + i + i0, i1 - i0 + 1) ;
		BufferAddCh('\0') ;
		if( Unify(X3, MakeAtomOrText(FreeBuffer())) ) JumpNext()
		DoFail()
	}
	else Error("The two integer arguments of slice/4 must have the same arithmetic signal") ;
}

static void PConcat()
{
	if( Unify(X1, MakeAtomOrText(PConcatString(X0))) ) JumpNext()
	DoFail()
}

void InitTerms()
{
	InstallCBuiltinPred("var", 1, PVar) ;
	InstallCBuiltinPred("nonvar", 1, PNonVar) ;
	InstallCBuiltinPred("atom", 1, PAtom) ;
	InstallCBuiltinPred("integer", 1, PInt) ;
	InstallCBuiltinPred("real", 1, PReal) ;
	InstallCBuiltinPred("float", 1, PReal) ;
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
	InstallCBuiltinPred("===", 2, PConcat) ;
}
