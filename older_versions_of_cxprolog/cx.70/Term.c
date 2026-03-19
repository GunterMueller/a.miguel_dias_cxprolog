/*
 *   This file is part of the CxProlog system

 *   Term.c
 *   by A.Miguel Dias - 1989/11/14
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
#include <math.h>

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
			sprintf(strBuffer, "L%ld", stacksEnd - XVar(t) ) ;
		else sprintf(strBuffer, "_%ld", XVar(t) - stacksBegin ) ;
	else strcpy(strBuffer, "<NOT A VAR>") ;
	return strBuffer ;
}

Bool IsVarName(CharPt s)
{
	return( s[0] == '_' || InRange(s[0],'A','Z') ) ;
}


/* REALS */

static Bool ieeeFloats ;
static Bool useFloat ;

void InitReals()
{
	ieeeFloats = false ;
	ieeeFloats = fabs(222222.0 - XReal(MakeReal(222222.0))) > 0.1 ;

	if( sizeof(float) == sizeof(Word) )
		useFloat = true ;
	elif( sizeof(double) == sizeof(Word) )
		useFloat = false ;
	else
		FatalError("Cannot run on this machine because of the format of the real numbers") ;
}

Pt MakeReal(Real r)
{
	Mix mix ;
	if( useFloat ) mix.asFloat = r ; else mix.asDouble = r ;
	if( ieeeFloats ) EncodeRealIEEE(mix) else EncodeRealNoIEEE(mix)	
	mix.asWord |= tagReal ;
	return mix.asPt ;
}

Real XReal(Pt p)
{
	Mix mix ;
	mix.asPt = p ;
	if( ieeeFloats ) DecodeRealIEEE(mix) else DecodeRealNoIEEE(mix)	
	if( useFloat ) return mix.asFloat ; else return mix.asDouble ;
}



/* STRUCTS */

Pt MakeStruct(FunctorPt functor, Hdl args)
{
	Struct *st = cStructPt(H) ;

	GrowGlobal(WordsOf(Struct) + functor->arity) ;	
	StructFunctor(st) = functor ;
	if( args != nil )
		CopyWords(StructArgs(st), args, functor->arity) ;
	return TagStruct(st) ;
}

Pt MakeUnStruct(FunctorPt functor, Pt arg)
{
	Struct *st = cStructPt(H) ;

	GrowGlobal(WordsOf(Struct) + 1) ;	
	StructFunctor(st) = functor ;
	StructArg(st, 0) = arg ;
	return TagStruct(st) ;
}

Pt MakeBinStruct(FunctorPt functor, Pt arg0, Pt arg1)
{
	Struct *st = cStructPt(H) ;

	if( functor == listFunctor )
		return MakeList(arg0, arg1) ;
	GrowGlobal(WordsOf(Struct) + 2) ;	
	StructFunctor(st) = functor ;
	StructArg(st, 0) = arg0 ;
	StructArg(st, 1) = arg1 ;
	return TagStruct(st) ;
}

CharPt StructNameArity(Struct *st)
{
	sprintf(strBuffer, "%s/%d", StructName(st), StructArity(st)) ;
	return strBuffer ;
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

Int XUnitParam(register Pt t)
{
	Int i ;

	t = Drf(t) ;
	if( not IsUnitParam(t) )
		Error("Unit parameter expected") ;
	i = XTestInt(XStructArg(t, 0)) ;
	if( not InRange(i,1,UnitArity(CurrUnit())) )
		Error("Reference to a non existent parameter of unit '%s'", UnitSignature(CurrUnit())) ;
	return i ;
}



/* LISTS */

Pt MakeList(Pt h, Pt t)
{
	List *l = cListPt(H) ;

	GrowGlobal(WordsOf(List)) ;	
	ListHead(l) = h ;
	ListTail(l) = t ;
	return TagList(l) ;
}

Pt ArrayToList(int n, Hdl elems)
{
	Pt list ;
	
	list = elems[ --n ] ;
	while( n ) list = MakeList(elems[ --n ], list) ;
	return list ;
}

Pt ArrayToListAddNil(int n, Hdl elems)
{
	Pt list ;
	
	list = tNilAtom ;
	while( n ) list = MakeList(elems[ --n ], list) ;
	return list ;
}

int ListLength(Pt l, Bool *proper)
{
	int n = 0 ;

	l = Drf(l) ;
	while( IsList(l) ) {
		l = Drf(XListTail(l)) ;
		n++ ;
	}
	*proper = l == tNilAtom ;
	return n ;
}

Pt StringToPString(CharPt s)
{
	register int n = strlen(s) ;
	register UCharPt u = cUCharPt(s) ;
	Pt list ;

	list = tNilAtom ;
	while( n )
		list = MakeList(MakeInt(u[--n]), list) ;
	return list ;
}

CharPt PStringToString(Pt l)
{
	register CharPt b = strBuffer, lim = b + strBufferSize - 2 ;
	Pt t ;
	
	l = Drf(l) ;
	for( ; IsList(l) ; l = Drf(XListTail(l)) ) {
		t = Drf(XListHead(l)) ;
		if( not IsByte(t) ) Error("Invalid string") ;
		*b++ = XInt(t) ;
		if( b > lim ) {
			*b = '\0' ;
			Error("Too much text: '%s'", b) ;
		}
	}
	*b = '\0' ;
	if( l != tNilAtom ) Error("Invalid list") ;
	return strBuffer ;
}

CharPt PConcatString(Pt l)
{
	register CharPt b = cCharPt(codeBuffer) ;
	register CharPt s ;
	Pt t ;

	l = Drf(l) ;
	for( ; IsList(l) ; l = Drf(XListTail(l)) ) {
		t = Drf(XListHead(l)) ;
		if( IsAtom(t) ) s = t == tNilAtom ? "" : XAtomName(t) ;
		elif( IsInt(t) ) sprintf(s = strBuffer, "%ld", XInt(t)) ;
		elif( IsReal(t) ) sprintf(s = strBuffer, "%.5g", XReal(t)) ;
		elif( IsList(t) ) s = PStringToString(t) ;
		elif( IsExtra(t) ) s = XExtraName(t) ;
		else Error("Invalid arguments") ;

		if( b + strlen(s) > cCharPt(codeBufferEnd) )
			Error("Too much text: (more than %ld bytes)",
							cCharPt(codeBufferEnd) - cCharPt(codeBuffer)) ;

		strcpy(b, s) ;
		b += strlen(s) ;
	}
	*b = '\0' ;
	if( l != tNilAtom )
		Error("Invalid list") ;
	return cCharPt(codeBuffer) ;
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
		IsExtra(t)	? "extra" :
		IsAtom(t)	? "atom" :
		IsInt(t)	? "int" :
		IsReal(t)	? "real" :
					  "unknown term"
	;
}

Pt MakeCleanTerm(FunctorPt functor)
{
	int arity = FunctorArity(functor) ;
	Pt t ;
	Hdl args ;
	
	if( arity == 0 )
		t = TagAtom(FunctorAtom(functor)) ;
	elif( functor == listFunctor ) {
		t = MakeList(nil,nil) ;
		args = XListArgs(t) ;
	}
	else {
		t = MakeStruct(functor, nil) ;
		args = XStructArgs(t) ;
	}
	
	while( arity-- )
		PushVar(args) ;
	return t ;
}

static int TermSize(register Pt term)
{
	VarValue(term) ;
	if( IsStruct(term) ) {
		register int arity = XStructArity(term) ;
		register Hdl args = XStructArgs(term) ;
		register int size = arity + 1 ;

		while( arity-- ) size += TermSize(*args++) ;
		return size ;
	}
	elif( IsList(term) )
		return 2 + TermSize(XListHead(term)) + TermSize(XListTail(term)) ;
	else
		return 0  ;
}

static Hdl termCurr ;

static Pt CopyTermAux(register Pt term, Hdl trVar)
{
	VarValue(term) ;
	if( IsVar(term) ) {
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
			*args1 = CopyTermAux(*args0++, args1) ;
			args1++ ;
		}
		return TagStruct(st) ;
	}
	elif( IsList(term) ) {
		register int arity = 2 ;
		register Pt *args0 = XListArgs(term) ;

		ListPt l = cListPt(termCurr) ;
		Hdl args1 = ListArgs(l) ;
		Pt t ;

		termCurr += WordsOf(List) ;
		while( arity-- ) {
			*args1 = CopyTermAux(*args0++, args1) ;
			args1++ ;
		}
		return TagList(l) ;
	}
	elif( IsExtra(term) )
		return MakeExtraPermanent(term) ;
	else
		return term ;
}

static Pt CopyTerm(Pt term, Hdl to)
{
	ResetVarDic() ;
	termCurr = to ;
	return CopyTermAux(term, nil) ;
}

Pt CopyTermToSpace(Pt term)	/* The saved term is completely derreferenced */
{
	term = Drf(term) ;
	if( IsAtomic(term) )
		return term ;
	elif( IsVar(term) ) {
		static Pt freeVar = cPt(&freeVar) ;
		return freeVar ;
	}
	elif( IsExtra(term) )
		return MakeExtraPermanent(term) ;
	else {
		Hdl whereTo = SpaceAlloc(TermSize(term), false) ;
		return CopyTerm(term, whereTo) ;
	}
}

void FreeSpaceTerm(Pt term)
{
	if( IsAtomic(term) || IsVar(term) || IsExtra(term) )
		return ;
	SpaceFree(XPt(term)) ;
}

Pt CopyTermToGlobal(Pt term)
{
	term = Drf(term) ;
	if( IsAtomic(term) )
		return term ;
	elif( IsVar(term) )
		return PushVar(H) ;
	elif( IsExtra(term) )
		return term ;
	else {
		Hdl whereTo = H ;
		GrowGlobal(TermSize(term)) ;
		return CopyTerm(term, whereTo) ;
	}
}


/* TYPE ERRORS */

AtomPt XTestAtom(register Pt t)
{
	VarValue(t) ;
	if( IsAtom(t) ) return XAtom(t) ;
	TypeError("atom", t) ;
}

CharPt XTestAtomName(Pt t)
{
	return AtomName(XTestAtom(t)) ;
}

Int XTestInt(register Pt t)
{
	VarValue(t) ;
	if( IsInt(t) ) return XInt(t) ;
	TypeError("integer", t) ;
}

Int XTestPosInt(register Pt t)
{
	VarValue(t) ;
	if( IsPos(t) ) return XInt(t) ;
	TypeError("positive integer", t) ;
}

Int XTestNat(register Pt t)
{
	VarValue(t) ;
	if( IsNat(t) ) return XInt(t) ;
	TypeError("natural number", t) ;
}

Pt XTestVar(register Pt t)
{
	VarValue(t) ;
	if( IsVar(t) ) return t ;
	TypeError("var", t) ;
}

FunctorPt XTestFunctor(Pt t)
{
	t = Drf(t) ;
	if( IsStruct(t) ) return XStructFunctor(t) ;
	elif( IsAtom(t) ) return LookupFunctor(XAtom(t), 0) ;
	elif( IsList(t) ) return listFunctor ;
	else TypeError("atom or functor", t) ;
}

FunctorPt XTestFunctor2(Pt t1, Pt t2)
{
	return( LookupFunctor(XTestAtom(t1), XTestNat(t2)) ) ;
}
