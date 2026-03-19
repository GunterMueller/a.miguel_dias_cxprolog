/*
 *   This file is part of the NanoProlog system

 *   Term.c
 *   by A.Miguel Dias - 89/11/14
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931203: handling of reals.
 931117: release of version 0.5

*/

#include "NanoProlog.h"


static char tStr[120] ;


void CheckHost()
{
	CharPt pt ;
	
	pt = TAlloc(1000L) ;
	if( pt == nil ) InternalError("CheckHost") ;
	if( GetTag(pt) != tagVar )
		Error("Term tags must be redefined for this machine <%lx>", pt) ;
	free(pt) ;
}

/* ATOMS */

Atom *nilAtom, *trueAtom, *eofAtom, *userAtom ;
Pt tNilAtom, tCutAtom, tFailAtom ;

#define atomTableSize 317

static Atom *atomTable[atomTableSize] = { nil } ;

static int HashFunction(char *str)
{
	register int sum = 0 ;

	while( *str ) sum += *str++ ;
	return( sum % atomTableSize ) ;
}

static Atom *NewAtom(char *name)
{
	Atom *at = HeapAlloc(WordsOf(Atom) + Words(strlen(name) + 1), true) ;
	
	at->next = nil ;
	AtomFunctors(at) = nil ;
	strcpy(AtomName(at), name) ;
	return( at ) ;
}

Atom *LookupAtom(char *name)
{
	Atom **pt ;
	
	for( pt = atomTable + HashFunction(name) ;
				*pt != nil ; pt = &(**pt).next )
					if( strcmp(AtomName(*pt), name) == 0 ) return( *pt ) ;	
	return( *pt = NewAtom(name) ) ;
}

void ForEachAtom(Proc p)
{
	Atom *at ;
	int i ;
	
	dotimes(i, atomTableSize)
		foreach(at, atomTable[i], at->next)
			p(at) ;	
}

void CreateInitialAtoms()
{
	nilAtom = LookupAtom("[]") ;
	trueAtom = LookupAtom("true") ;
	eofAtom = LookupAtom("end_of_file") ;
	userAtom = LookupAtom("user") ;
	tNilAtom = TagAtom( nilAtom ) ;
	tCutAtom = MakeAtom("!") ;
	tFailAtom = MakeAtom("fail") ;
}

static void ListAtom(Atom *at)
{
	printf("%s\n", AtomName(at)) ;
}

void ListAtoms()
{
	ForEachAtom(ListAtom) ;
}



/* FUNCTORS */

Functor *commaFunctor, *semicolonFunctor, *neckFunctor,
		*listFunctor, *switchFunctor ;

static Functor *NewFunctor(Atom *atom, int arity)
{
	Functor *f = HeapAlloc(WordsOf(Functor), true) ;
	
	f->next = nil ;
	FunctorAtom(f) = atom ;
	FunctorArity(f) = arity ;
	FunctorPreds(f) = nil ;
	FunctorIsC(f) = false ;
	FunctorIsSys(f) = false ;
	return( f ) ;
}

Functor *LookupFunctor(Atom *atom, int arity)
{
	Functor **pt ;
	
	for( pt = &AtomFunctors(atom) ; *pt != nil ; pt = &(**pt).next )
					if( FunctorArity(*pt) == arity ) return( *pt ) ;	
	return( *pt = NewFunctor(atom, arity) ) ;
}

char *FunctorNameArity(Functor *f)
{
	sprintf(tStr, "%s/%d", FunctorName(f), FunctorArity(f)) ;
	return(tStr) ;
}

void CreateInitialFunctors()
{
	commaFunctor = LookupFunctor2(",", 2) ;
	semicolonFunctor = LookupFunctor2(";", 2) ;
	neckFunctor = LookupFunctor2(":-", 2) ;
	listFunctor = LookupFunctor2(".", 2) ;
	switchFunctor = LookupFunctor2(":", 2) ;
}



/* VARS */

Pt drf, drf0, drf1, drf2, drf3, drf4, drf5, drf6 ;

Pt Drf(register Pt t)
{
	VarValue(t) ;
	return( t ) ;
}

Pt MakeVar()
{
	return( PushVar(H) ) ;
}

char *VarName(Pt t)
{
	if (IsVar(t))
		if( IsLocalVar(t) )
			sprintf(tStr, "L%ld", stacksEnd - XVar(t) ) ;
		else sprintf(tStr, "_%ld", XVar(t) - stacksBegin ) ;
	else strcpy(tStr, "<NOT A VAR>") ;
	return( tStr ) ;
}



/* REALS */

Pt MakeReal(Real r)
{
	Mix mix ;

	if( sizeof(float) == sizeof(Word) )
		mix.asFloat = r ;
	else
		mix.asDouble = r ;
	EncodeReal(mix)	
	mix.asWord |= tagReal ;
	return mix.asPt ;
}

Real XReal(Pt p)
{
	Mix mix ;

	mix.asPt = p ;
	DecodeReal(mix)	
	if( sizeof(float) == sizeof(Word) )
		return mix.asFloat ;
	else
		return mix.asDouble ;
}

/* STRUCTS */

Pt MakeStruct(Functor *functor, Hdl args)
{
	Struct *st = cStructPt(H) ;

	GrowGlobal(WordsOf(Struct) + functor->arity) ;	
	StructFunctor(st) = functor ;
	if( args != nil )
		CopyWords(StructArgs(st), args, functor->arity) ;
	return( TagStruct(st) ) ;
}

Pt MakeUnStruct(Functor *functor, Pt arg)
{
	Struct *st = cStructPt(H) ;

	GrowGlobal(WordsOf(Struct) + 1) ;	
	StructFunctor(st) = functor ;
	StructArg(st, 0) = arg ;
	return( TagStruct(st) ) ;
}

Pt MakeBinStruct(Functor *functor, Pt arg0, Pt arg1)
{
	Struct *st = cStructPt(H) ;

	GrowGlobal(WordsOf(Struct) + 2) ;	
	StructFunctor(st) = functor ;
	StructArg(st, 0) = arg0 ;
	StructArg(st, 1) = arg1 ;
	return( TagStruct(st) ) ;
}

char *StructNameArity(Struct *st)
{
	sprintf(tStr, "%s/%d", StructName(st), StructArity(st)) ;
	return(tStr) ;
}

void SplitClauseTerm(Pt c, Pt *h, Pt *t)
{
	VarValue(c) ;
	if( IsThisStruct(c, neckFunctor) )
	{
		*h = Drf(XStructArg(c,0)) ;
		*t = Drf(XStructArg(c,1)) ;
	}
	else
	{
		*h = c ;
		*t = TagAtom(trueAtom) ;
	}
}

Pt ClauseHead(Pt cl)
{
	return( IsThisStruct(cl, neckFunctor) ? XStructArg(cl, 0) : cl ) ;
}



/* LISTS */

Pt MakeList(h, t)
Pt h, t ;
{
	List *l = cListPt(H) ;

	GrowGlobal(WordsOf(List)) ;	
	ListHead(l) = h ;
	ListTail(l) = t ;
	return( TagList(l) ) ;
}

Pt ArrayToList(n, elems)
int n;
Hdl elems;
{
	Pt list ;
	
	list = elems[ --n ] ;
	while( n ) list = MakeList(elems[ --n ], list) ;
	return( list ) ;
}

Pt ArrayToListAddNil(n, elems)
int n;
Hdl elems;
{
	Pt list ;
	
	list = tNilAtom ;
	while( n ) list = MakeList(elems[ --n ], list) ;
	return( list ) ;
}

int ListLength(l, proper)
Pt l ;
Bool *proper ;
{
	int n = 0 ;

	VarValue(l) ;
	while( IsList(l) )
	{
		l = Drf(XListTail(l)) ;
		n++ ;
	}
	*proper = l == tNilAtom ;
	return( n ) ;
}

Pt StringToAtom(l)
Pt l ;
{
	char buff[1024] ;
	int maxPos = 1022 ;
	int n = 0 ;
	Pt h ;
	
	while( IsList(l) )
	{
		h = Drf(XListHead(l)) ;
		if( not IsByte(h) ) Error("Invalid string") ;
		if( n >= maxPos ) Error("String to long") ;
		buff[n++] = XInt(h) ;
		l = Drf(XListTail(l)) ;
	}
	if( l != tNilAtom ) Error("Invalid string") ;
	buff[n] = '\0' ;
	return( MakeAtom(buff) ) ;
}

Pt AtomToString(a)
Pt a ;
{
	char *name = XAtomName(a) ;
	int n = strlen(name) ;
	Pt list ;
	
	list = tNilAtom ;
	while( n ) list = MakeList(MakeInt(name[--n]), list) ;
	return( list ) ;
}

Pt StringToList(CharPt s)
{
	int n = strlen(s) ;
	unsigned char *u = (unsigned char *)s ;
	Pt list ;

	list = tNilAtom ;
	while( n ) list = MakeList(MakeInt(u[--n]), list) ;
	return( list ) ;
}


/* TERMS */

char *TermTypeStr(Pt t)
{
	return(
		t == nil	? "nil pointer" :
		IsVar(t)	? "var" :
		IsStruct(t)	? "struct" :
		IsList(t)	? "list" :
		IsAtom(t)	? "atom" :
		IsNumber(t)	? "number" :
					  "unknown term"
	) ;
}

Pt MakeCleanTerm(Functor *functor)
{
	int arity = FunctorArity(functor) ;
	Pt t ;
	Hdl args ;
	
	if( arity == 0 ) return( TagAtom(FunctorAtom(functor)) ) ;
	elif( functor == listFunctor )
	{
		t = MakeList(nil,nil) ;
		args = XListArgs(t) ;
	}
	else
	{
		t = MakeStruct(functor, nil) ;
		args = XStructArgs(t) ;
	}
	
	while( arity-- ) PushVar(args) ;
	return( t ) ;
}

static Hdl termCurr ;

static Pt TransVar(Pt old, Hdl where)
{
	int i ;
	
	dotimes(i, varDic.nVars)
		if( varDic.vars[i].var == old ) return( varDic.vars[i].new ) ;
	varDic.vars[varDic.nVars].var = old ;
	varDic.vars[varDic.nVars].new = ResetVar(*where) ;
	return( varDic.vars[varDic.nVars++].new ) ;
}

static Pt CopyTermAux(Pt term)
/* Argument already derreferenced */
{
	if( IsStruct(term) )
	{
		register Functor *f = XStructFunctor(term) ;
		register int arity = FunctorArity(f) ;
		register Pt *args0 = XStructArgs(term) ;

		Struct *st = cStructPt(termCurr) ;
		Pt *args1 = StructArgs(st) ;

		termCurr += WordsOf(Struct) + arity ;
		StructFunctor(st) = f ;
		while( arity-- )
		{
			VarValue2(drf, *args0++) ;
			*args1 = IsVar(drf) ? TransVar(drf, args1) : CopyTermAux(drf) ;
			args1++ ;
		}
		return( TagStruct(st) ) ;
	}
	
	if( IsList(term) )
	{
		register int arity = 2 ;
		register Pt *args0 = XListArgs(term) ;

		List *l = cListPt(termCurr) ;
		Pt *args1 = ListArgs(l) ;

		termCurr += WordsOf(List) ;
		while( arity-- )
		{
			VarValue2(drf1, *args0++) ;
			*args1 = IsVar(drf1) ? TransVar(drf1, args1) : CopyTermAux(drf1) ;
			args1++ ;
		}
		return( TagList(l) ) ;
	}
	
	return( term ) ;
}

static int TSize(Pt term)
/* Argument already derreferenced */
{
	if( IsStruct(term) )
	{
		register int arity = XStructArity(term) ;
		register Pt *args = XStructArgs(term) ;
		register int size = 1 ;

		while( arity-- ) size += TSize(Drf(*args++)) ;
		return( size + 1 ) ;
	}
	
	if( IsList(term) )
		return( TSize(Drf(XListHead(term)))
					+ TSize(Drf(XListTail(term))) + 1 ) ;
	
	return( 1 ) ;
}

static int TermSize(Pt term)
/* Argument already derreferenced */
{
	return( TSize(term) - 1 ) ;
}

static Pt CopyTerm(Pt term, Hdl to)
/* Argument already derreferenced */
{
	ResetVarDic() ;
	termCurr = to ;
	return( CopyTermAux(term) ) ;
}

Pt CopyTermToHeap(Pt term)	/* The saved term is completly derreferenced */
{
	VarValue(term) ;
	if( IsAtomic(term) ) return( term ) ;
	else return( CopyTerm(term, HeapAlloc(TermSize(term), false)) ) ;
}

void FreeHeapTerm(Pt term)
{
	if( not IsAtomic(term) ) HeapFree(XPt(term)) ;
}

Pt CopyTermToGlobal(Pt term)
{
	Hdl whereTo ;
	
	VarValue(term) ;
	if( IsAtomic(term) ) return( term ) ;
	whereTo = H ;
	GrowGlobal(TermSize(term)) ;
	return( CopyTerm(term, whereTo) ) ;
}
