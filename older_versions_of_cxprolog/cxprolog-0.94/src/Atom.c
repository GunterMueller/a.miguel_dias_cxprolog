/*
 *   This file is part of the CxProlog system

 *   Atom.c
 *   by A.Miguel Dias - 2000/04/02
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

/*
 * PERMANENT/TEMPORARY ATOM RULES:
 * Atoms occurring in source code (clauses) are kept as 'permanent atoms'.
 * Ivar names are kept as 'permanent atoms'.
 * Unit names are kept as 'permanent atoms'.
 * Operator names are kept as 'permanent atoms'.
 * Predicate names are kept as 'permanent atoms'.
 * All other atoms are kept as 'temporary atoms'.
 * 
 * There is a garbage collector of temporary atoms that safely recycles
 * again and again the space taken by transient text.
 */

#include "CxProlog.h"


/* ATOMS */

static ExtraTypePt atomType ;

Pt tNilAtom, tEmptyAtom, tEofAtom, tCutAtom, tTrueAtom, tFalseAtom,
	tFailAtom, tOnAtom, tOffAtom, tMinusAtom, tUnderAtom,
	tDotAtom, tBracketsAtom, tQuestionAtom, tEllispisAtom, 
	tLessAtom, tEqualAtom, tGreaterAtom, tErrorAtom, tGoingAtom,
	tKilledAtom, tCompletedAtom, tFailedAtom,
	tTextAtom, tBinaryAtom, tEofCodeAtom, tResetAtom,
	tUserAtom, tVoidAtom, tNullAtom, tBadTermAtom ;

#define AtomWords(name)	(WordsOf(Atom) + Words(strlen(name) + 1))

static Bool LookupTempAtomAux(VoidPt x, VoidPt ref)
{
	return StrEqual(AtomName(x), cCharPt(ref)) ;
}

AtomPt LookupTempAtom(CharPt name)
{
	UChar slot = StrHash(name) ;	/* hash function */
	register AtomPt a ;
	if( (a = ExtraFindFirst(atomType, slot, LookupTempAtomAux, name)) == nil ) {
		a = ExtraNewWithSize(atomType, AtomWords(name), slot) ;
		AtomFunctors(a) = nil ;
		AtomToUnit(a) = nil ;
		AtomToIVar(a) = nil ;
		AtomToOperator(a) = nil ;
		strcpy(AtomName(a), name) ;
	}
	return a ;
}

AtomPt LookupAtom(CharPt name)
{
	AtomPt a = LookupTempAtom(name) ;
	ExtraSetPermanent(a) ;
	return a ;
}  

Size ForEachInSpec(Pt t, FunctorFun fun, Bool allowAtoms)
{
	Size n = 0 ;
	t = Drf(t) ;
	if( allowAtoms && IsAtom(t) ) {
		if( t == tNilAtom ) ;
		else {
			FunctorPt f ;
			doseq(f, AtomFunctors(XAtom(t)), f->nextArity)
				n += fun(f) ;
		}
	}
	elif( IsThisStruct(t, slashFunctor) )	
		n += fun(XTestSlash(t)) ;
	elif( IsThisStruct(t, commaFunctor) ) {
		n += ForEachInSpec(XStructArg(t,0), fun, allowAtoms) ;
		n += ForEachInSpec(XStructArg(t,1), fun, allowAtoms) ;	
	}	
	elif( IsList(t) ) {
		for( ; IsList(t) ; t = Drf(XListTail(t)) )
			n += ForEachInSpec(XListHead(t), fun, allowAtoms) ;
		if( t != tNilAtom )
			TypeError("PROPERLY-TERMINATED-LIST", nil) ;
	}
	else Error("Invalid predicate indicator") ;
	return n ;
}

static void PNDCurrentAtom()
{
	ExtraPNDCurrent(atomType, nil, 1, 0) ;
	JumpNext() ;
}

static Size AtomSizeFun(VoidPt x)
{
	if( x == nil )
		InternalError("AtomSizeFun") ;
	return AtomWords(AtomName(x)) ;
}

static Bool AtomBasicGCDelete(VoidPt x)
{
	return true ;
}


/* FUNCTORS */

FunctorPt commaFunctor, semicolonFunctor, neckFunctor, commandFunctor,
		slashFunctor, colonFunctor, listFunctor, stringFunctor, metaCutFunctor,
        eqFunctor, barFunctor, hifenFunctor, bracketsFunctor, parFunctor,
		varFunctor, primitiveFunctor, unitParamFunctor, emptyFunctor,
		ctxPushFunctor, ctxSwitchFunctor, ctxHEnterFunctor, ctxHExitFunctor ;

static Bool FunctorCheckAux(VoidPt x, VoidPt fref)
{
	AtomPt a = cAtomPt(x) ;
	register FunctorPt f ;
	doseq(f, AtomFunctors(a), f->nextArity)
		if( fref == f ) return true ;
	return false ;
}
Bool FunctorCheck(VoidPt ref)
{
	return ExtraFindFirst(atomType, -1, FunctorCheckAux, ref) != nil ;
}

FunctorPt LookupFunctor(AtomPt atom, int arity)
{
	register FunctorPt f ;
/* Find functor */
	doseq(f, AtomFunctors(atom), f->nextArity)
		if( FunctorArity(f) == arity ) return f ;
/* New functor */
	if( arity > maxFunctorArity )
		DatabaseError("Highest arity (%d) exceeded on functor '%s/%d'",
							maxFunctorArity, AtomName(atom), arity) ;
	f = Allocate(WordsOf(Functor), false) ;
	FunctorAtom(f) = atom ;
	FunctorArity(f) = arity ;
	FunctorPreds(f) = nil ;
	FunctorIsBuiltin(f) = false ;
	FunctorIsMeta(f) = false ;
/*	FunctorIsInLine(f) = false ; */
	FunctorIsSpy(f) = false ;
/* Update functor atom */
	ExtraSetPermanent(atom) ;
	f->nextArity = AtomFunctors(atom) ;
	AtomFunctors(atom) = f ;
	return f ;
}

FunctorPt LookupFunctorByName(CharPt name, int arity)
{
	return LookupFunctor(LookupAtom(name), arity) ;
}

CharPt FunctorNameArity(FunctorPt f)
{
	return GStrFormat("%s/%d", FunctorName(f), FunctorArity(f)) ;
}

FunctorFun funaux ;
static Size ForEachFunctorAux(VoidPt x)
{
	AtomPt a = cAtomPt(x) ;
	Size n = 0 ;
	register FunctorPt f ;
	doseq(f, AtomFunctors(a), f->nextArity)
		n += funaux(f) ;
	return n ;
}
Size ForEachFunctor(FunctorFun fun)
{
	funaux = fun ;
	return ExtraForEach(atomType, ForEachFunctorAux) ;
}

Size SpyOff(FunctorPt f)
{
	if( FunctorIsSpy(f) ) {
		WriteStd("%% Spy point removed from %s.\n", FunctorNameArity(f)) ;
		FunctorIsSpy(f) = false ;
		return 1 ;
	}
	return 0 ;
}

Size SpyOn(FunctorPt f)
{
    if( FunctorIsBuiltin(f) ) {
         WriteStd("%% Cannot place spy point on built-in '%s'.\n",
                                        FunctorNameArity(f)) ;
        return 0 ;
    }
    if( !FunctorIsSpy(f) ) {
        WriteStd("%% Spy point on %s.\n", FunctorNameArity(f)) ;
        FunctorIsSpy(f) = true ;
        return 1 ;
    }
    return 0 ;
}

void NoSpyAll()
{
	ForEachFunctor(SpyOff) ;
}

static Size WriteFunctorSpyPoint(FunctorPt f)
{
	if( FunctorIsSpy(f) ) {
		WriteStd("%%           %s\n", FunctorNameArity(f)) ;
		return 1 ;
	}
	else return 0 ;
}
void WriteSpyPoints()
{
	WriteStd("%% Spy points on:\n") ;
	if( ForEachFunctor(WriteFunctorSpyPoint) == 0 )
		WriteStd("%%        %% none %%\n") ;
}

void FunctorsInit()
{
	commaFunctor = LookupFunctorByName(",", 2) ;
	semicolonFunctor = LookupFunctorByName(";", 2) ;
	neckFunctor = LookupFunctorByName(":-", 2) ;
	commandFunctor = LookupFunctorByName(":-", 1) ;
	slashFunctor = LookupFunctorByName("/", 2) ;
	colonFunctor = LookupFunctorByName(":", 2) ;
	listFunctor = LookupFunctorByName(".", 2) ;
	stringFunctor = LookupFunctorByName("\"\"", 1) ;
    metaCutFunctor = LookupFunctorByName("$$_meta_cut", 1) ;
 	eqFunctor = LookupFunctorByName("=", 2) ;
	barFunctor = LookupFunctorByName("|", 2) ;
	hifenFunctor = LookupFunctorByName("-", 2) ;
	bracketsFunctor = LookupFunctorByName("{}", 1) ;
	parFunctor = LookupFunctorByName("$PAR", 1) ;
	varFunctor = LookupFunctorByName("$VAR", 1) ;
	primitiveFunctor = LookupFunctorByName("$$_primitive", 1) ;
	unitParamFunctor = LookupFunctorByName("$$_unit_parameter", 1) ;
	emptyFunctor = LookupFunctorByName("", 0) ;
	ctxPushFunctor = LookupFunctorByName(">>", 2) ;
	ctxSwitchFunctor = LookupFunctorByName("<>", 2) ;
	ctxHEnterFunctor = LookupFunctorByName(">", 1) ;
	ctxHExitFunctor = LookupFunctorByName("<", 1) ;
}


/* CXPROLOG C'BUILTINS */

static Size AtomsAux(VoidPt x)
{
	Write("%s", AtomName(x)) ;
	return 1 ;
}
static void PAtoms()
{
	ExtraShow(atomType, AtomsAux) ;
	JumpNext() ;
}

static void PSaneArity()
{
	if( XTestNat(X0) > maxFunctorArity )
		DatabaseError("Highest functor arity (%d) exceeded", maxFunctorArity) ;
	JumpNext() ;
}

void AtomsInit(void)
{
	register int i ;
	Str4 s ;
	atomType = ExtraTypeNew("ATOM", AtomSizeFun, nil, AtomBasicGCDelete, 256) ;
	tNilAtom = MakeAtom("[]") ;
	tEmptyAtom = MakeAtom("") ;
	tEofAtom = MakeAtom("end_of_file") ;
	tCutAtom = MakeAtom("!") ;
	tTrueAtom = MakeAtom("true") ;
	tFalseAtom = MakeAtom("false") ;
	tFailAtom = MakeAtom("fail") ;
	tOnAtom = MakeAtom("on") ;
	tOffAtom = MakeAtom("off") ;
	tMinusAtom = MakeAtom("-") ;
	tUnderAtom = MakeAtom("_") ;
	tDotAtom = MakeAtom(".") ;
	tBracketsAtom = MakeAtom("{}") ;
	tQuestionAtom = MakeAtom("?") ;
	tEllispisAtom = MakeAtom("...") ;
	tLessAtom = MakeAtom("<") ;
	tEqualAtom = MakeAtom("=") ;
	tGreaterAtom = MakeAtom(">") ;	
	tErrorAtom = MakeAtom("error") ;
	tGoingAtom = MakeAtom("going") ;
	tKilledAtom = MakeAtom("killed") ;
	tCompletedAtom = MakeAtom("completed") ;
	tFailedAtom = MakeAtom("failed") ;

	tTextAtom = MakeAtom("text") ;
	tBinaryAtom = MakeAtom("binary") ;
	tEofCodeAtom = MakeAtom("eof_code") ;
	tResetAtom = MakeAtom("reset") ;
	
	tUserAtom = MakeAtom("user") ;
	tVoidAtom = MakeAtom("void") ;
	tNullAtom = MakeAtom("null") ;
	tBadTermAtom = MakeAtom("*BADTERM*") ;
	s[1] = '\0' ;
	for( i = 'A' ; i <= 'Z' ; i++ ) {
		s[0] = i ; Ignore(MakeAtom(s)) ;
	}
	for( i = '0' ; i <= '9' ; i++ ) {
		s[0] = i ; Ignore(MakeAtom(s)) ;
	}
}

void AtomsInit2()
{
	InstallCBuiltinPred("atoms", 0, PAtoms) ;
	InstallGNDeterCBuiltinPred("current_atom", 1, 2, PNDCurrentAtom) ;
	InstallCBuiltinPred("$$_sane_arity", 1, PSaneArity) ;
}
