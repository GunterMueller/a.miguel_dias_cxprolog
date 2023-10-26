/*
 *   This file is part of the CxProlog system

 *   Atom.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

Pt tNilAtom, tEmptyAtom, tEofAtom, tCutAtom, tTrueAtom, tFalseAtom,
	tFailAtom, tOnAtom, tOffAtom, tMinusAtom, tUnderAtom,
	tDotAtom, tBracketsAtom, tQuestionAtom, tEllispisAtom, 
	tLessAtom, tEqualAtom, tGreaterAtom, tErrorAtom, tGoingAtom,
	tKilledAtom, tCompletedAtom, tFailedAtom, tUserAtom, tVoidAtom,
	tBadTermAtom ;


#define atomTableCapacity	256

static AtomPt *atomTable ;

void AtomsInit(void)
{
	register int i ;
	Str4 s ;
	atomTable = PermAllocate(atomTableCapacity) ;
	dotimes(i, atomTableCapacity)
		atomTable[i] = nil ;
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
	tUserAtom = MakeAtom("user") ;
	tVoidAtom = MakeAtom("void") ;
	tBadTermAtom = MakeAtom("*BADTERM*") ;
	s[1] = '\0' ;
	for( i = 'A' ; i <= 'Z' ; i++ ) {
		s[0] = i ; Ignore(MakeAtom(s)) ;
	}
	for( i = '0' ; i <= '9' ; i++ ) {
		s[0] = i ; Ignore(MakeAtom(s)) ;
	}
}

AtomPt LookupAtom(CharPt name)
{
	register int slot ;
	register AtomPt a ;
/* hash function */
	slot = StrHash(name) ;
/* find atom */
	doseq(a, atomTable[slot % atomTableCapacity], a->nextHash)
		if( StrEqual(AtomName(a), name) ) {
			AtomIsPermanent(a) = true ;
			return a ;
		}	
/* create new atom */
	a = PermAllocate(WordsOf(Atom) + Words(strlen(name) + 1)) ;
	a->nextHash = atomTable[slot % atomTableCapacity] ;
	atomTable[slot % atomTableCapacity] = a ;
	AtomFunctors(a) = nil ;
	AtomToUnit(a) = nil ;
	AtomToIVar(a) = nil ;
	AtomToOperator(a) = nil ;
	strcpy(AtomName(a), name) ;
	AtomIsPermanent(a) = true ;
	return a ;
}

AtomPt LookupTempAtom(CharPt name)
{
	register int slot ;
	register AtomPt a ;
	int size ;
#if 0
	return LookupAtom(name) ;
#endif
/* hash function */
	slot = StrHash(name) ;
/* find atom */
	doseq(a, atomTable[slot % atomTableCapacity], a->nextHash)
		if( StrEqual(AtomName(a), name) )
			return a ;	
/* create new atom */
	size = WordsOf(Atom) + Words(strlen(name) + 1) ;
	a = TempAllocate(size) ;
	BasicGCAddDelta(size) ;
	a->nextHash = atomTable[slot % atomTableCapacity] ;
	atomTable[slot % atomTableCapacity] = a ;
	AtomFunctors(a) = nil ;
	AtomToUnit(a) = nil ;
	AtomToIVar(a) = nil ;
	AtomToOperator(a) = nil ;
	strcpy(AtomName(a), name) ;
	AtomIsPermanent(a) = false ;
	return a ;
}

Size AtomForEach(AtomFun fun)
{
	int i ;
	AtomPt a ;
	Size n = 0 ;
	dotimes(i, atomTableCapacity)
		doseq(a, atomTable[i], a->nextHash)
			n += fun(a) ;
	return n ;   
}   

Bool AtomCheck(VoidPt ref)
{
	register AtomPt a ;
	register int i ;
	dotimes(i, atomTableCapacity)
		doseq(a, atomTable[i], a->nextHash)
			if( ref == a ) return true ;
	return false ;	
}

void AtomGCMark(AtomPt at)
{
	AtomIsGCMarked(at) = true ;
}

Size AtomGCUnmark(AtomPt at)
{
    AtomIsGCMarked(at) = false ;
    return 0 ;
}

int AtomGCClearNotMarked()
{
	register int i ;
	register AtomPt *a ;
	int n = 0 ;
	dotimes(i, atomTableCapacity) {
		for( a = &atomTable[i] ; *a != nil ; ) {
			if( !AtomIsPermanent(*a) && !AtomIsGCMarked(*a) ) {
				AtomPt r = *a ;
#if 0
				Mesg(AtomName(r)) ;
#endif
				*a = (*a)->nextHash ;
				Release(r) ;
				n++ ;
			}
			else a = &(*a)->nextHash ;
		}
	}
	if( n > 0 && !testGCollection_flag )
		MemoryWarning("%d temporary atom%s removed by garbage collector",
														n, n>1 ? "s" : "") ;
	return n ;
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
			TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
	}
	else Error("Invalid predicate indicator") ;
	return n ;
}

#if unused
static Size ListAtom(AtomPt a)
{
	Write("%s\n", AtomName(a)) ;
	return 0 ;
}
static void AtomsList()
{
	AtomForEach(ListAtom) ;
}
#endif

static void PrintAtomTable()
{
	int i ;
	AtomPt a ;
	for( i = 0 ; i < atomTableCapacity ; i++ ) {
		Write("%d - ", i) ;
		doseq(a, atomTable[i], a->nextHash)
			Write("%s%s ", AtomIsPermanent(a) ? " " : "@", AtomName(a)) ;
		Write("\n") ;
	}
}



/* FUNCTORS */

FunctorPt commaFunctor, semicolonFunctor, neckFunctor, commandFunctor,
		slashFunctor, colonFunctor, listFunctor, stringFunctor, metaCutFunctor,
        eqFunctor, barFunctor, hifenFunctor, bracketsFunctor, parFunctor,
		varFunctor, primitiveFunctor, unitParamFunctor, emptyFunctor,
		ctxPushFunctor, ctxSwitchFunctor, ctxHEnterFunctor, ctxHExitFunctor ;

Bool FunctorCheck(VoidPt ref)
{
	register int i ;
	register AtomPt a ;
	register FunctorPt f ;
	dotimes(i, atomTableCapacity)
		doseq(a, atomTable[i], a->nextHash)
			doseq(f, AtomFunctors(a), f->nextArity)
				if( ref == f ) return true ;
	return false ;
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
	f = PermAllocate(WordsOf(Functor)) ;
	FunctorAtom(f) = atom ;
	FunctorArity(f) = arity ;
	FunctorPreds(f) = nil ;
	FunctorIsBuiltin(f) = false ;
	FunctorIsMeta(f) = false ;
/*	FunctorIsInLine(f) = false ; */
	FunctorIsSpy(f) = false ;
/* Update functor atom */
	AtomIsPermanent(atom) = true ;
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

Size ForEachFunctor(FunctorFun fun)
{
	int i ;
	AtomPt a ;
	FunctorPt f ;
	Size n = 0 ;
	dotimes(i, atomTableCapacity)
		doseq(a, atomTable[i], a->nextHash)
			doseq(f, AtomFunctors(a), f->nextArity)
				n += fun(f) ;
	return n ;
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

#if unused
static Size FuncCount(AtomPt a)
	{	return AtomFunctors(a) != nil ? 1 : 0 ;	}
#endif
static Size TempCount(AtomPt a)
	{	return !AtomIsPermanent(a) ? 1 : 0 ;	}
static Size PermCount(AtomPt a)
	{	return AtomIsPermanent(a) ? 1 : 0 ;	}
static void PAtoms()
{
	int i, n ;
	AtomPt a ;
	Size atomTemp = AtomForEach(TempCount) ;
	Size atomPerm = AtomForEach(PermCount) ;
	ShowVersion() ;
	Write("ATOMS:\n") ;
	Write("  Atoms are stored in a %d-entry hash table:\n", atomTableCapacity) ;
	Write("    Current number of permanent atoms in the hash table -> %7ld\n", atomPerm) ;
	Write("    Current number of temporary atoms in the hash table -> %7ld\n", atomTemp) ;
#if 0
	Write("    Current number of functor atoms in the hash table -> %7ld\n",
														AtomForEach(FuncCount)) ;
#endif
	Write("    Current average length of the hash chains -> %5f\n",
				(atomPerm + atomTemp)/(double)atomTableCapacity) ;
	Write("    Current length of the individual hash chains ->") ;
	for( i = 0 ; i < atomTableCapacity ; i++ ) {
		n = 0 ;
		doseq(a, atomTable[i], a->nextHash) n++ ;
		Write(" %d", n) ;
	}
	Write("\n") ;

/*	PrintAtomTable() ;
	BasicGC() ;*/
	JumpNext() ;
}

static void PPrintAtomTable()
{
	PrintAtomTable() ;
	JumpNext() ;
}

static void PSaneArity()
{
	if( XTestNat(X0) > maxFunctorArity )
		DatabaseError("Highest functor arity (%d) exceeded", maxFunctorArity) ;
	JumpNext() ;
}

void AtomsInit2()
{
	InstallCBuiltinPred("atoms", 0, PAtoms) ;
	InstallCBuiltinPred("atom_table", 0, PPrintAtomTable) ;
	InstallCBuiltinPred("$$_sane_arity", 1, PSaneArity) ;
}