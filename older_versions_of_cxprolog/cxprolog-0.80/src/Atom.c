/*
 *   This file is part of the CxProlog system

 *   Atom.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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


/* ATOMS */

Pt tNilAtom, tEmptyAtom, tEofAtom, tCutAtom, tTrueAtom, tFalseAtom,
	tFailAtom, tOnAtom, tOffAtom,
	tBracketsAtom, tStringAtom, tMinusAtom, tUnderAtom, tDotAtom,
	tEllispisAtom, tErrorAtom, tGoingAtom,
	tKilledAtom, tCompletedAtom, tFailedAtom, tUserAtom ;


#define atomTableSize	256

static AtomPt *atomTable ;

void AtomsInit(void)
{
	register int i ;
	atomTable = PermBlockAllocate(atomTableSize) ;
	dotimes(i, atomTableSize)
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
	tBracketsAtom = MakeAtom("{}") ;
	tStringAtom = MakeAtom("\"\"") ;
	tMinusAtom = MakeAtom("-") ;
	tUnderAtom = MakeAtom("_") ;
	tDotAtom = MakeAtom(".") ;
	tEllispisAtom = MakeAtom("...") ;	
	tErrorAtom = MakeAtom("error") ;
	tGoingAtom = MakeAtom("going") ;
	tKilledAtom = MakeAtom("killed") ;
	tCompletedAtom = MakeAtom("completed") ;
	tFailedAtom = MakeAtom("failed") ;
	tUserAtom = MakeAtom("user") ;
}

AtomPt LookupAtom(CharPt name)
{
	register UCharPt u = cUCharPt(name) ;
	register int slot = 0 ;
	register AtomPt a ;
/* hash function */
	while( *u ) slot += *u++ ;
/* find atom */
	dolist(a, atomTable[slot % atomTableSize], a->nextHash)
		if( EqualStr(AtomName(a), name) ) {
			AtomPermanent(a) = true ;
			return a ;
		}	
/* create new atom */
	a = PermBlockAllocate(WordsOf(Atom) + Words(strlen(name) + 1)) ;
	a->nextHash = atomTable[slot % atomTableSize] ;
	atomTable[slot % atomTableSize] = a ;
	AtomFunctors(a) = nil ;
	strcpy(AtomName(a), name) ;
	AtomPermanent(a) = true ;
	return a ;
}

AtomPt LookupTempAtom(CharPt name)
{
	register UCharPt u = cUCharPt(name) ;
	register int slot = 0 ;
	register AtomPt a ;
	Size size ;
/* hash function */
	while( *u ) slot += *u++ ;
/* find atom */
	dolist(a, atomTable[slot % atomTableSize], a->nextHash)
		if( EqualStr(AtomName(a), name) )
			return a ;	
/* create new atom */
	a = TempBlockAllocate(size = WordsOf(Atom) + Words(strlen(name) + 1)) ;
	AtomsGCAddDelta(size) ;
	a->nextHash = atomTable[slot % atomTableSize] ;
	atomTable[slot % atomTableSize] = a ;
	AtomFunctors(a) = nil ;
	strcpy(AtomName(a), name) ;
	AtomPermanent(a) = false ;
	return a ;
}

Size ForEachAtom(AtomFun p, Pt x)
{
	int i ;
	AtomPt a ;
	Size n = 0 ;
	dotimes(i, atomTableSize)
		dolist(a, atomTable[i], a->nextHash)
			n += p(a, x) ;
	return n ;	
}

void AtomsClearNotMarked()
{
	register int i ;
	register AtomPt *a ;
	int n = 0 ;
	dotimes(i, atomTableSize) {
		for( a = &atomTable[i] ; *a != nil ; ) {
			if( not AtomPermanent(*a) && not AtomMarked(*a) ) {
				AtomPt r = *a ;
#if 0
Warning("Remove atom %s", AtomName(*a)) ;
#endif
				*a = (*a)->nextHash ;
				BlockRelease(r) ;
				n++ ;
			}
			else a = &(*a)->nextHash ;
		}
	}
	if( n > 0 )
		MemoryWarning("%d temporary atoms removed by garbage collector", n) ;
}

Size Spec(Pt t, FunctorFun p, Pt x)
{
	Size n = 0 ;
	t = Drf(t) ;
	if( t == tNilAtom ) ;
	elif( IsAtom(t) ) {
		FunctorPt f ;
		dolist(f, AtomFunctors(XAtom(t)), f->nextArity)
			n += (*p)(f, x) ;
	}
	elif( IsThisStruct(t, slashFunctor) )	
		n += (*p)(XTestFunctor2(XStructArg(t,0), XStructArg(t,1)), x) ;
	elif( IsList(t) ) {
		for( ; IsList(t) ; t = Drf(XListTail(t)) )
			n += Spec(XListHead(t), p, x) ;
		if( t != tNilAtom )
			TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
	}
	else Error("Invalid specification") ;
	return n ;
}

static Size ListAtom(AtomPt a, Pt x)
{
	Write("%s\n", AtomName(a)) ;
	return 0 ;
}
Size AtomsList()
{
	return ForEachAtom(ListAtom, tNilAtom) ;
}

void PrintAtomTable()
{
	int i, j ;
	AtomPt a ;
	for( i = 0 ; i < atomTableSize ; i++ ) {
		Write("%d - ", i) ;
		dolist(a, atomTable[i], a->nextHash)
			Write("%s%s ", AtomPermanent(a) ? " " : "@", AtomName(a)) ;
		Write("\n") ;
	}
}



/* FUNCTORS */

FunctorPt commaFunctor, semicolonFunctor, neckFunctor, commandFunctor,
		slashFunctor, formatFunctor, listFunctor, cutFunctor, eqFunctor,
		barFunctor, hifenFunctor, varFunctor, primitiveFunctor,
		unitParamFunctor ;

static FunctorPt NewFunctor(AtomPt atom, int arity)
{
	register FunctorPt f ;
	
	if( arity > maxArity )
		DatabaseError("Highest arity (%d) exceeded on functor '%s/%d'",
							maxArity, AtomName(atom), arity) ;

	f = PermBlockAllocate(WordsOf(Functor)) ;
	FunctorAtom(f) = atom ;
	FunctorArity(f) = arity ;
	FunctorPreds(f) = nil ;
	FunctorIsBuiltin(f) = false ;
	FunctorIsMeta(f) = false ;
	FunctorIsSpy(f) = false ;
	AtomPermanent(atom) = true ;
	return f ;
}

Bool FunctorCheck(VoidPt ref)
{
	register int i ;
	register AtomPt a ;
	register FunctorPt f ;
	dotimes(i, atomTableSize)
		dolist(a, atomTable[i], a->nextHash)
			dolist(f, AtomFunctors(a), f->nextArity)
				if( f == ref ) return true ;
	return false ;
}

FunctorPt LookupFunctor(AtomPt atom, int arity)
{
	register FunctorPt f ;
	dolist(f, AtomFunctors(atom), f->nextArity)
		if( FunctorArity(f) == arity ) return f ;	
	f = NewFunctor(atom, arity) ;
	f->nextArity = AtomFunctors(atom) ;
	return AtomFunctors(atom) = f ;
}

FunctorPt LookupFunctorByName(CharPt name, int arity)
{
	return LookupFunctor(LookupAtom(name), arity) ;
}

CharPt FunctorNameArity(FunctorPt f)
{
	sprintf(retBuffer+(retBufferSize/2), "%s/%d", FunctorName(f), FunctorArity(f)) ;
	return retBuffer+(retBufferSize/2) ;
}

Size ForEachFunctor(FunctorFun p, Pt x)
{
	int i ;
	AtomPt a ;
	FunctorPt f ;
	Size n = 0 ;
	dotimes(i, atomTableSize)
		dolist(a, atomTable[i], a->nextHash)
			dolist(f, AtomFunctors(a), f->nextArity)
				n += p(f, x) ;
	return n ;	
}

Size SpyFunctor(FunctorPt f, Pt x)
{
	Bool b = x == tTrueAtom ;	
	if( FunctorIsBuiltin(f) ) {
		if( b ) 
			WriteStd("Cannot place spy-point on builtin '%s'.\n",
										FunctorNameArity(f)) ;
		return 0 ;
	}
	if( FunctorIsSpy(f) != b && FunctorPreds(f) != nil ) {
		CharPt mesg = b ? "Spy-point set on functor %s.\n"
						: "Spy-point on %s removed.\n" ;
		WriteStd(mesg, FunctorNameArity(f)) ;
		FunctorIsSpy(f) = b ;
		return 1 ;
	}
	return 0 ;
}

void NoSpyAll()
{
	if( ForEachFunctor(SpyFunctor, false) > 0 )
		WriteStd("All spypoints removed.\n") ;
}

static Size WriteFunctorSpyPoint(FunctorPt f, Pt x)
{
	if( FunctorIsSpy(f) ) {
		WriteStd("              %s\n", FunctorNameArity(f)) ;
		return 1 ;
	}
	return 0 ;
}
void WriteSpyPoints()
{
	WriteStd("Spy-points set on functors:\n") ;
	if( ForEachFunctor(WriteFunctorSpyPoint, tNilAtom) == 0 )
		WriteStd("           %% none %%\n") ;
}

void FunctorsInit()
{
	commaFunctor = LookupFunctorByName(",", 2) ;
	semicolonFunctor = LookupFunctorByName(";", 2) ;
	neckFunctor = LookupFunctorByName(":-", 2) ;
	commandFunctor = LookupFunctorByName(":-", 1) ;
	slashFunctor = LookupFunctorByName("/", 2) ;
	formatFunctor = LookupFunctorByName(":", 2) ;
	listFunctor = LookupFunctorByName(".", 2) ;
	cutFunctor = LookupFunctorByName("$$_cut", 1) ;
	eqFunctor = LookupFunctorByName("=", 2) ;
	barFunctor = LookupFunctorByName("|", 2) ;
	hifenFunctor = LookupFunctorByName("-", 2) ;
	varFunctor = LookupFunctorByName("$VAR", 1) ;
	primitiveFunctor = LookupFunctorByName("$$_primitive", 1) ;
	unitParamFunctor = LookupFunctorByName("$$_unit_parameter", 1) ;
}

/* CXPROLOG C'BUILTINS */

static Size atomPerm, atomTemp ;

static Size AtomCount(AtomPt a, Pt x)
{
	if( AtomPermanent(a) ) atomPerm++ ;
	else atomTemp++ ;
	return 0 ;
}
static void PAtoms()
{
	int i, n ;
	AtomPt a ;
	VersionShow() ;
	Write("Atoms:\n") ;
	Write("  ATOMS are stored in a %d-entry hash table:\n",
					atomTableSize) ;
	atomPerm = atomTemp = 0 ;
	ForEachAtom(AtomCount, tNilAtom) ;
	Write("    Current number of permanent atoms in the hash table -> %7ld\n", atomPerm) ;
	Write("    Current number of temporary atoms in the hash table -> %7ld\n", atomTemp) ;
	Write("    Current average length of the hash chains -> %5f\n",
				(atomPerm + atomTemp)/(double)atomTableSize) ;
	Write("    Current length of the individual hash chains ->") ;
	for( i = 0 ; i < atomTableSize ; i++ ) {
		n = 0 ;
		dolist(a, atomTable[i], a->nextHash) n++ ;
		Write(" %d", n) ;
	}
	Write("\n") ;

/*	PrintAtomTable() ;
	AtomsGC() ;*/
	JumpNext()
}

static void PSaneArity()
{
	if( XTestNat(X0) > maxArity )
		DatabaseError("Highest functor arity (%d) exceeded", maxArity) ;
}

void AtomsInit2()
{
	InstallCBuiltinPred("atoms", 0, PAtoms) ;
	InstallCBuiltinPred("$$_sane_arity", 1, PSaneArity) ;
}
