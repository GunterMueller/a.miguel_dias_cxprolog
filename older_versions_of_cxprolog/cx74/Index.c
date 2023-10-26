/*
 *   This file is part of the CxProlog system

 *   Index.c
 *   by A.Miguel Dias - 1990/01/26
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

#define DOINDEX	1

#define initialIndexTableSize	256


typedef struct
{
	Pt idxArg ;
	ClausePt first ;
	int nClauses ;
	int version ;
} IndexElem, *IndexElemPt ;

#define IsFreeIndexElem(ie)		( (ie)->version != currVersion )

static int indexTableSize ;
static IndexElemPt indexTable ;
static IndexElem lists, vars ; 
static int currVersion, currArg ;

static void InitIndexTable()
{
	register IndexElemPt el ;
	
	dotable(el, indexTable, indexTableSize)
		el->version = 0 ;
	lists.version = 0 ;
	vars.version = 0 ;
	currVersion = 1 ;
}

static void GrowIndexTable()
{
	Release(indexTable) ;
	indexTableSize *= 2 ;
	indexTable = TemporaryAllocate(indexTableSize * WordsOf(IndexElem)) ;
	InitIndexTable() ;
	if( memoryWarnings_flag )
		Warning("Index table size increased from %ld bytes to %ld bytes",
			indexTableSize * sizeof(IndexElem) / 2,
			indexTableSize * sizeof(IndexElem)) ;

}

static void RefreshIndexTable()
{
	if( ++currVersion == 0 )
		InitIndexTable() ;
}

static int Hash(Pt t)
{
	return ( cWord(t) >> 2 ) % indexTableSize ;
}

static IndexElemPt FindEntry(register Pt t)
{
	register int i, h = Hash(t) ;
	
	for( i = h ; i < indexTableSize ; i++ )
		if( IsFreeIndexElem(&indexTable[i]) || indexTable[i].idxArg == t )
			return &indexTable[i] ;
	for( i = 0 ; i < h ; i++ )
		if( IsFreeIndexElem(&indexTable[i]) || indexTable[i].idxArg == t )
			return &indexTable[i] ;
	return nil ;
}

static void InsertIndexElem(IndexElemPt ie, ClausePt cl, Pt t)
{
	if( IsFreeIndexElem(ie) ) {
		ie->idxArg = t ;
		ie->first = cl ;
		ie->nClauses = 1 ;
		ie->version = currVersion ;
	}
	else
		ie->nClauses++ ;
}

static void ListIndexTable()
{
	register IndexElemPt el ;

	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) )
			WriteStd("%s %d\n", TermTypeStr(el->idxArg), el->nClauses) ;
}

static ClausePt FindMatch(ClausePt cl, Pt t)
{
	for( ;
		not (IsVar(ClauseIdxArg(cl,currArg)) || ClauseIdxArg(cl,currArg) == t) ;
			cl = ClauseNext(cl) ) ;
	return cl ;
}

static Hdl GenIndexChain(ClausePt clauses, IndexElemPt ie, int nVarClauses)
{
	int nClauses = ie->nClauses + nVarClauses ;
	ClausePt cl = nVarClauses == 0 ? ie->first
							: FindMatch(clauses, ie->idxArg) ;

	if( nClauses == 1 )
		return ClauseCodeSkipHeader(cl) ;
	else {
		Hdl res = CodeCurr() ;
		Gen1(Try, ClauseCodeSkipHeader(cl)) ;
		while( --nClauses > 1 ) {
			cl = FindMatch(ClauseNext(cl), ie->idxArg) ;
			Gen1(Retry, ClauseCodeSkipHeader(cl)) ;
			if( CodeOverflow() ) return nil ;
		}
		cl = FindMatch(ClauseNext(cl), ie->idxArg) ;
		Gen1(Trust, ClauseCodeSkipHeader(cl)) ;
		return res ;
	}
}

static int UpPower2(int n)
{
	int p = 2 ;

	while( p < n ) p <<= 1 ;
	return p ;
}

static void ListPrologHash(PrologHashTable ph, int size)
{
	PrologHashTable el ;
			
	dotable(el, ph, size)
		if( el->value != nil )
			if( IsAtomic(el->value) ) {
				WriteStd("\t\t") ;
				WriteTermStd(el->value) ;
				WriteStd(" %lx\n", el->address) ;
			}
			else {
				WriteStd("\t\t%s %lx\n",
						FunctorNameArity(cFunctorPt(el->value)),
						el->address) ;
			}
}

static void InitPrologHash(PrologHashElemPt ph, int size)
{
	register PrologHashElemPt el ;
	
	dotable(el, ph, size) {
		el->value = nil ;
		el->address = nil ;
		el->next = nil ;
	}
}

static void InsertPrologHash(PrologHashTable ph, int size, Pt t, Hdl addr)
/* pre: t not in indexTable */
{
	register PrologHashElemPt h = ph + PrologHash(t,size), x ;
	
	if( h->value == nil ) {
		h->value = t ;
		h->address = addr ;
		return ;
	}
	dotable(x, ph, size)
		if( x->value == nil ) {
			x->next = h->next ;
			h->next = x ;
			x->value = t ;
			x->address = addr ;
			return ;
		}
	InternalError("InsertPrologHash") ; 
}

static Hdl GenConstTableCode(ClausePt clauses, int nConsts,
								int nVarClauses, Hdl onlyVarChain)
{
	register IndexElemPt el ;
	int hTSize = UpPower2(nConsts) ;
	PrologHashElemPt hTable ;
	Hdl res = CodeCurr() ;
	
	Gen2(SwitchOnAtomic, hTSize, onlyVarChain) ;
	hTable = (PrologHashElemPt)(CodeCurr()) ;
	CodeSkip(hTSize * WordsOf(PrologHashElem)) ;
	if( CodeOverflow() ) return nil ;
	InitPrologHash(hTable, hTSize) ;

	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) && IsAtomic(el->idxArg) ) {
			if( CodeOverflow() ) return nil ;
			InsertPrologHash(hTable, hTSize, el->idxArg,
									GenIndexChain(clauses, el, nVarClauses)) ;
		}
	return res ;
}

static Hdl GenConstSingleEntryCode(ClausePt clauses, int nVarClauses)
{
	register IndexElemPt el ;
	
	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) && IsAtomic(el->idxArg) )
			return GenIndexChain(clauses, el, nVarClauses) ;
	InternalError("GenConstSingleEntryCode") ;
	return nil ;
}

static Hdl GenStructTableCode(ClausePt clauses, int nStructs,
									int nVarClauses, Hdl onlyVarChain)
{
	register IndexElemPt el ;
	int hTSize = UpPower2(nStructs) ;
	PrologHashElemPt hTable ;
	Hdl res = CodeCurr() ;

	Gen2(SwitchOnStructure, hTSize, onlyVarChain) ;
	hTable = (PrologHashElemPt)(CodeCurr()) ;
	CodeSkip(hTSize * WordsOf(PrologHashElem)) ;
	if( CodeOverflow() ) return nil ;
	InitPrologHash(hTable, hTSize) ;

	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) && IsStruct(el->idxArg) ) {
			if( CodeOverflow() ) return nil ;
			InsertPrologHash(hTable, hTSize, XPt(el->idxArg),
									GenIndexChain(clauses, el, nVarClauses)) ;
		}
	return res ;
}

static Hdl GenStructSingleEntryCode(ClausePt clauses, int nVarClauses)
{
	register IndexElemPt el ;
	
	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) && IsStruct(el->idxArg) )
			return GenIndexChain(clauses, el, nVarClauses) ;
	InternalError("GenStructSingleEntryCode") ;
	return nil ;
}

static Bool GenIndexSegment(int argN, ClausePt clauses, Hdl switchArgs, Hdl ifVarChain)
{
	IndexElemPt ie ;
	register ClausePt cl ;
	int nClauses, nConsts, nStructs ;
	int nListClauses, nVarsClauses ;
	Hdl onlyVarChain ;

redo:
	RefreshIndexTable() ;
	currArg = argN ;	
	nClauses = nConsts = nStructs = 0 ;
	nListClauses = nVarsClauses = 0 ;
	dolist(cl, clauses, ClauseNext(cl)) {
		register Pt t = ClauseIdxArg(cl,currArg) ;		
		if( IsVar(t) ) {
			InsertIndexElem(&vars, cl, cPt(-1)) ;
			nVarsClauses++ ;
		}
		elif( IsStruct(t) ) {
			if( (ie = FindEntry(t)) == nil ) {
				GrowIndexTable() ;
				goto redo ;
			}
			InsertIndexElem(ie, cl, t) ;
			if( ie->nClauses == 1 )
				nStructs++ ;
		}
		elif( IsList(t) ) {
			InsertIndexElem(&lists, cl, TagList(nil)) ;
			nListClauses++ ;
		}
		elif( IsExtra(t) ) {
			InternalError("DoIndex (Extra)") ;
		}
		else {
			if( (ie = FindEntry(t)) == nil ) {
				GrowIndexTable() ;
				goto redo ;
			}
			InsertIndexElem(ie, cl, t) ;
			if( ie->nClauses == 1 )
				nConsts++ ;
		}
		nClauses++ ;
	}
	if( (nClauses - nVarsClauses) < 2
		|| (nClauses - nVarsClauses) < nVarsClauses )
			return false ;

	/* Is onlyVarChain needed? */
	if( nConsts != 1 || nListClauses == 0 || nStructs != 1 )
		onlyVarChain =
			nVarsClauses > 0
				? GenIndexChain(clauses, &vars, 0)
				: cHdl(&PFailAddr) ;

	switchArgs[0] = cPt(
		nConsts == 0
			? onlyVarChain :
		nConsts == 1
			? GenConstSingleEntryCode(clauses, nVarsClauses)
			: GenConstTableCode(clauses, nConsts, nVarsClauses, onlyVarChain)) ;
	if( CodeOverflow() ) return false ;

	switchArgs[1] = cPt(
		nListClauses == 0
			? onlyVarChain
			: GenIndexChain(clauses, &lists, nVarsClauses)) ;
	if( CodeOverflow() ) return false ;

	switchArgs[2] = cPt(
		nStructs == 0
			? onlyVarChain :
		nStructs == 1
			? GenStructSingleEntryCode(clauses, nVarsClauses)
			: GenStructTableCode(clauses, nStructs, nVarsClauses, onlyVarChain)) ;
	if( CodeOverflow() ) return false ;

	switchArgs[3] = cPt(ifVarChain) ;

	return true ;
}

static void DoNoIndex(PredicatePt pr)
{
	if( PredHasOneClause(pr) )
		BindPredicate(pr, LocalJump, cPt(ClauseCodeSkipHeader(PredClauses(pr)))) ;
	else
		BindPredicate(pr, LocalJump, cPt(ClauseCode(PredClauses(pr)))) ;
}


/* Public */

void InitIndex()
{
	indexTableSize = initialIndexTableSize ;
	indexTable = TemporaryAllocate(indexTableSize * WordsOf(IndexElem)) ;
	InitIndexTable() ;
}

void InvalidIndex(PredicatePt pr)
{
	if( PredHasNoClauses(pr) )
		BindPredicate(pr, Undef, cPt(PredFunctor(pr))) ;
	else	
#if DOINDEX
		BindPredicate(pr, MakeIndex, cPt(pr)) ;
#else
		DoNoIndex(pr) ;
#endif
}

void DoIndex(PredicatePt pr)
{
	Hdl prevSegm, currSegm ;
	Bool hasIndex ;

	if( PredIsNotIndexed(pr) ) {
		DoNoIndex(pr) ;
		return ;
	}

/* First attempt to invalidate the index makes the predicate loose
	the index definitively. But the original index is never deleted,
	(unless the predicate is abolished). */  
	if( PredHasIndex(pr) ) {
		PredIsNotIndexed(pr) = true ;
		DoNoIndex(pr) ;
		return ;
	}

#if 0
	Mesg("INDEX: %s", PredNameArity(pr)) ;
#endif

	CodeReset() ;
	hasIndex = false ;
	currSegm = ClauseCode(PredClauses(pr)) ;

	if( superIndexes_flag && PredArity(pr) >= nIndexedArgs ) {
		prevSegm = currSegm ;
		currSegm = CodeCurr() ;
		Gen4(SwitchOnTerm2, 0, 0, 0, 0) ;
		if( GenIndexSegment(2, PredClauses(pr), CodeCurr() - 4, prevSegm) )
			hasIndex = true ;
		else {
			if( CodeOverflow() ) {
				CodeFinish() ;
				DoNoIndex(pr) ;
				return ;
			}
			CodeSkip(-5) ;
			currSegm = prevSegm ;
		}
	}

	if( superIndexes_flag && PredArity(pr) >= 2 ) {
		prevSegm = currSegm ;
		currSegm = CodeCurr() ;
		Gen4(SwitchOnTerm1, 0, 0, 0, 0) ;
		if( GenIndexSegment(1, PredClauses(pr), CodeCurr() - 4, prevSegm) )
			hasIndex = true ;
		else {
			if( CodeOverflow() ) {
				CodeFinish() ;
				DoNoIndex(pr) ;
				return ;
			}
			CodeSkip(-5) ;
			currSegm = prevSegm ;
		}
	}

	prevSegm = currSegm ;
	currSegm = CodeCurr() ;
	BindPredicate(pr, SwitchOnTerm0, nil) ;
	if( GenIndexSegment(0, PredClauses(pr), PredStartInstArgs(pr), prevSegm) )
		hasIndex = true ;
	else {
		if( not hasIndex || CodeOverflow() ) {
			CodeFinish() ;
			DoNoIndex(pr) ;
			return ;
		}
		BindPredicate(pr, LocalJump, cPt(prevSegm)) ;
	}

	if( CodeSize() > 0 ) {
		int i ;
		Gen0(Proceed) ;		/* ListCode requires this as terminator */
		PredIndex(pr) = TemporaryAllocate(CodeSize()) ;
		CopyWordsRelloc(PredIndex(pr), CodeStart(), CodeSize()) ;
		dotimes(i, 4)
			if( InRange(PredStartInstArgs(pr)[i],
						cPt(CodeStart()), cPt(CodeCurr())) )
				PredStartInstArgs(pr)[i] += PredIndex(pr) - CodeStart() ;
	}
	CodeFinish() ;
}
