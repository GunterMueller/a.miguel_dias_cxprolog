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

#define FreeIndexElem(ie)		( (ie)->version == 0 )
#define InitIndexElem(ie)		( (ie)->nClauses = 0, (ie)->version = 0 )

static int indexTableSize ;
static IndexElemPt indexTable ;

static void InitIndexTable()
{
	register IndexElemPt el ;
	
	dotable(el, indexTable, indexTableSize)
		InitIndexElem(el) ;
}

static void IndexTableCountClauses(int *nConsts, int *nCClauses, int *nStructs, int *nSClauses)
{
	register IndexElemPt el ;
	
	*nConsts = *nCClauses = *nStructs = *nSClauses = 0 ;
	dotable(el, indexTable, indexTableSize)
		if( not FreeIndexElem(el) )
			if( IsStruct(el->idxArg) ) {
				(*nStructs)++ ;
				*nSClauses += el->nClauses ;
			}
			else {
				(*nConsts)++ ;
				*nCClauses += el->nClauses ;
			}
}

static int Hash(Pt t)
{
	return ( cWord(t) >> 2 ) % indexTableSize ;
}

static int FindEntry(Pt t)
{
	register int i, h = Hash(t) ;
	
	for( i = h ; i < indexTableSize ; i++ )
		if( FreeIndexElem(&indexTable[i]) || indexTable[i].idxArg == t ) return i ;
	for( i = 0 ; i < h ; i++ )
		if( FreeIndexElem(&indexTable[i]) || indexTable[i].idxArg == t ) return i ;
	return -1 ;
}

static void InsertIndexElem(IndexElemPt ie, ClausePt cl, Pt t)
{
	if( FreeIndexElem(ie) ) {
		ie->idxArg = t ;
		ie->first = cl ;
	}
	ie->nClauses++ ;
	ie->version = 1 ;
}

static Bool InsertIndexTable(ClausePt cl, Pt t)
{
	int i ;

	if( (i = FindEntry(t)) == -1 )
		return false ;
	else InsertIndexElem(&indexTable[i], cl, t) ;
	return true ;
}

static void ListIndexTable()
{
	register IndexElemPt el ;

	dotable(el, indexTable, indexTableSize)
		if( not FreeIndexElem(el) )
			WriteStd("%s %d\n" ,TermTypeStr(el->idxArg), el->nClauses) ;
}

static ClausePt FindMatch(ClausePt cl, Pt t)
{
	for( ;
		not (IsVar(ClauseIdxArg(cl,0)) || ClauseIdxArg(cl,0) == t) ;
			cl = ClauseNext(cl) ) ;
	return cl ;
}

static Hdl GenIndexChain(PredicatePt pr, IndexElemPt ie, int nVarClauses)
{
	int nClauses = ie->nClauses + nVarClauses ;
	ClausePt cl = nVarClauses == 0 ? ie->first
							: FindMatch(PredFirstClause(pr), ie->idxArg) ;

	if( nClauses == 1 )
		return ClauseCodeSkipHeader(cl) ;
	else {
		Hdl res = CodeCurr() ;
		Gen1(Try, ClauseCodeSkipHeader(cl)) ;
		while( --nClauses > 1 ) {
			cl = FindMatch(ClauseNext(cl), ie->idxArg) ;
			Gen1(Retry, ClauseCodeSkipHeader(cl)) ;
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

static Hdl GenConstTableCode(PredicatePt pr, int nConsts, int nVarClauses, Hdl onlyVarChain)
{
	register IndexElemPt el ;
	int hTSize = UpPower2(nConsts) ;
	PrologHashElemPt hTable ;
	Hdl res = CodeCurr() ;
	
	Gen2(SwitchOnAtomic, hTSize, onlyVarChain) ;
	hTable = (PrologHashElemPt)(CodeCurr()) ;
	CodeSkip(hTSize * WordsOf(PrologHashElem)) ;
	InitPrologHash(hTable, hTSize) ;
	dotable(el, indexTable, indexTableSize)
		if( not FreeIndexElem(el) && IsAtomic(el->idxArg) ) {
			if( CodeOverflow() ) return res ;
			InsertPrologHash(hTable, hTSize, el->idxArg,
									GenIndexChain(pr, el, nVarClauses)) ;
			InitIndexElem(el) ;
		}
	return res ;
}

static Hdl GenConstSingleEntryCode(PredicatePt pr, int nVarClauses)
{
	register IndexElemPt el ;
	
	dotable(el, indexTable, indexTableSize)
		if( not FreeIndexElem(el) && IsAtomic(el->idxArg) ) {
			Hdl res = GenIndexChain(pr, el, nVarClauses) ;
			InitIndexElem(el) ;
			return res  ;
		}
	InternalError("GenConstSingleEntryCode") ;
}

static Hdl GenStructTableCode(PredicatePt pr, int nStructs, int nVarClauses, Hdl onlyVarChain)
{
	register IndexElemPt el ;
	int hTSize = UpPower2(nStructs) ;
	PrologHashElemPt hTable ;
	Hdl res = CodeCurr() ;

	Gen2(SwitchOnStructure, hTSize, onlyVarChain) ;
	hTable = (PrologHashElemPt)(CodeCurr()) ;
	CodeSkip(hTSize * WordsOf(PrologHashElem)) ;
	InitPrologHash(hTable, hTSize) ;
	dotable(el, indexTable, indexTableSize)
		if( not FreeIndexElem(el) && IsStruct(el->idxArg) ) {
			if( CodeOverflow() )
				return res ;
			InsertPrologHash(hTable, hTSize, XPt(el->idxArg),
									GenIndexChain(pr, el, nVarClauses)) ;
			InitIndexElem(el) ;
		}
	return res ;
}

static Hdl GenStructSingleEntryCode(PredicatePt pr, int nVarClauses)
{
	register IndexElemPt el ;
	
	dotable(el, indexTable, indexTableSize)
		if( not FreeIndexElem(el) && IsStruct(el->idxArg) ) {
			Hdl res = GenIndexChain(pr, el, nVarClauses) ;
			InitIndexElem(el) ;
			return res  ;
		}
	InternalError("GenStructSingleEntryCode") ;
}

static void DoNoIndex(PredicatePt pr)
{
	if( PredHasOneClause(pr) )
		BindPredicate(pr, LocalJump, cPt(ClauseCodeSkipHeader(PredFirstClause(pr)))) ;
	else
		BindPredicate(pr, LocalJump, cPt(ClauseCode(PredFirstClause(pr)))) ;
	PredHasIndex(pr) = false ;
}


/* Public */

void InitIndex()
{
	indexTableSize = initialIndexTableSize ;
	indexTable = TemporaryAllocate(indexTableSize * WordsOf(IndexElem)) ;
	InitIndexTable() ;
}

static void ReinitIndex()
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
	IndexElem lists, vars ;
	ClausePt cl ;
	int i, noVar, nNoVarClauses, nConsts, nCClauses, nStructs, nSClauses, nVarClauses ;
	Hdl index, indexPt, h, onlyVarChain ;
	Int offset ;
	Bool onlyVarChainNeeded ;
	
	if( PredIsNotIndexed(pr) ) {
		DoNoIndex(pr) ;
		return ;
	}

/* First attempt to invalidate the index makes the predicate loose
		the index definitively. But the original index is never deleted. */  
	if( PredHasIndex(pr) ) {
		PredIsNotIndexed(pr) = true ;
		DoNoIndex(pr) ;
		return ;
	}

redo:
	InitIndexElem(&vars) ;
	InitIndexElem(&lists) ;
	dolist(cl, PredFirstClause(pr), ClauseNext(cl)) {
		register Pt t = ClauseIdxArg(cl,0) ;		
		if( IsVar(t) ) InsertIndexElem(&vars, cl, cPt(-1)) ;
		elif( IsList(t) ) InsertIndexElem(&lists, cl, TagList(nil)) ;
		elif( IsExtra(t) ) InternalError("DoIndex (Extra)") ;
		elif( not InsertIndexTable(cl, t) ) {
			ReinitIndex() ;
			goto redo ;
		}
	}

	IndexTableCountClauses(&nConsts, &nCClauses, &nStructs, &nSClauses) ;
	nVarClauses = vars.nClauses ;
	nNoVarClauses = nCClauses + nSClauses + lists.nClauses ;
	onlyVarChainNeeded = nConsts != 1 || lists.nClauses == 0 || nStructs != 1 ;
	if( nVarClauses > nNoVarClauses || nNoVarClauses == 1 )
		goto doNoIndex ;

	CodeReset() ;
	if( onlyVarChainNeeded )
		onlyVarChain =
			nVarClauses > 0
				? GenIndexChain(pr, &vars, 0)
				: cHdl(&PFailAddr) ;		

	BindPredicate(pr, SwitchOnTerm, nil) ;
	PredStartInstArgs(pr)[0] = cPt(
		nConsts == 0
			? onlyVarChain :
		nConsts == 1
			? GenConstSingleEntryCode(pr, nVarClauses)
			: GenConstTableCode(pr, nConsts, nVarClauses, onlyVarChain)) ;
	if( CodeOverflow() ) goto doNoIndex ;

	PredStartInstArgs(pr)[1] = cPt(
		lists.nClauses == 0
			? onlyVarChain
			: GenIndexChain(pr, &lists, nVarClauses)) ;
	if( CodeOverflow() ) goto doNoIndex ;

	PredStartInstArgs(pr)[2] = cPt(
		nStructs == 0
			? onlyVarChain :
		nStructs == 1
			? GenStructSingleEntryCode(pr, nVarClauses)
			: GenStructTableCode(pr, nStructs, nVarClauses, onlyVarChain)) ;
	if( CodeOverflow() ) goto doNoIndex ;

	if( CodeSize() > 0 ) {
		Gen0(Proceed) ;		/* ListCode requires this as terminator */
		index = TemporaryAllocate(CodeSize()) ;
		CopyWordsRelloc(index, codeBuffer, CodeSize()) ;
		dotimes(i, 3)
			if( InRange(PredStartInstArgs(pr)[i], cPt(codeBuffer), cPt(codePt)) )
				PredStartInstArgs(pr)[i] += index - codeBuffer ;
	}
	PredHasIndex(pr) = true ;
	return ;

doNoIndex:
	InitIndexTable() ;
	DoNoIndex(pr) ;
}
