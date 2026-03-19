/*
 *   This file is part of the CxProlog system

 *   Index.c
 *   by A.Miguel Dias - 1990/01/26
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

#define initialIndexTableSize	256

typedef struct
{
	Pt idxArg ;
	ClausePt first ;
	short nClauses ;
	short version ;
} IndexElem, *IndexElemPt ;

#define IsFreeIndexElem(ie)		( (ie)->version != currVersion )

static int indexTableSize ;
static IndexElemPt indexTable ;
static IndexElem lists, vars ; 
static int currVersion, currArg ;

static void IndexTableInit()
{
	register IndexElemPt el ;
	dotable(el, indexTable, indexTableSize)
		el->version = 0 ;
	lists.version = 0 ;
	vars.version = 0 ;
	currVersion = 1 ;
}

static void IndexTableExpand()
{
	int oldSize = indexTableSize * WordsOf(IndexElem) ;
	MemoryGrowWarning("index table", oldSize, oldSize * 2) ;
	PrimitiveRelease(indexTable) ;
	indexTableSize *= 2 ;
	indexTable = PrimitiveAllocate(indexTableSize * WordsOf(IndexElem)) ;
	IndexTableInit() ;
}

static void IndexTableRefresh()
{
	if( ++currVersion == 0 )
		IndexTableInit() ;
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

static void IndexTableList()
{
	register IndexElemPt el ;
	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) )
			Write("%s %d\n", TermTypeStr(el->idxArg), el->nClauses) ;
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
		Hdl res = BufferCurr() ;
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

static Size UpPower2(Size n)
{
	int p = 2 ;
	while( p < n ) p <<= 1 ;
	return p ;
}

static void PrologHashList(PrologHashTable ph, int size)
{
	PrologHashTable el ;			
	dotable(el, ph, size)
		if( el->value != nil )
			if( IsAtomic(el->value) )
				Write("\t\t%s %lx\n", TermAsStr(el->value), el->address) ;
			else
				Write("\t\t%s %lx\n",
						FunctorNameArity(cFunctorPt(el->value)),
						el->address) ;
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

static void PrologHashInsert(PrologHashTable ph, int size, Pt t, Hdl addr)
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
	InternalError("PrologHashInsert") ; 
}

static Hdl GenConstTableCode(ClausePt clauses, int nConsts,
								int nVarClauses, Hdl onlyVarChain)
{
	register IndexElemPt el ;
	Size hTSize = UpPower2(nConsts) ;
	PrologHashElemPt hTable ;
	Hdl res = BufferCurr() ;
	
	Gen2(SwitchOnAtomic, hTSize, onlyVarChain) ;
	hTable = (PrologHashElemPt)(BufferCurr()) ;
	BufferMakeRoom(hTSize * WordsOf(PrologHashElem)) ;
	if( BufferHasMoved() ) return nil ;
	InitPrologHash(hTable, hTSize) ;

	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) && IsAtomic(el->idxArg) ) {
			Hdl chain = GenIndexChain(clauses, el, nVarClauses) ;
			if( BufferHasMoved() ) return nil ;
			PrologHashInsert(hTable, hTSize, el->idxArg, chain) ;
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
	Size hTSize = UpPower2(nStructs) ;
	PrologHashElemPt hTable ;
	Hdl res = BufferCurr() ;

	Gen2(SwitchOnStructure, hTSize, onlyVarChain) ;
	hTable = (PrologHashElemPt)(BufferCurr()) ;
	BufferMakeRoom(hTSize * WordsOf(PrologHashElem)) ;
	if( BufferHasMoved() ) return nil ;
	InitPrologHash(hTable, hTSize) ;

	dotable(el, indexTable, indexTableSize)
		if( not IsFreeIndexElem(el) && IsStruct(el->idxArg) ) {
			Hdl chain = GenIndexChain(clauses, el, nVarClauses) ;
			if( BufferHasMoved() ) return nil ;
			PrologHashInsert(hTable, hTSize, XPt(el->idxArg), chain) ;
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
	IndexTableRefresh() ;
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
				IndexTableExpand() ;
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
				IndexTableExpand() ;
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
				: &Fail ;

	switchArgs[0] = cPt(
		nConsts == 0
			? onlyVarChain :
		nConsts == 1
			? GenConstSingleEntryCode(clauses, nVarsClauses)
			: GenConstTableCode(clauses, nConsts, nVarsClauses, onlyVarChain)) ;

	switchArgs[1] = cPt(
		nListClauses == 0
			? onlyVarChain
			: GenIndexChain(clauses, &lists, nVarsClauses)) ;

	switchArgs[2] = cPt(
		nStructs == 0
			? onlyVarChain :
		nStructs == 1
			? GenStructSingleEntryCode(clauses, nVarsClauses)
			: GenStructTableCode(clauses, nStructs, nVarsClauses, onlyVarChain)) ;

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

void IndexInit()
{
	indexTableSize = initialIndexTableSize ;
	indexTable = PrimitiveAllocate(indexTableSize * WordsOf(IndexElem)) ;
	IndexTableInit() ;
}

void InvalidIndex(PredicatePt pr)
{
	if( PredHasClauses(pr) )
		BindPredicate(pr, MakeIndex, cPt(pr)) ;
	else
		BindPredicate(pr, Undef, cPt(PredFunctor(pr))) ;
}

void DoIndex(PredicatePt pr)
{
	Hdl prevSegm, currSegm ;
	Bool hasIndex ;

	if( indexParams_flag == 0 || not PredIsIndexable(pr) )
		PredIndexMade(pr) = true ;

/* The first attempt to invalidate the index makes the predicate loose
	the index definitively. But the original index is never deleted,
	(unless the predicate is abolished). */  
	if( PredIndexMade(pr) ) {
		PredIsIndexable(pr) = false ;
		DoNoIndex(pr) ;
		return ;
	}
	PredIndexMade(pr) = true ;

#if 0
	Mesg("INDEX: %s", UPredNameArity(pr)) ;
#endif

	UseBuffer() ;

redo:
	BufferReset() ;
	BufferRegister() ;
	hasIndex = false ;
	currSegm = ClauseCode(PredClauses(pr)) ;

	if( indexParams_flag >= 3 && PredArity(pr) >= 3 ) {
		prevSegm = currSegm ;
		currSegm = BufferCurr() ;
		Gen4(SwitchOnTerm2, 0, 0, 0, 0) ;
		if( GenIndexSegment(2, PredClauses(pr), BufferCurr() - 4, prevSegm) )
			hasIndex = true ;
		else {
			BufferBack(5) ;
			currSegm = prevSegm ;
		}
		if( BufferHasMoved() ) goto redo ;
	}

	if( indexParams_flag >= 2 && PredArity(pr) >= 2 ) {
		prevSegm = currSegm ;
		currSegm = BufferCurr() ;
		Gen4(SwitchOnTerm1, 0, 0, 0, 0) ;
		if( GenIndexSegment(1, PredClauses(pr), BufferCurr() - 4, prevSegm) )
			hasIndex = true ;
		else {
			BufferBack(5) ;
			currSegm = prevSegm ;
		}
		if( BufferHasMoved() ) goto redo ;
	}

	prevSegm = currSegm ;
	currSegm = BufferCurr() ;
	BindPredicate(pr, SwitchOnTerm0, nil) ;
	if( GenIndexSegment(0, PredClauses(pr), PredStartInstArgs(pr), prevSegm) )
		hasIndex = true ;
	elif( hasIndex )
		BindPredicate(pr, LocalJump, cPt(prevSegm)) ;
	if( BufferHasMoved() ) goto redo ;	

	if( hasIndex ) {
		Gen0(Proceed) ;		/* ListCode requires this as a terminator */
		if( BufferHasMoved() ) goto redo ;	
		if( BufferUsed() > 1 ) {
			int i ;
			PredIndex(pr) = TempBlockAllocate(BufferUsed()) ;
			CopyWordsRelloc(PredIndex(pr), BufferBegin(), BufferUsed()) ;
			dotimes(i, 4) 
				if( InRange(PredStartInstArgs(pr)[i],
							cPt(BufferBegin()), cPt(BufferCurr())) )
					PredStartInstArgs(pr)[i] +=
							PredIndex(pr) - BufferBegin() ;
			}
	}
	else DoNoIndex(pr) ;

	FreeBuffer() ;
}
