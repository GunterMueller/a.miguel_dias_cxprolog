/*
 *   This file is part of the NanoProlog system

 *   Index.c
 *   by A.Miguel Dias - 90/1/26
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

 990527: AUTONOINDEX introduced
 931203: macros StartCode and PatchClause changed.
 		 cHdl(0x8ffffff0) is no longer used.
 		 Value(.) -> cWord(.)
 931117: release of version 0.5

*/

#include "NanoProlog.h"

#define DEBUG	0

static Hdl codePt, codeEnd ;

#define StartCode(c, e)		(codePt = c, codeEnd = e)
#define PatchClause(cl)		StartCode(ClauseCode(cl), ClauseCode(cl) + 100)

#define Skip(n)				( codePt += (n) )

#define AddArg(c)			if( codePt >= codeEnd ) Error("Code overflow") ; \
							else *codePt++ = cPt(c) ;

#define AddInst0(c)			if( codePt >= codeEnd ) Error("Code overflow") ; \
							else *codePt++ = cPt(c) ;

#define AddInst1(c1,c2)		if( codePt + 1 >= codeEnd ) Error("Code overflow") ; \
							else { *codePt++ = cPt(c1) ; *codePt++ = cPt(c2) ; }

#define AddInst3(c1,c2,c3,c4)	(*codePt++ = cPt(c1), *codePt++ = cPt(c2),	\
				  				 *codePt++ = cPt(c3), *codePt++ = cPt(c4))



/* INSTALL, DELETE & PATCH CLAUSES */

Clause *InstallClause(PredicatePt pr, Hdl code, int codeSize,
					Pt term, Bool end)
{
	Clause *prev, *next,
		   *this = end ? InstallClauseTail(pr, code, codeSize, term)
					   : InstallClauseHead(pr, code, codeSize, term) ;

	codeEnd = cHdl(0x8ffffff0) ;
	switch( PredNClauses(pr) )
	{
		case 1:
		{
			PatchClause(this) ;
			AddInst1(TryMeElse, nil) ;
			break ;
		}
		
		case 2:
		{
			this = PredFirstClause(pr) ;
 			next = ClauseNext(this) ;
			PatchClause(this) ;
			AddInst1(TryMeElse, ClauseCode(next)) ;
			PatchClause(next) ;
			AddInst1(TrustMe, Nop) ;
			break ;
		}

		default:
		{
			if( end )
			{
				prev = ClausePrev(this) ;
				PatchClause(prev) ;
				AddInst1(RetryMeElse, ClauseCode(this)) ;
				PatchClause(this) ;
				AddInst1(TrustMe, Nop) ;
			}
			else
			{
				next = ClauseNext(this) ;
				PatchClause(this) ;
				AddInst1(TryMeElse, ClauseCode(next)) ;
				PatchClause(next) ;
				AddInst0(RetryMeElse) ;
			}
			break ;
		}
	}
#if DEBUG == 1
	ListPredicateCode(pr) ;
	getchar() ;
#endif
	return( this ) ;
}

void DeleteCode(Clause *cl)
{
	Predicate *pr = cl->owner ;
	Clause *prev = ClausePrev(cl),
		   *next = ClauseNext(cl) ;

	codeEnd = cHdl(0x8ffffff0) ;
	DeleteClause(cl) ;
	if( PredNClauses(pr) == 1 )
	{
		PatchClause(PredFirstClause(pr)) ;
		AddInst1(TryMeElse, &PFailAddr) ;
	}
	elif( PredNClauses(pr) > 1 )
	{
		if( prev == nil )	/* first was removed */
		{
			PatchClause(next) ;
			AddInst0(TryMeElse) ;
		}
		elif( next == nil )	/* last was removed */
		{
			PatchClause(prev) ;
			AddInst1(TrustMe, Nop) ;
		}
		else
		{
			PatchClause(prev) ;
			Skip(1) ;
			AddArg(ClauseCode(next)) ;
		}
	}
}

static void PatchPredicate(char *name, int arity)
{
	Clause *cl ;
	Predicate *pr ;

	codeEnd = cHdl(0x8ffffff0) ;
	pr = LookupPredicate2(name, arity) ;
	if( PredNClauses(pr) == 0 )
		FatalError("Missing patched predicate %s in boot file",
												PredNameArity(pr)) ;
	if( PredNClauses(pr) != 2 )
		FatalError("Patched predicate %s is incorrect in boot file",
												PredNameArity(pr)) ;
	cl = ClauseNext(PredFirstClause(pr)) ;
	PatchClause(cl) ;
	AddInst1(RetryMeElse, ClauseCode(cl)) ;
	NoIndex(pr) ;
}

void PatchPredicates()
{
	PatchPredicate("repeat", 0) ;
	PatchPredicate("$clause", 3) ;
	PatchPredicate("$retract", 2) ;
	PatchPredicate("$user_predicate", 2) ;
	PatchPredicate("$system_predicate", 2) ;
}



/* INDEX */

#define indexTableSize 128

typedef struct
{
	Pt idxArg ;
	Clause *first ;
	int nClauses ;
} IndexElem ;

typedef IndexElem IndexTable[indexTableSize] ;

#define FreeIndexElem(ie)		( (ie)->nClauses == 0 )
#define InitIndexElem(ie)		( (ie)->nClauses = 0 )

static void InitIndexTable(IndexTable table)
{
	register IndexElem *el ;
	
	dotable(el, table, indexTableSize) InitIndexElem(el) ;
}

static void IndexTableCountClauses(IndexTable table, int *nConsts,
						int *nCClauses, int *nStructs, int *nSClauses)
{
	register IndexElem *el ;
	
	*nConsts = *nCClauses = *nStructs = *nSClauses = 0 ;
	dotable(el, table, indexTableSize)
		if( not FreeIndexElem(el) )
			if( IsStruct(el->idxArg) )
			{
				(*nStructs)++ ;
				*nSClauses += el->nClauses ;
			}
			else
			{
				(*nConsts)++ ;
				*nCClauses += el->nClauses ;
			}
}

static int Hash(Pt t)
{
	return( ( cWord(t) >> 2 ) % indexTableSize ) ;
}

static int FindEntry(IndexTable table, Pt t)
{
	int i, h = Hash(t) ;
	
	for( i = h ; i < indexTableSize ; i++ )
		if( FreeIndexElem(&table[i]) || table[i].idxArg == t ) return( i ) ;
	for( i = 0 ; i < h ; i++ )
		if( FreeIndexElem(&table[i]) || table[i].idxArg == t ) return( i ) ;
	return( -1 ) ;
}

static void InsertIndexElem(IndexElem *ie, Clause *cl, Pt t)
{
	if( FreeIndexElem(ie) )
	{
		ie->idxArg = t ;
		ie->first = cl ;
	}
	ie->nClauses++ ;
}

static Bool InsertIndexTable(IndexTable table, Clause *cl, Pt t)
{
	int i ;

	if( (i = FindEntry(table, t)) == -1 ) return( false ) ;
	else InsertIndexElem(&table[i], cl, t) ;
	return( true ) ;
}

static void ListIndexTable(IndexTable table)
{
	register IndexElem *el ;

	dotable(el, table, indexTableSize)
		if( not FreeIndexElem(el) )
			printf("%s %d\n" ,TermTypeStr(el->idxArg), el->nClauses) ;
}

static Clause *FindMatch(Clause *cl, Pt t)
{
	for( ; not (IsVar(cl->idxArg) || cl->idxArg == t) ; cl = ClauseNext(cl) ) ;
	return( cl ) ;
}

static Hdl GenIndexChain(Predicate  *pr, IndexElem *ie, int nCVars)
{
	int nInsts = ie->nClauses + nCVars ;
	Clause *cl = nCVars == 0 ? ie->first
							: FindMatch(PredFirstClause(pr), ie->idxArg) ;

	if( nInsts == 1 ) return( ClauseCode(cl) + clauseHeaderSize ) ;
	else
	{
		Hdl res = codePt ;
		
		AddInst1(Try, ClauseCode(cl) + clauseHeaderSize) ;
		while( --nInsts > 1 )
		{
			cl = FindMatch(ClauseNext(cl), ie->idxArg) ;
			AddInst1(Retry, ClauseCode(cl) + clauseHeaderSize) ;
		}
		cl = FindMatch(ClauseNext(cl), ie->idxArg) ;
		AddInst1(Trust, ClauseCode(cl) + clauseHeaderSize) ;
		return( res ) ;
	}
}

static int UpPower2(int n)
{
	int p = 2 ;

	while( p < n ) p <<= 1 ;
	return( p ) ;
}

static void ListPrologHash(PrologHashTable ph, int size)
{
	PrologHashTable el ;
			
	dotable(el, ph, size)
		if( el->value != nil )
			if( IsAtomic(el->value) )
			{
				printf("\t\t") ;
				WriteTerm(el->value) ;
				printf(" %lx\n", el->address) ;
			}
			else
			{
				printf("\t\t%s %lx\n",
						FunctorNameArity(cFunctorPt(el->value)),
						el->address) ;
			}
}

static void InitPrologHash(PrologHashElem *ph, int size)
{
	register PrologHashElem *el ;
	
	dotable(el, ph, size)
	{
		el->value = nil ;
		el->next = nil ;
	}
}

static void InsertPrologHash(PrologHashTable ph, int size, Pt t, Hdl addr)
/* pre: t not in table */
{
	register PrologHashElem *h = ph + PrologHash(t,size), *x ;
	
	if( h->value == nil )
	{
		h->value = t ;
		h->address = addr ;
		return ;
	}
	dotable(x, ph, size)
		if( x->value == nil )
		{
			x->next = h->next ;
			h->next = x ;
			x->value = t ;
			x->address = addr ;
			return ;
		}
	InternalError("InsertPrologHash") ; 
}

static Hdl GenConstTableCode(Predicate  *pr, IndexTable table, int nConsts,
											int nCVars, Hdl onlyVarChain)
{
	register IndexElem *el ;
	int hTSize = UpPower2(nConsts) ;
	PrologHashElem *hTable = HeapAlloc(hTSize * WordsOf(PrologHashElem), false) ;
	Hdl res = codePt ;
	
	InitPrologHash(hTable, hTSize) ;
	PredHConst(pr) = cHdl(hTable) ;	
	AddInst3(SwitchOnConstant, hTable, hTSize, onlyVarChain) ;
	dotable(el, table, indexTableSize)
		if( not FreeIndexElem(el) && IsAtomic(el->idxArg) )
		{
			InsertPrologHash(hTable, hTSize, el->idxArg,
									GenIndexChain(pr, el, nCVars)) ;
			InitIndexElem(el) ;
		}
	return( res ) ;
}

static Hdl GenStructTableCode(Predicate  *pr, IndexTable table, int nStructs,
											int nCVars, Hdl onlyVarChain)
{
	register IndexElem *el ;
	int hTSize = UpPower2(nStructs) ;
	PrologHashElem *hTable = HeapAlloc(hTSize * WordsOf(PrologHashElem), false) ;
	Hdl res = codePt ;

	InitPrologHash(hTable, hTSize) ;
	PredHStruct(pr) = cHdl(hTable) ;
	AddInst3(SwitchOnStructure, hTable, hTSize, onlyVarChain) ;
	dotable(el, table, indexTableSize)
		if( not FreeIndexElem(el) && IsStruct(el->idxArg) )
		{
			InsertPrologHash(hTable, hTSize, XPt(el->idxArg),
									GenIndexChain(pr, el, nCVars)) ;
			InitIndexElem(el) ;
		}
	return( res ) ;
}

static Hdl GenSingleEntryCode(Predicate  *pr, IndexTable table,
										int nCVars, Bool atomic)
{
	register IndexElem *el ;
	Hdl res ;
	
	dotable(el, table, indexTableSize)
		if( not FreeIndexElem(el) &&
			(atomic ? IsAtomic(el->idxArg) : IsStruct(el->idxArg)) ) break ;
	res = GenIndexChain(pr, el, nCVars) ;
	InitIndexElem(el) ;
	return( res ) ;
}

static Hdl GenOnlyVarChain(Predicate  *pr, IndexElem *vars)
{
	return( GenIndexChain(pr, vars, 0) ) ;
}

#define AUTONOINDEX	1

static IndexTable idx ;

static void SetIndex(Predicate *pr,Hdl idx)
{
	PredCode(pr) = idx ;
	PredHaveIndex(pr) = true ;
}

/* Public */

void InitIndex()
{
	InitIndexTable(idx) ;
}

void InvalidIndex(Predicate *pr)
{
	if( PredHaveIndex(pr) )
	{
#if AUTONOINDEX
		PredHaveIndex(pr) = false ; /* index is not deleted */
		PredNoIndex(pr) = true ;
		if( PredNClauses(pr) == 0 )
			PredCode(pr) = &PFailAddr ;
		else NoIndex(pr) ;
		return ;
#endif
		HeapFree(PredCode(pr)) ;	
		if( PredHConst(pr) != nil )
		{
			HeapFree(PredHConst(pr)) ;
			PredHConst(pr) = nil ;
		}
		if( PredHStruct(pr) != nil )
		{
			HeapFree(PredHStruct(pr)) ;
			PredHStruct(pr) = nil ;
		}
	}
	if( PredNClauses(pr) == 0 )
		PredCode(pr) = &PFailAddr ;
	else PredCode(pr) = PredIdxInst(pr) ;
	PredHaveIndex(pr) = false ;
}

void NoIndex(Predicate *pr)
{
	PredCode(pr) = ClauseCode(PredFirstClause(pr)) ;
	if( PredNClauses(pr) == 1 ) PredCode(pr) += clauseHeaderSize ;
	PredHaveIndex(pr) = false ;
}

void DoIndex(Predicate  *pr)
{
	IndexElem lists, vars ;
	Clause *cl ;
	Pt t ;
	int i, nNoVar, nNoVarClauses, nConsts, nCClauses,
		nStructs, nSClauses, nVarClauses, indexMax ;
	Hdl index = nil, *switchArgs, onlyVarChain ;
	Bool onlyVarChainNeeded ;
	
	if( PredNoIndex(pr) || PredArity(pr) == 0 ) goto noIndex ;
		
	InitIndexElem(&vars) ;
	InitIndexElem(&lists) ;

	foreach(cl, PredFirstClause(pr), ClauseNext(cl))
	{
		t = ClauseIdxArg(cl) ;		
		if( IsVar(t) ) InsertIndexElem(&vars, cl, cPt(-1)) ;
		elif( IsList(t) ) InsertIndexElem(&lists, cl, TagList(nil)) ;
		elif( not InsertIndexTable(idx, cl, t) ) Error("Index table overflow") ;
	}

	IndexTableCountClauses(idx, &nConsts, &nCClauses, &nStructs, &nSClauses) ;
	nVarClauses = vars.nClauses ;
	nNoVarClauses = nCClauses + nSClauses + lists.nClauses ;
	if( nVarClauses >= 2 * nNoVarClauses || nNoVarClauses == 1 )
	{
		InitIndexTable(idx) ;
		goto noIndex ;
	}
	
	onlyVarChainNeeded = nConsts != 1 || lists.nClauses == 0 || nStructs != 1 ;

	indexMax = 2 * (((nConsts > 1) + (nStructs > 1)) * 4 +
						2 * ( onlyVarChainNeeded * nVarClauses +
						nCClauses + nConsts * nVarClauses +
						lists.nClauses + (lists.nClauses > 0) * nVarClauses +
						nSClauses + nStructs * nVarClauses ) + 10) ;
	index = HeapAlloc(indexMax, false) ;
	StartCode(index, index + indexMax) ;
	AddInst1(SwitchOnTerm, cPt(ClauseCode(PredFirstClause(pr)))) ;
	switchArgs = (Hdl *)codePt ;
	Skip(3) ;

	if( onlyVarChainNeeded )
		onlyVarChain =
			nVarClauses > 0
				? GenOnlyVarChain(pr, &vars)
				: cHdl(&PFailAddr) ;  
				
	if( codeEnd - codePt < 10 ) goto noIndex ;
	switchArgs[0] =
		nConsts == 0
			? onlyVarChain :
		nConsts == 1
			? GenSingleEntryCode(pr, idx, nVarClauses, true)
			: GenConstTableCode(pr, idx, nConsts, nVarClauses, onlyVarChain) ;

	if( codeEnd - codePt < 10 ) goto noIndex ;
	switchArgs[1] =
		lists.nClauses == 0
			? onlyVarChain
			: GenIndexChain(pr, &lists, nVarClauses) ;

	if( codeEnd - codePt < 10 ) goto noIndex ;
	switchArgs[2] =
		nStructs == 0
			? onlyVarChain :
		nStructs == 1
			? GenSingleEntryCode(pr, idx, nVarClauses, false)
			: GenStructTableCode(pr, idx, nStructs, nVarClauses, onlyVarChain) ;

	AddInst0(Proceed) ;

	SetIndex(pr, index) ;
#if DEBUG == 1	
	ListPredicateCode(pr) ;
	goto noIndex ;
#endif

	return ;
noIndex:
	if( index != nil ) HeapFree(index) ;
	NoIndex(pr) ;
}
