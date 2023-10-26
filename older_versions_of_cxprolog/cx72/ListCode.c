/*
 *   This file is part of the CxProlog system

 *   ListCode.c
 *   by A.Miguel Dias - 1990/01/20
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

static Bool IsFailInst(Pt p)
{
	return p != nil && *cHdl(p) == PFailAddr ;
}

static void ListCode(Hdl theCode)
{
	Pt currInst ;
	CharPt name, types ;

	CodeStartReading(theCode) ;
	for(;;) {
		Write("%8lx ", CodeCurr()) ;
		currInst = GetCode() ;

		if( not GetInstInfo(currInst, &name, &types) ) {
			Write("%s\n", CPredNameArity(currInst)) ;
			continue ;
		}

		Write("%s", name) ;
		while( *types != '\0' )
			switch( *types++ ) {
				case '@': {
					Write("\n") ;
					return ;
				}
				case '!': {
					/* do nothing */
					break ;
				}
				case '.': {
					GetCode() ;
					break ;
				}
				case 'p': {
					Write(" %s", PredNameArity(cPredicatePt(GetCode()))) ;
					break ;
				}
				case 'n': {
					Write(" %ld", cInt(GetCode())) ;
					break ;
				}
				case 'e': {
					Write(" %ld", cInt(GetCode()) - WordsOf(Environment)) ;
					break ;
				}
				case 'x': {
					Write(" X%ld", cHdl(GetCode()) - X) ;
					break ;
				}
				case 'y': {
					Write(" Y%ld", -cInt(GetCode()) - WordsOf(Environment) - 1) ;
					break ;
				}
				case 't': {
					Write(" ") ;
					WriteTerm(GetCode()) ;
					break ;
				}
				case 'u': {
					Write(" %s", UnitName(cUnitPt(GetCode()))) ;
					break ;
				}
				case 'f': {
					Write(" %s", FunctorNameArity(cFunctorPt(GetCode()))) ;
					break ;
				}
				case 'l': {
					Pt chain = GetCode() ;
					if( IsFailInst(chain) )
						Write(" FAIL") ;
					else Write(" %lx", chain) ;
					break ;
				}
				case 'c': {
					ClausePt cl = cClausePt(GetCode()) ;
					Write(" %lx", cl == nil ? nil : ClauseCode(cl)) ;
					break ;
				}
				case 'H': {
					int size = cInt(GetCode()) ;
					Pt chain = GetCode() ;
					PrologHashTable el, ph = (PrologHashTable)CodeCurr() ;
					
					CodeSkip(size * WordsOf(PrologHashElem)) ;
					Write(" %d", size) ;
					if( IsFailInst(chain) ) Write(" FAIL") ;
					else Write(" %lx", chain) ;
					
					dotable(el, ph, size)
						if( el->value != nil )
							if( IsAtomic(el->value) ) {
								Write("\n\t\t") ;
								WriteTerm(el->value) ;
								Write(" %lx (%lx)", el->address, el->next) ;
							}
							else Write("\n\t\t%s %lx (%lx)",
										FunctorNameArity(cFunctorPt(el->value)),
										el->address, el->next) ;
					break ;
				}
			
				default: Default("ListCode") ;	
			}
		Write("\n") ;
	}
}

static Bool HasIndexSegment(PredicatePt pr, Hdl *h)
{
	int i ;
	CharPt name, types ;
	if( PredStartInst(pr) == SwitchOnTerm )
		dotimes(i, 3)
			if( GetInstInfo(*cHdl(PredStartInstArgs(pr)[i]), &name, &types) && types[0] == '!' ) {
				*h = cHdl(PredStartInstArgs(pr)[i]) ;
				return true ;
			}
	return false ;
}

void ListPredicateCode(PredicatePt pr)
{
	ClausePt cl ;
	Hdl h ;

	Write("---------------------\n") ;
	Write("%s\n\n", PredNameArity(pr)) ;
	ListCode(PredCode(pr)) ;
	if( HasIndexSegment(pr, &h) )
		ListCode(h) ;
	Write("\n") ;
	dolist(cl, PredFirstClause(pr), ClauseNext(cl)) {
		ListCode(ClauseCode(cl)) ;
		Write("\n") ;
		if( ClauseNext(cl) == cl )
			break ;
	}
}

void ListAtomCode(AtomPt at)
{
	FunctorPt f ;
	PredicatePt pr ;

	dolist(f, AtomFunctors(at), f->nextArity)
		if( (pr = FindPredicate(f)) != nil )
			ListPredicateCode(pr) ;
}

void ListAllCode()
{
	ForEachAtom(ListAtomCode) ;
}
