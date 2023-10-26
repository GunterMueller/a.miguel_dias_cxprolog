/*
 *   This file is part of the NanoProlog system

 *   ListCode.c
 *   by A.Miguel Dias - 90/1/20
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

 931117: release of version 0.5

*/

#include "NanoProlog.h"

static Hdl codePt ;

#define ResetCode(c)	codePt = (c)
#define GetCode()		*codePt++
#define EndOfClause(i)	( i == Proceed || i == Execute || i == ExecuteVar )

Bool IsFailInst(Hdl p)
{
	return( p != nil && *p == PFailAddr ) ;
}

void ListCode(Hdl theCode)
{
	Pt currInst ;
	char *name, *types ;

	ResetCode(theCode) ;
	do
	{
		printf("%8lx ", codePt) ;
		currInst = GetCode() ;

		if( not GetInstInfo(currInst, &name, &types) )
		{
			printf("%s\n", ProcToName(cProc(currInst))) ;
			continue ;
		}

		printf("%s", name) ;
	while( *types != '\0' )
		switch( *types++ )
		{
			case 'p':
			{
				printf(" %s", PredNameArity(cPredicatePt(GetCode()))) ;
				break ;
			}
	
			case 'n':
			{
				printf(" %ld", cInt(GetCode())) ;
				break ;
			}
	
			case 'e':
			{
				printf(" %ld", cInt(GetCode()) - WordsOf(Environment)) ;
				break ;
			}
	
			case 'x':
			{
				printf(" X%ld", cHdl(GetCode()) - X) ;
				break ;
			}
	
			case 'y':
			{
				printf(" Y%ld", -cInt(GetCode()) - WordsOf(Environment) - 1) ;
				break ;
			}

			case 'c':
			{
				Pt term = GetCode() ;
				
				if( IsAtom(term) ) printf(" '%s'", XAtomName(term)) ;
				elif( IsInt(term) ) printf(" %ld", XInt(term)) ;
				else printf(" %f", XReal(term)) ;
				break ;
			}
	
			case 'm':
			{
				printf(" %s", ModuleName(cModulePt(GetCode()))) ;
				break ;
			}
	
			case 'f':
			{
				printf(" %s", FunctorNameArity(cFunctorPt(GetCode()))) ;
				break ;
			}
	
			case 'l':
			{
				Int i ;
				
				if( IsFailInst(cHdl(i = cInt(GetCode()))) )
										printf(" FAIL") ;
				else printf(" %lx", i) ;
				break ;
			}
	
			case '0':
			{
				GetCode() ;
				break ;
			}
	
			case 'H':
			{
				PrologHashTable el, ph = (PrologHashTable)GetCode() ;
				int size = cInt(GetCode()) ;
				Int i = cInt(GetCode()) ;
				
				printf(" %lx", ph) ;
				printf(" %d", size) ;
				if( IsFailInst(cHdl(i)) ) printf("FAIL") ;
				else printf(" %lx", i) ;
				
				dotable(el, ph, size)
					if( el->value != nil )
						if( IsAtomic(el->value) )
						{
							printf("\n\t\t") ;
							WriteTerm(el->value) ;
							printf(" %lx", el->address) ;
						}
						else printf("\n\t\t%s %lx",
									FunctorNameArity(cFunctorPt(el->value)),
									el->address) ;
				break ;
			}
		
			default: Default("ListCode") ;	
		}
		printf("\n") ;
	}
	while( not EndOfClause(currInst) ) ;
}

void ListClauseCode(Clause *cl)
{
	ListCode(ClauseCode(cl)) ;
}

void ListPredicateCode(Predicate *pr)
{
	if( PredIsC(pr) )
	{
		printf("---------------------\n") ;
		printf("%s -- CC\n", PredNameArity(pr)) ;
	}
	elif( PredNClauses(pr) > 0 )
	{
		Clause *cl ;
		
		printf("---------------------\n") ;
		printf("%s\n", PredNameArity(pr)) ;
		printf("\n") ;
		if( PredHaveIndex(pr) )
		{
			ListCode(PredCode(pr)) ;
			printf("\n") ;
		}
		foreach(cl, PredFirstClause(pr), ClauseNext(cl))
		{
			ListClauseCode(cl) ;
			printf("\n") ;
		}
	}
}

void ListAtomCode(at)
Atom *at ;
{
	Functor *f ;
	
	foreach(f, AtomFunctors(at), f->next)
			ListPredicateCode(FunctorPreds(f)) ;
}

void ListAllCode()
{
	ForEachAtom(ListAtomCode) ;
}