/*
 *   This file is part of the CxProlog system

 *   Disassembler.c
 *   by A.Miguel Dias - 1990/01/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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


static Bool IsFailInst(Pt p)
{
	return p != nil && *cHdl(p) == Fail ;
}

Hdl DisassembleInst(Hdl code)
{
	Pt inst ;
	CharPt name, types ;

	Write("%8lx ", code) ;
	inst = *code++ ;
	if( not GetInstInfo(inst, &name, &types) ) {
		CharPt s = CPredNameArity(inst) ;
		if( s == nil ) InternalError("DisassembleInst") ;
		else Write("%s\n", s) ;
		return code ;
	}

	Write("%s", name) ;
	while( *types != '\0' )
		switch( *types++ ) {
			case '@': {
				Write("\n") ;
				return nil ;
			}
			case '.': {
				code++ ;
				break ;
			}
			case 'p': {
				Write(" %s", UPredNameArity(cPredicatePt(*code++))) ;
				break ;
			}
			case 'n': {
				Write(" %ld", cWord(*code++)) ;
				break ;
			}
			case 'e': {
				Write(" %ld", cWord(*code++) - WordsOf(Environment)) ;
				break ;
			}
			case 'x': {
				Write(" X%ld", Df(*code++, X)) ;
				break ;
			}
			case 'y': {
				Write(" Y%ld", cWord(*code++) - WordsOf(Environment) - 1) ;
				break ;
			}
			case 't': {
				Write(" %s", TermAsStr(*code++)) ;
				break ;
			}
			case 'u': {
				Write(" %s", UnitName(cUnitPt(*code++))) ;
				break ;
			}
			case 'f': {
				Write(" %s", FunctorNameArity(cFunctorPt(*code++))) ;
				break ;
			}
			case 'l': {
				Pt chain = *code++ ;
				if( IsFailInst(chain) )
					Write(" FAIL") ;
				else Write(" %lx", chain) ;
				break ;
			}
			case 'c': {
				ClausePt cl = cClausePt(*code++) ;
				Write(" %lx", cl == nil ? nil : ClauseCode(cl)) ;
				break ;
			}
			case 'H': {
				Size size = cWord(*code++) ;
				Pt chain = *code++ ;
				PrologHashTable el, ph = (PrologHashTable)code ;
				
				code += size * WordsOf(PrologHashElem) ;
				Write(" %ld", size) ;
				if( IsFailInst(chain) ) Write(" FAIL") ;
				else Write(" %lx", chain) ;

				dotable(el, ph, size)
					if( el->value != nil )
						if( IsAtomic(el->value) )
							Write("\n\t\t%s %lx (%lx)",
								TermAsStr(el->value), el->address, el->next) ;
						else
						 	Write("\n\t\t%s %lx (%lx)",
									FunctorNameArity(cFunctorPt(el->value)),
									el->address, el->next) ;
				break ;
			}
		
			default: Default("DisassembleInst (2)") ;	
		}
	Write("\n") ;
	return code ;
}

void Disassemble(Hdl code)
{
	while( (code = DisassembleInst(code)) != nil ) ;
}

static void DisassemblePredicate(PredicatePt pr)
{
	ClausePt cl ;
	Write("---------------------\n") ;
	Write("%s", UPredNameArity(pr)) ;
	if( PredIsBuiltin(pr) ) Write(" - builtin") ;
	if( PredIsVisible(pr) ) Write(" - visible") ;
	if( PredIsPermanent(pr) ) Write(" - permanent") ;
	if( PredIsNoCurrUnit(pr) ) Write(" - noCurrUnit") ;
	Write("\n\n") ;
	Disassemble(PredCode(pr)) ;
	if( PredIsIndexable(pr) && PredIndex(pr) != nil )
		Disassemble(PredIndex(pr)) ;
	Write("\n") ;
	dolist(cl, PredClauses(pr), ClauseNext(cl)) {
		Disassemble(ClauseCode(cl)) ;
		Write("\n") ;
		if( ClauseNext(cl) == cl )	/* Handles patched predicates */
			break ;
	}
}


/* CXPROLOG C'BUILTINS */

static Size DisassembleFunctor(FunctorPt f, Pt x)
{
	PredicatePt pr ;
	if( (pr = FindPredicate(f)) != nil ) {
		DisassemblePredicate(pr) ;
		return 1 ;
	}
	return 0 ;
}
static void PCode()
{
	Spec(X0, DisassembleFunctor, tNilAtom) ;
	JumpNext()
}

void DisassemblerInit()
{
	InstallCBuiltinPred("code", 1, PCode) ;
}
