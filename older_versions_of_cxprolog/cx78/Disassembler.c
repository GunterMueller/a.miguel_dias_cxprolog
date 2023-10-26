/*
 *   This file is part of the CxProlog system

 *   Disassembler.c
 *   by A.Miguel Dias - 1990/01/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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
				Write(" X%ld", cHdl(*code++) - X) ;
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
				Write(" %d", size) ;
				if( IsFailInst(chain) ) Write(" FAIL") ;
				else Write(" %lx", chain) ;
				
				dotable(el, ph, size)
					if( el->value != nil )
						if( IsAtomic(el->value) )
							Write("\n\t\t%s %lx (%lx)",
								TermAsStr(el->value), el->address, el->next) ;
						else Write("\n\t\t%s %lx (%lx)",
									FunctorNameArity(cFunctorPt(el->value)),
									el->address, el->next) ;
				break ;
			}
		
			default: Default("DisassembleInst") ;	
		}
	Write("\n") ;
	return code ;
}

void Disassemble(Hdl code)
{
	while( (code = DisassembleInst(code)) != nil ) ;
}

void DisassemblePredicate(PredicatePt pr)
{
	ClausePt cl ;
	Write("---------------------\n") ;
	Write("%s\n\n", UPredNameArity(pr)) ;
	Disassemble(PredCode(pr)) ;
	if( PredIsIndexed(pr) && PredIndex(pr) != nil )
		Disassemble(PredIndex(pr)) ;
	Write("\n") ;
	dolist(cl, PredClauses(pr), ClauseNext(cl)) {
		Disassemble(ClauseCode(cl)) ;
		Write("\n") ;
		if( ClauseNext(cl) == cl )	/* This is here for patched predicates */
			break ;
	}
}

void DisassembleAtom(AtomPt at)
{
	FunctorPt f ;
	PredicatePt pr ;
	dolist(f, AtomFunctors(at), f->nextArity)
		if( (pr = FindPredicate(f)) != nil )
			DisassemblePredicate(pr) ;
}

void DisassembleAll()
{
	ForEachAtom(DisassembleAtom) ;
}


/* CXPROLOG C'BUILTINS */

static void PCode()
{
	Pt t = Drf(X0) ;
	if( IsThisStruct(t, slashFunctor) ) {
		PredicatePt pr ;
		if( (pr = FindPredicate(XTestFunctor2(XStructArg(t,0),
											XStructArg(t,1)))) != nil )
			DisassemblePredicate(pr) ;
	}
	else
		DisassembleAtom(XTestAtom(X0)) ;
	JumpNext()
}

void DisassemblerInit()
{
	InstallCBuiltinPred("code", 1, PCode) ;
}
