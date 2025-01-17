/*
 *   This file is part of the CxProlog system

 *   Disassembler.c
 *   by A.Miguel Dias - 1990/01/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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

Hdl DisassembleInst(StreamPt srm, Hdl code)
{
    Pt inst ;
 	CharPt name, types ;

	StreamWrite(srm, "%8lx ", code) ;
    inst = *code++ ;
    if( !GetInstInfo(inst, &name, &types) ) {
        PredicatePt pr = FindCPredByInst(inst) ;
        if( pr == nil ) InternalError("DisassembleInst") ;
        else StreamWrite(srm, "%s\n", PredNameArity(pr)) ;
        return code ;
    }
                                                                                
	StreamWrite(srm, "%s", name) ;
	while( *types != '\0' )
		switch( *types++ ) {
			case '@': {
				StreamWrite(srm, "\n") ;
				return nil ;
			}
			case '.': {
				code++ ;
				break ;
			}
			case 'p': {
				StreamWrite(srm, " %s", PredNameArity(cPredicatePt(*code++))) ;
				break ;
			}
			case 'n': {
				StreamWrite(srm, " %ld", cWord(*code++)) ;
				break ;
			}
			case 'e': {
				StreamWrite(srm, " %ld",
								cWord(*code++) - WordsOf(Environment)) ;
				break ;
			}
			case 'x': {
				StreamWrite(srm, " X%ld", Df(*code++, X)) ;
				break ;
			}
			case 'y': {
				StreamWrite(srm, " Y%ld",
								cWord(*code++) - WordsOf(Environment) - 1) ;
				break ;
			}
			case 'z': {
				StreamWrite(srm, " Z%ld", cWord(*code++)) ;
				break ;
			}
			case 't': {
				StreamWrite(srm, " %s", TermAsStr(*code++)) ;
				break ;
			}
			case 'u': {
				StreamWrite(srm, " %s", UnitName(cUnitPt(*code++))) ;
				break ;
			}
			case 'f': {
				StreamWrite(srm, " %s", FunctorNameArity(cFunctorPt(*code++))) ;
				break ;
			}
			case 'l': {
				Pt chain = *code++ ;
				if( IsFailInst(chain) )
					StreamWrite(srm, " FAIL") ;
				else StreamWrite(srm, " %lx", chain) ;
				break ;
			}
			case 'c': {
				ClausePt cl = cClausePt(*code++) ;
				StreamWrite(srm, " %lx", cl == nil ? nil : ClauseCode(cl)) ;
				break ;
			}
			case 'H': {
				Size size = cWord(*code++) ;
				Pt chain = *code++ ;
				PrologHashTable el, ph = (PrologHashTable)code ;
				
				code += size * WordsOf(PrologHashElem) ;
				StreamWrite(srm, " %ld", size) ;
				if( IsFailInst(chain) ) StreamWrite(srm, " FAIL") ;
				else StreamWrite(srm, " %lx", chain) ;

				dotable(el, ph, size)
					if( el->value != nil ) {
						if( IsAtomic(el->value) )
                            StreamWrite(srm, "\n%12c%s %lx (%lx)",
                                ' ',
                                TermAsStr(el->value), el->address, el->next) ;
						else
						 	StreamWrite(srm, "\n%12c%s %lx (%lx)",
									' ',
									FunctorNameArity(cFunctorPt(el->value)),
									el->address, el->next) ;
					}
				break ;
			}
			default: InternalError("DisassembleInst (2)") ;	
		}
	StreamWrite(srm, "\n") ;
	return code ;
}

void Disassemble(StreamPt srm, Hdl code)
{
	while( (code = DisassembleInst(srm, code)) != nil ) ;
}

static void DisassemblePredicate(StreamPt srm, PredicatePt pr)
{
	ClausePt cl ;
	StreamWrite(srm, "---------------------\n") ;
	StreamWrite(srm, "%s", PredNameArity(pr)) ;
	if( PredIsBuiltin(pr) ) StreamWrite(srm, " - builtin") ;
	if( PredIsImported(pr) ) StreamWrite(srm, " - imported") ;
	if( PredIsVisible(pr) ) StreamWrite(srm, " - visible") ;
	if( PredIsPermanent(pr) ) StreamWrite(srm, " - permanent") ;
	if( PredIsNoCurrUnit(pr) ) StreamWrite(srm, " - noCurrUnit") ;
	StreamWrite(srm, "\n\n") ;
	Disassemble(srm, PredCode(pr)) ;
	if( PredIsIndexable(pr) && PredIndex(pr) != nil )
		Disassemble(srm, PredIndex(pr)) ;
	StreamWrite(srm, "\n") ;
	doseq(cl, PredClauses(pr), ClauseNext(cl)) {
		Disassemble(srm, ClauseCode(cl)) ;
		StreamWrite(srm, "\n") ;
		if( IsClauseLoop(cl) ) break ;
	}
}


/* CXPROLOG C'BUILTINS */

static Size DisassembleFunctor(FunctorPt f)
{
	PredicatePt pr ;
	if( (pr = FindPredicate(f)) != nil ) {
		DisassemblePredicate(currOut, pr) ;
		return 1 ;
	}
	return 0 ;
}
static void PCode()
{
	ForEachInSpec(X0, DisassembleFunctor) ;
	JumpNext()
}

void DisassemblerInit()
{
	InstallCBuiltinPred("code", 1, PCode) ;
}
