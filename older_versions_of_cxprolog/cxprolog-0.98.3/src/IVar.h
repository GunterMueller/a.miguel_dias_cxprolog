/*
 *   This file is part of the CxProlog system

 *   ImperativeVar.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _ImperativeVar_
#define _ImperativeVar_

void IVarSetAtomic(AtomPt atom, Pt atomic) ;
void IVarSetCopy(AtomPt atom, Pt value) ;
void IVarSetRef(AtomPt atom, Pt value) ;
Pt IVarGet(AtomPt atom) ;
void IVarReversibleRestore(AtomPt atom, Pt oldValue) ;
void IVarDelete(AtomPt atom) ;

/*
void IVarForceSet(AtomPt atom, Pt value, Bool cons) ;
*/

void IVarsWithWrite(Pt t) ;
void IVarsInit(void) ;

#endif
