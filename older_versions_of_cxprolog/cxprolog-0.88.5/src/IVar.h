/*
 *   This file is part of the CxProlog system

 *   ImperativeVar.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _ImperativeVar_
#define _ImperativeVar_

void IVarSet(AtomPt atom, Pt value) ;
void IVarReversibleRestore(AtomPt atom, Pt oldValue) ;
void IVarConstSet(AtomPt atom, Pt value) ;
void IVarForceSet(AtomPt atom, Pt value, Bool cons) ;
Pt IVarGet(AtomPt atom) ;
AtomPt IVarWith(Pt t) ;
void IVarsInit(void) ;

#endif
