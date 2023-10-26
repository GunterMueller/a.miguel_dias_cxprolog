/*
 *   This file is part of the CxProlog system

 *   ImperativeVar.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _ImperativeVar_
#define _ImperativeVar_

void IVarConstSet(AtomPt atom, Pt value) ;
void IVarSet(AtomPt atom, Pt value) ;
Pt IVarGet(AtomPt atom) ;
AtomPt IVarWith(Pt t) ;
void IVarsAtomGCMark(void) ;
void IVarsInit(void) ;

#endif
