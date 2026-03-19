/*
 *   This file is part of the CxProlog system

 *   ImperativeVar.h
 *   by A.Miguel Dias - 2007/12/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2009 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Alias_
#define _Alias_

ExtraPt AliasGet(ExtraTypePt e, AtomPt a) ;
void AliasSet(AtomPt a, VoidPt extra) ;
void AliasUnset(AtomPt a, VoidPt extra) ;
AtomPt AliasSearch(VoidPt extra) ;	/* Slow */
void AliasedWithWrite(Pt t) ;
void AliasInit(void) ;

#endif
