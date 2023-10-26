/*
 *   This file is part of the CxProlog system

 *   GCollection.h
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _GCollection_
#define _GCollection_

void AtomGCMark(AtomPt at) ;
void AtomGCMarkRange(Hdl a, Hdl z) ;
void AtomGCMarkRangeCheck(Hdl a, Hdl z) ;
void AtomsGCAddDelta(Size size) ;
void AtomsGC(void) ;

void InstallAtomGCHandler(Proc p) ;
void GCollectionInit(void) ;

#endif
