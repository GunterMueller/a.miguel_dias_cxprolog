/*
 *   This file is part of the CxProlog system

 *   GCollection.h
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _GCollection_
#define _GCollection_

void InstallBasicGCHandler(CharPt name, Fun p) ;

void BasicGCMarkRange(Hdl a, Hdl z) ;
void BasicGCAddDelta(Size size) ;
void BasicGC(void) ;

void GCollectionInit(void) ;

#endif
