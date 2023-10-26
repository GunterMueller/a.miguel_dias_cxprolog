/*
 *   This file is part of the CxProlog system

 *   FileSys.h
 *   by A.Miguel Dias - 2002/01/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _FileSys_
#define _FileSys_

Pt PathNameListFromAtom(AtomPt s) ;
AtomPt PathNameListToAtom(Pt list) ;
Bool IsAbsoluteFileName(AtomPt at) ;
AtomPt AbsoluteFileName(AtomPt at) ;
void GoHome(void) ;
void FileSysInit(void) ;

#endif
