/*
 *   This file is part of the CxProlog system

 *   TermRead.h
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _TermRead_
#define _TermRead_

Pt ReadTerm(void) ;
void SeeStr(CharPt s) ;
void SeenStr(void) ;
Pt ReadTermFromStr(CharPt s) ;
void TermReadInit(void) ;

#endif
