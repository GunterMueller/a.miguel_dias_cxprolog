/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _TermWrite_
#define _TermWrite_

CharPt SubtermAsStr(Pt subterm, Pt term) ;
CharPt TermAsStr(Pt term) ;
CharPt TermsAsStr(Pt list) ;
void SetWriteDepth(Size termDepth, Size listLength) ;
void TermWriteInit(void) ;

#endif
