/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2010 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _TermWrite_
#define _TermWrite_

void TermWriteN(StreamPt srm, Pt term) ;
void TermWriteQ(StreamPt srm, Pt term) ;
void TermWriteP(StreamPt srm, Pt term) ;
void TermWriteD(StreamPt srm, Pt term) ;
void TermWriteC(StreamPt srm, Pt term) ;

CharPt TermAsStrN(Pt term) ;
CharPt TermAsStrQ(Pt term) ;
CharPt SubtermAsStrN(Pt subterm, Pt term) ;
CharPt SubtermAsStrQ(Pt subterm, Pt term) ;
CharPt TermsAsStrN(Pt list) ;
CharPt TermsAsStrQ(Pt list) ;
CharPt TermAsStr(Pt term) ;

CharPt SubtermAsStr(Pt subterm, Pt term) ;
CharPt TermAsStr(Pt term) ;
CharPt TermsAsStr(Pt list) ;

void SetWriteDepth(Size termDepth, Size listLength) ;
void TermWriteInit(void) ;

#endif
