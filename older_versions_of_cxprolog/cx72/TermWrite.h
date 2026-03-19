/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _TermWrite_
#define _TermWrite_

CharPt Quote(CharPt a) ;
CharPt WriteTermToStr(Pt term) ;
void WriteTerm(Pt term) ;
void WriteTermStd(Pt term) ;
void WriteDepth(int termDepth, int listLength) ;
void InitTermWrite(void) ;

#endif
