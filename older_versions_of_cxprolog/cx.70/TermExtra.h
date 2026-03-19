/*
 *   This file is part of the CxProlog system

 *   TermExtra.h
 *   by A.Miguel Dias - 1999/12/30
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _TermExtra_
#define _TermExtra_

Pt MakeExtra(CharPt str, int subcode) ;
int GetExtraSize(Pt term) ;
Pt MakeExtraPermanent(Pt t) ;
CharPt XExtraName(Pt t) ;
Bool EqExtra(Pt t1, Pt t2) ;
void WriteExtra(Pt t) ;

/* add here the subTags of the new extra primitive types */
#define textSubTag	33


#endif
