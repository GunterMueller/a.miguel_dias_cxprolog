/*
 *   This file is part of the CxProlog system

 *   Extra.h
 *   by A.Miguel Dias - 1999/12/30
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Extra_
#define _Extra_

int GetExtraSize(Pt term) ;
Pt MakeExtraPermanent(Pt t) ;
Bool EqExtra(Pt t1, Pt t2) ;
CharPt XExtraAsStr(Pt t) ;


#endif
