/*
 *   This file is part of the CxProlog system

 *   Unify.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Unify_
#define _Unify_

Bool UnifyWithNumber(Pt t, Pt numb) ;
Bool UnifyWithAtomic(Pt t, Pt at) ;
Bool Unify(Pt t1, Pt t2) ;
Bool Equal(Pt t1, Pt t2) ;
Bool RawUnify(Pt t1, Pt t2) ;
int Compare(Pt t1, Pt t2) ;
void UnifyInit(void) ;

#endif
