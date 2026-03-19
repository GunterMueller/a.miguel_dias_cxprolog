/*
 *   This file is part of the CxProlog system

 *   Unify.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Unify_
#define _Unify_

Bool UnifyWithAtomic(Pt a, Pt c) ;
Bool Unify(Pt t1, Pt t2) ;
Bool Equal(Pt t1, Pt t2) ;
Bool RawUnify(Pt t1, Pt t2) ;

#endif
