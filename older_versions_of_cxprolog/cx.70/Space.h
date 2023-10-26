/*
 *   This file is part of the CxProlog system

 *   Space.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Space_
#define _Space_

#define NoSpace(n)			Error("%s overflow", n)

#define GrowArea(c,e,w,n)	{	if( ((c) += (w)) > (e) )	\
								{	(c) -= (w) ; NoSpace(n) ; } }

#define GrowAreaR(c,b,w,n) {	if( ((c) -= (w)) < (b) )	\
								{	(c) += (w) ; NoSpace(n) ; }	}

extern CharPt strBuffer, strBufferEnd ;
extern Hdl codeBuffer, codeBufferEnd ;

void InitSpace(void) ;
VoidPt SpaceAlloc(int nWords, Bool permanent) ;
void SpaceFree(VoidPt ptr) ;
long SpaceUsed(void) ;
long StacksSize(void) ;
void ListSpace(void) ;

#endif
