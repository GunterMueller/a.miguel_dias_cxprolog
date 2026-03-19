/*
 *   This file is part of the CxProlog system

 *   Memory.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Memory_
#define _Memory_

void InitMemory(void) ;
VoidPt PermanentAllocate(Size nWords) ;
VoidPt TemporaryAllocate(Size nWords) ;
void Release(VoidPt ptr) ;
Size StaticMemoryUsed(void) ;
Size StaticMemory(void) ;

#endif
