/*
 *   This file is part of the CxProlog system

 *   Memory.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Memory_
#define _Memory_

extern Bool memoryManagerActive ;

#define MemoryManagerActive()	 ( memoryManagerActive )

VoidPt PermBlockAllocate(Size nWords) ;
VoidPt TempBlockAllocate(Size nWords) ;
void BlockRelease(VoidPt ptr) ;
Size BlockSize(VoidPt ptr) ;
Size StaticMemoryUsed(void) ;
Size StaticMemory(void) ;
void MemoryInit(void) ;

#endif
