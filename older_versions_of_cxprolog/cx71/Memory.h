/*
 *   This file is part of the CxProlog system

 *   Memory.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Memory_
#define _Memory_

extern Hdl codeBuffer, codeBufferEnd ;
extern CharPt strBuffer, strBufferEnd ;

VoidPt AllocAligned(long bytes) ;
void InitMemory(void) ;
VoidPt PermanentAllocate(int nWords) ;
VoidPt TemporaryAllocate(int nWords) ;
void Release(VoidPt ptr) ;
long MemoryUsed(void) ;
long TotalMemory(void) ;

#endif
