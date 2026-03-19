/*
 *   This file is part of the CxProlog system

 *   Memory.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Memory_
#define _Memory_

/* PRIMITIVE ALLOCATE */
VoidPt PrimitiveAllocate(Size nWords) ;
VoidPt PrimitiveReallocate(VoidPt mem, Size copySize, Size newSize) ;
void PrimitiveRelease(VoidPt mem) ;
Bool UsingMMap(void) ;

/* CODE AREA ALLOCATE */
VoidPt PermAllocate(Size nWords) ;
VoidPt TempAllocate(Size nWords) ;
void Release(VoidPt ptr) ;
VoidPt Reallocate(VoidPt ptr, Size newSize) ;
Size TempBlockSize(VoidPt ptr) ;
Size CodeAreaSize(void) ;
Size CodeAreaUsed(void) ;

#endif
