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

#define HandleBufferOverflowBytes(b, w)	\
	{ if( b >= bufferEnd ) GrowBufferAndCopyBytes(&b, w) ; }

#define HandleBufferOverflowWords(h, w)	\
	{ if( cCharPt(h) >= bufferEnd ) GrowBufferAndCopyWords(&h, w) ; }

#define UseBuffer()		((bufferInUse ? IError("Buffer in use") : 0),	\
									(bufferInUse = true), buffer)
#define FreeBuffer()	(bufferInUse = false, buffer)

#define retBufferSize	1024
extern char retBuffer[] ;
extern CharPt buffer, bufferEnd ;
extern Bool bufferInUse ;

VoidPt PrimitiveAllocate(long nWords) ;
void GrowBufferAndCopyBytes(CharPt *b, CharPt why) ;
void GrowBufferAndCopyWords(Hdl *h, CharPt why) ;
long BufferSize(void) ;

void InitMemory(void) ;
VoidPt PermanentAllocate(long nWords) ;
VoidPt TemporaryAllocate(long nWords) ;
void Release(VoidPt ptr) ;
long MemoryUsed(void) ;
long TotalMemory(void) ;

#endif
