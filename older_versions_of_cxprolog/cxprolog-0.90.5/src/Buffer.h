/*
 *   This file is part of the CxProlog system

 *   Buffer.h
 *   by A.Miguel Dias - 2003/09/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Buffer_
#define _Buffer_

/* BUFFER */
typedef struct Buffer {
	ExtraDef(Buffer) ;
	CharPt begin, end ;
	CharPt last, position ;
} Buffer, *BufferPt ;

#define cBufferPt(x)			((BufferPt)(x))

#define BufferBegin(b)			((b)->begin)
#define BufferEnd(b)			((b)->end)
#define BufferLast(b)			((b)->last)
#define BufferPosition(b)		((b)->position)

/* MAIN OPERATIONS */
Size BufferSize(BufferPt b) ;
CharPt BufferContents(BufferPt b) ;
BufferPt BufferNew(void) ;
void BufferClear(BufferPt b) ;
void BufferDelete(BufferPt b) ;
int BufferGet(BufferPt b, PInt idx) ;
void BufferSet(BufferPt b, PInt idx, int i) ;
void BufferResize(BufferPt b, Size newSize) ;

/* SEQUENTIAL OPERATIONS - only used in Stream.c */
void BufferEnsureSpace(BufferPt b, Size freeSpace) ;
void BufferSetSizeUnsafe(BufferPt b, Size size) ;
void BufferReset(BufferPt b) ;
void BufferRewrite(BufferPt b) ;
void BufferAppend(BufferPt b) ;
/* sequential read operations */
int BufferAccessByte(BufferPt b, Bool advance) ;
Bool BufferAtEnd(BufferPt b) ;
Size BufferReadN(BufferPt b, VoidPt v, Size n) ;
int BufferAccessChar(BufferPt b, Bool advance) ;
/* sequential write operations */
void BufferPutByte(BufferPt b, int c) ;
Size BufferWriteN(BufferPt b, VoidPt v, Size n) ;
void BufferPutChar(BufferPt b, int c) ;

/* TEST, EXTRACT & INIT */
Bool IsBuffer(Pt t) ;
BufferPt XTestBuffer(Pt t) ;
void BuffersInit(void) ;

#endif
