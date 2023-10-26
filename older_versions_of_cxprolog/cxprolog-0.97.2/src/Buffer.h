/*
 *   This file is part of the CxProlog system

 *   Buffer.h
 *   by A.Miguel Dias - 2003/09/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Buffer_
#define _Buffer_

typedef struct Buffer *BufferPt ;

/* MAIN OPERATIONS */
Size BufferSize(BufferPt b) ;
UCharPt BufferContents(BufferPt b) ;
BufferPt BufferNew(void) ;
void BufferSetEncoding(BufferPt b, CharPt encoding) ;
CharPt BufferGetEncoding(BufferPt b) ;
void BufferDelete(BufferPt b) ;
void BufferClear(BufferPt b) ;
int BufferGet(BufferPt b, PInt idx) ;
void BufferSet(BufferPt b, PInt idx, int i) ;
void BufferResize(BufferPt b, Size newSize) ;

/* SEQUENTIAL OPERATIONS - only used in Stream.c */
void BufferEnsureCapacity(BufferPt b, Size freeSpace) ;
void BufferSetSizeUnsafe(BufferPt b, Size size) ;
void BufferReset(BufferPt b) ;
void BufferRewrite(BufferPt b) ;
void BufferAppend(BufferPt b) ;
/* sequential read operations */
Bool BufferAtEnd(BufferPt b) ;
WChar BufferGetByte(BufferPt b) ;
WChar BufferPeekByte(BufferPt b) ;
Size BufferGetNBytes(BufferPt b, VoidPt v, Size n) ;
WChar BufferGetChar(BufferPt b) ;
WChar BufferPeekChar(BufferPt b) ;
/* sequential write operations */
void BufferPutByte(BufferPt b, WChar c) ;
Size BufferPutNBytes(BufferPt b, VoidPt v, Size n) ;
void BufferPutChar(BufferPt b, WChar c) ;
void BufferPutCharStr(BufferPt b, CharPt s) ;

/* TEST, EXTRACT & INIT */
Bool IsBuffer(Pt t) ;
BufferPt XTestBuffer(Pt t) ;
void BuffersInit(void) ;

#endif
