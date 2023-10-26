/*
 *   This file is part of the CxProlog system

 *   Buffer.h
 *   by A.Miguel Dias - 2003/09/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Buffer_
#define _Buffer_

/* BUFFER */

typedef struct Buffer {
	ExtraDef(Buffer) ;
	CharPt begin, end ;
	CharPt last, position ;
} Buffer, *BufferPt ;

Size BufferSize(BufferPt b) ;
CharPt BufferContents(BufferPt b) ;
void BufferResize(BufferPt b, Size newSize) ;

void BufferEnsureSpace(BufferPt b, Size freeSpace) ;
void BufferSetSizeUnsafe(BufferPt b, Size size) ;
void BufferReset(BufferPt b) ;
void BufferRewrite(BufferPt b) ;
void BufferAppend(BufferPt b) ;
Bool BufferAtEnd(BufferPt b) ;
int BufferGetChar(BufferPt b) ;
int BufferPeekChar(BufferPt b) ;
void BufferPutChar(BufferPt b, int c) ;
void BufferPutStr(BufferPt b, CharPt s) ;
void BufferWriteV(BufferPt b, CharPt fmt, VoidPt v) ;
void BufferWrite(BufferPt b, CharPt fmt, ...) ;

BufferPt XTestBuffer(Pt t) ;

void BuffersInit(void) ;

#endif
