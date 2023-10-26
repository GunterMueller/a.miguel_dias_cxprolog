/*
 *   This file is part of the CxProlog system

 *   File.h
 *   by A.Miguel Dias - 2007/01/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _File_
#define _File_

typedef VoidPt FilePt ;

/* MAIN OPERATIONS */
FilePt FileNew(FILE *file, CharPt name, int mode) ;
void FileDelete(FilePt f, Bool force) ;
FILE *FileAsFILE(FilePt f) ;
Bool FileIsATty(FilePt f) ;

/* SEQUENTIAL OPERATIONS - only used in Stream.c */
/* sequential read operations */
WChar FileGetByte(FilePt f) ;
WChar FilePeekByte(FilePt f) ;
Size FileGetNBytes(FilePt f, VoidPt v, Size n, Bool isText) ;
WChar FileGetChar(FilePt f) ;
WChar FilePeekChar(FilePt f) ;
WChar FileGetCharInteractive(FilePt f) ;
WChar FilePeekCharInteractive(FilePt f) ;
CharPt FileGetCharStrInteractive(FilePt f, CharPt line, Size size) ;
/* sequential write operations */
void FileFlush(FilePt f) ;
void FilePutByte(FilePt f, WChar c) ;
Size FilePutNBytes(FilePt f, VoidPt v, Size n, Bool isText) ;
void FilePutChar(FilePt f, WChar c) ;
void FilePutCharStr(FilePt f, CharPt s) ;

/* TEST, EXTRACT & INIT */
void FilesInit(void) ;

#endif
