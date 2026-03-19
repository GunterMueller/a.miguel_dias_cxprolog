/*
 *   This file is part of the CxProlog system

 *   File.c
 *   by A.Miguel Dias - 2007/01/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

#if UNDERSTAND_EXTERNAL_ENCODINGS
#include <wchar.h>

#define MyEOF						WEOF
#define MyGetChar(f)				fgetwc(f)
#define MyUngetChar(c,f)			ungetwc(c,f)
#define MyPutChar(c,f)				fputwc(c,f)
#define MyFileSetNoOrientation(f,m)	FileSetNoOrientation(f, m)
#else
#define MyEOF						EOF
#define MyGetChar(f)				fgetc(f)
#define MyUngetChar(c,f)			ungetc(c,f)
#define MyPutChar(c,f)				fputc(c,f)
#define MyFileSetNoOrientation(f,m)	/* nothing */
#endif

static ExtraTypePt fileType ;


/* PRIVATE FUNCTIONS */

#if UNDERSTAND_EXTERNAL_ENCODINGS
static void FileSetNoOrientation(FilePt f, CharPt mode)
{
	if( *mode == 'r' ) {
		long pos ;
		if( (pos = ftell(FileFILE(f))) == -1 )
			FileError("Cannot ftell on file '%s'", FileName(f)) ;
		if( freopen(nil, mode, FileFILE(f)) == nil )
			FileError("Cannot unset orientation of file '%s'", FileName(f)) ;
		if( fseek(FileFILE(f), pos, SEEK_SET) != 0 )
			FileError("Cannot fseek on file '%s'", FileName(f)) ;
	}
	else
		if( freopen(nil, mode, FileFILE(f)) == nil )
			FileError("Cannot unset orientation of file '%s'", FileName(f)) ;
}
#endif

static void FileDisable(FilePt f, Bool force)
{
	if( ExtraIsAlive(f) ) {
		if( !force
			&& (FileMode(f) == mWrite || FileMode(f) == mAppend)
			&& fflush(FileFILE(f)) != 0 )
				FileError("Could not close file '%s'", FileName(f)) ;
		fclose(FileFILE(f)) ;
		ExtraSetDisabled(f) ;
	}
}

static Size FileSizeFun(VoidPt x)
{
	return WordsOf(File) ;
}

static Bool FileBasicGCDelete(VoidPt x)
{
	FilePt f = cFilePt(x) ;
	FileDisable(f, true) ;
	return true ;
}


/* MAIN OPERATIONS */

FilePt FileNew(FILE *file, CharPt name, int mode)
{
	FilePt f ;
	Str32 m ;
	strcpy(m, mode==mWrite ? "w" : mode==mRead ? "r" : "a") ;
	if( file == nil && (file = fopen(StrExternalize(name), m)) == nil )
		FileError("Cannot open file '%s'", name) ;
	f = ExtraNew(fileType, 0) ;
	FileFILE(f) = file ;
	FileName(f) = name ;
	FileMode(f) = mode ;
	return f ;
}

void FileDelete(FilePt f, Bool force)
{
	FileDisable(f, force) ;
}



/* SEQUENTIAL READ OPERATIONS */

Bool FileAtEnd(FilePt f)
{
	return feof(FileFILE(f)) ;
}

WChar FileGetByte(FilePt f)
{
	return fgetc(FileFILE(f)) ;
}

WChar FilePeekByte(FilePt f)
{
	return ungetc(fgetc(FileFILE(f)), FileFILE(f)) ;
}

Size FileGetNBytes(FilePt f, VoidPt v, Size n, Bool isText)
{
	Size r ;
	if( isText ) MyFileSetNoOrientation(f, "rb") ;
	r = fread(v, 1, n, FileFILE(f)) ;
	if( isText ) MyFileSetNoOrientation(f, "r") ;
	return r ;
}

WChar FileGetChar(FilePt f)
{
	WChar c ;
	for(;;) {
		c = MyGetChar(FileFILE(f)) ;
		if( c == MyEOF ) {
	#ifdef EILSEQ
			if( errno == EILSEQ )
				FileError("Wide character conversion error on input file  '%s'",
												FileName(f)) ;
	#endif
			return EOF ;
		}
		if( c == 0 ) continue ;
		return CharFixCode(c) ;
	}
}

WChar FilePeekChar(FilePt f)
{
	WChar c = FileGetChar(f) ;
	if( c != EOF ) MyUngetChar(c, FileFILE(f)) ;
	return c ;
}

WChar FileGetCharInteractive(FilePt f)
{
	WChar c ;
	for(;;) { /* this works even with raw input or sockets */
		AllowInterruptibleSysCalls() ;
		c = MyGetChar(FileFILE(f)) ;
		DisallowInterruptibleSysCalls() ;
		if( c == MyEOF ) {
	#ifdef EILSEQ
			if( errno == EILSEQ )
				FileError("Wide character conversion error on input file  '%s'",
												FileName(f)) ;
	#endif
			if( InterruptHandle() ) continue ;
			return EOF ;
		}
		if( c < ' ' && c != '\n' ) {
			if( c == 0 ) continue ;
			elif( c == 10 || c == 13 ) {
				return '\n' ;
			}
			elif( c == 4 || c == 26 ) { /* CNTL-D, CNTL-Z */
				MyUngetChar(c, FileFILE(f)) ;
				return EOF ;
			}
		}
		return CharFixCode(c) ;
	}
}

WChar FilePeekCharInteractive(FilePt f)
{
	WChar c = FileGetCharInteractive(f) ;
	if( c != EOF ) MyUngetChar(c, FileFILE(f)) ;
	return c ;
}


/* SEQUENTIAL WRITE OPERATIONS */

void FileFlush(FilePt f)
{
	if( fflush(FileFILE(f)) != 0 )
		FileError("Could not flush file '%s'", FileName(f)) ;
}

void FilePutByte(FilePt f, WChar c)
{
	if( fputc(c, FileFILE(f)) == EOF )
		FileError("Could not write on file '%s'", FileName(f)) ;
}

Size FilePutNBytes(FilePt f, VoidPt v, Size n, Bool isText)
{
	Size r ;
	if( isText ) MyFileSetNoOrientation(f, "ab") ;
	r = fwrite(v, 1, n, FileFILE(f)) ;
	if( isText ) MyFileSetNoOrientation(f, "a") ;
	return r ;
}

void FilePutChar(FilePt f, WChar c)
{
	if( MyPutChar(c, FileFILE(f)) == EOF ) {
	#ifdef EILSEQ
		if( errno == EILSEQ )
			FileError("Wide character conversion error on output file  '%s'",
												FileName(f)) ;
	#endif	
		FileError("Could not write on file '%s'", FileName(f)) ;
	}
}

void FilePutCharStr(FilePt f, CharPt s)
{ /* Can be optimized */
	while( *s )
		FilePutChar(f, CharDecode(s)) ;
}


/* TEST, EXTRACT & INIT */


void FilesInit()
{
	fileType = ExtraTypeNew("FILE", FileSizeFun, nil, FileBasicGCDelete, 1) ;
}
