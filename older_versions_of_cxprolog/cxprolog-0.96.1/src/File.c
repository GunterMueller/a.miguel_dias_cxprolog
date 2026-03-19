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
#endif

typedef struct File {
	ExtraDef(File) ;
	FILE *file ;
	CharPt name ;
	int mode ;
	Bool tty ;
} File, *_FilePt ;

#define cFilePt(x)			((_FilePt)(x))

#define FileFILE(f)			(cFilePt(f)->file)
#define FileFD(f)			fileno(FileFILE(f))
#define FileName(f)			(cFilePt(f)->name)
#define FileMode(f)			(cFilePt(f)->mode)
#define FileTty(f)			(cFilePt(f)->tty)


/* Parameterization */

#if UNDERSTAND_EXTERNAL_ENCODINGS
#define MyEOF						WEOF
#define MyGetChar(f)				fgetwc(f)
#define MyUngetChar(c,f)			ungetwc(c,f)
#define MyFileClearOrientation(f,m)	FileClearOrientation(f, m)
#else
#define MyEOF						EOF
#define MyGetChar(f)				fgetc(f)
#define MyUngetChar(c,f)			ungetc(c,f)
#define MyFileClearOrientation(f,m)	/* nothing */
#endif

static ExtraTypePt fileType ; /* @@@ Slave of streamType, will delete. */

/* PRIVATE FUNCTIONS */

#if UNDERSTAND_EXTERNAL_ENCODINGS
static void FileClearOrientation(FilePt f, CharPt mode)
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

/* Must try to use fwide!!! */
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


/* MAIN OPERATIONS */

FilePt FileNew(FILE *file, CharPt name, int mode)
{
	FilePt f ;
	Str32 m ;
	strcpy(m, mode==mWrite ? "w" : mode==mRead ? "r" : "a") ;
	if( file == nil && (file = fopen(StrExternalize(name), m)) == nil )
		FileError("Cannot open file '%s'", name) ;
	f = ExtraNew(fileType, 0, nil) ;
	FileFILE(f) = file ;
	FileName(f) = name ;
	FileMode(f) = mode ;
	FileTty(f) = OSIsATty(FileFD(f)) ;
	return f ;
}

void FileDelete(FilePt f, Bool force)
{
	FileDisable(f, force) ;
}

FILE *FileAsFILE(FilePt f)
{
	return FileFILE(f) ;
}

int FileAsFD(FilePt f)
{
	return FileFD(f) ;
}

Bool FileIsATty(FilePt f)
{
	return FileTty(f) ;
}


/* SEQUENTIAL READ OPERATIONS */

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
	if( isText ) MyFileClearOrientation(f, "rb") ;
	r = fread(v, 1, n, FileFILE(f)) ;
	if( isText ) MyFileClearOrientation(f, "r") ;
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

WChar FileGetCharInteractive(FilePt f) /* pre: f interactive */
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

WChar FilePeekCharInteractive(FilePt f) /* pre: f interactive */
{
	WChar c = FileGetCharInteractive(f) ;
	if( c != EOF ) MyUngetChar(c, FileFILE(f)) ;
	return c ;
}

CharPt FileGetCharStrInteractive(FilePt f, CharPt line, Size size)
{			/* pre: size > MB_LEN_MAX ; pre: f interactive */
	CharPt workLine = line, endLine = line + size ;
	WChar c ;
	do {
		if( (c = FileGetCharInteractive(f)) == EOF ) {
			if( workLine == line )
				return nil ;	/* EOF */
			else
				break ;
		}
		CharEncode(workLine, c) ;
	} while( c != '\n' && endLine - workLine > MB_LEN_MAX ) ;
	CharEncode(workLine, '\0') ;
	return line ;
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
	if( isText ) MyFileClearOrientation(f, "ab") ;
	r = fwrite(v, 1, n, FileFILE(f)) ;
	if( isText ) MyFileClearOrientation(f, "a") ;
	return r ;
}

#if UNDERSTAND_EXTERNAL_ENCODINGS

void FilePutChar(FilePt f, WChar c)
{
	if( FileTty(f) ) {
		/* This method of writting wide chars to a tty stream is 2.5 times
		   slower than the method of the "else" path below, be it but achives
		   compatibility with the readline library (which seems to be
		   incompatible with output oriented-streams.) Fortunatelly, this
		   slower method is only used for real interactive output files,
		   because if, for example, the output of the application is
		   redirected then the stream will be no longer a tty stream and
		   the fast alternative will be used. So this technique of achieving
		   compatibility with the readline library is perfect and need no
		   further improvement. */
		char buff[MB_LEN_MAX] ;
		int len = wcrtomb(buff, c, nil) ;
		if( len == -1 )
			FileError("Could not convert to multibyte sequence") ;
		if( fwrite(buff, 1, len, FileFILE(f)) != len )
			FileError("Could not write on file '%s'", FileName(f)) ;
		/*FilePutByte(f, ' ') ;*/
	}
	else {
		/* This is the faster way to output wide chars. */
		if( fputwc(c, FileFILE(f)) == EOF ) {
		#ifdef EILSEQ
			if( errno == EILSEQ )
				FileError("Wide character conversion error on output file  '%s'",
													FileName(f)) ;
		#endif	
			FileError("Could not write on file '%s'", FileName(f)) ;
		}
	}
}

void FilePutCharStr(FilePt f, CharPt s)
{
	while( *s )
		FilePutChar(f, CharDecode(s)) ;
}
	
#else /* !UNDERSTAND_EXTERNAL_ENCODING */

void FilePutChar(FilePt f, WChar c)
{
	if( fputc(c, FileFILE(f)) == EOF )
		FileError("Could not write on file '%s'", FileName(f)) ;
}

void FilePutCharStr(FilePt f, CharPt s)
{
	if( fputs(s, FileFILE(f)) == EOF )
		FileError("Could not write on file '%s'", FileName(f)) ;
}

#endif


/* TEST, EXTRACT & INIT */


void FilesInit()
{
	fileType = ExtraTypeNew("FILE", FileSizeFun, nil, nil /* @@@ */, 1) ;
}
