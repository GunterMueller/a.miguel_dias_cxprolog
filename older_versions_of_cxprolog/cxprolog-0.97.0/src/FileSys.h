/*
 *   This file is part of the CxProlog system

 *   FileSys.h
 *   by A.Miguel Dias - 2002/01/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _FileSys_
#define _FileSys_

Pt PathNameListFromAtom(AtomPt s) ;
AtomPt PathNameListToAtom(Pt list) ;
Bool IsAbsoluteFileName(CharPt s) ;
AtomPt AbsoluteFileName(CharPt s) ;
CharPt FileNameInternalize(CharPt s) ;
void EnsureNativeFileName(CharPt s) ;
void EnsureIndependentFileName(CharPt s) ;
CharPt GetFileNameLastComponent(CharPt s) ;
CharPt ProcessFileName(CharPt s) ;
void GoHome(Bool handleError) ;
CharPt CurrDirPath(void) ;
void DeleteFilesWithExtension(CharPt path, CharPt ext) ;
CharPt WithAppDirPath(CharPt path, CharPt name, CharPt extension) ;
CharPt WithPrefixDirPath(CharPt path, CharPt name, CharPt extension) ;
CharPt WithCurrentDirPath(CharPt name, CharPt extension) ;
CharPt WithLibDirPath(CharPt name, CharPt extension) ;
CharPt WithCacheDirPath(CharPt name, CharPt extension) ;
CharPt WithTmpDirPath(CharPt name, CharPt extension) ;
void FileSysInit(void) ;

#endif
