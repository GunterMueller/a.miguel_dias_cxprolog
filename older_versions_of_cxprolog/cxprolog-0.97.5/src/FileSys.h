/*
 *   This file is part of the CxProlog system

 *   FileSys.h
 *   by A.Miguel Dias - 2002/01/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2010 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _FileSys_
#define _FileSys_

Pt PathNameListFromAtom(AtomPt s) ;
AtomPt PathNameListToAtom(Pt list) ;
Bool IsAbsoluteFileName(Str s) ;
AtomPt AbsoluteFileName(Str s) ;
CharPt FileNameInternalize(CharPt s) ;
CharPt FileNameExternalize(Str s, Bool sepNotAllowedAtEnd) ;
void EnsureNativeFileName(CharPt s) ;
void EnsureIndependentFileName(CharPt s) ;
CharPt GetFileNameLastComponent(CharPt s) ;
CharPt ProcessFileName(CharPt s) ;
void GoHome(Bool handleError) ;
CharPt CurrDirPath(void) ;
void DeleteFilesWithExtension(Str path, Str ext) ;
CharPt WithAppDirPath(Str path, Str name, Str extension) ;
CharPt WithHomeDirPath(Str path, Str name, Str extension) ;
CharPt WithPrefixDirPath(Str path, Str name, Str extension) ;
CharPt WithCurrentDirPath(Str name, Str extension) ;
CharPt WithLibDirPath(Str name, Str extension) ;
CharPt WithPreferencesDirPath(Str name, Str extension) ;
CharPt WithCacheDirPath(Str name, Str extension) ;
CharPt WithTmpDirPath(Str name, Str extension) ;
void FSDelete(CharPt fname) ;
void FileSysInit(void) ;

#endif
