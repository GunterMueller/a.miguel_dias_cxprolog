/*
 *   This file is part of the CxProlog system

 *   FileSys.c
 *   by A.Miguel Dias - 2002/01/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

#if _WIN32 
#define SEP     '\\'
#else
#define SEP     '/'
#endif

static AtomPt homePath, currPath ;

Pt PathNameListFromAtom(AtomPt at) /* pre: at is a checked absolute path */
{
	register CharPt a, s = AtomName(at) ;
	Pt list = tNilAtom ;
	if( !IsAbsoluteFileName(at) )
		Error("Invalid pathname '%s'", s) ;
	if( *s == SEP ) s++ ;
	for( a = s ; *a != '\0' ; a++ )
		if( *a == SEP ) {
			*a = '\0' ;
			list = MakeList(MakeTempAtom(s), list) ;
			*a = SEP ;
			s = a + 1 ;
		}
	if( a != s )		
		list = MakeList(MakeTempAtom(s), list) ;
	return list ;
}

AtomPt PathNameListToAtom(Pt list)
{
	Size len ;
	Hdl h = ListToArray(list, &len) ;
    if( len == 0 ) {
#if _WIN32 || __MSDOS__
		Error("Invalid empty pathname") ;
#else
		static Str4 rootStr = { SEP, '\0' } ;
        return LookupAtom(rootStr) ;
#endif
    }
	GStrOpen() ;
#if _WIN32 || __MSDOS__
    GStrAddStr(XTestAtomName(h[--len])) ;
	if( len == 0 ) GStrAddChar(SEP) ; /* Obtain 'c:\' */
#endif	
	while( len-- ) {
		GStrAddChar(SEP) ;
		GStrAddStr(XTestAtomName(h[len])) ;
	}
	return LookupTempAtom(GStrClose()) ;
}

static Pt NormalizeFileName(CharPt fname, Pt list)
{
	register CharPt a, s = fname ;
	if( *s == SEP ) s++ ;
	for( a = s ; ; a++ ) {
		if( *a == '\0' || *a == SEP ) {
			Bool sep = *a == SEP ;
			if( sep ) *a = '\0' ;
			if( a == s )											/* skip / */
				;
			elif( s[0] == '.' && s[1] == '\0' )						/* skip . */
				;
			elif( s[0] == '.' && s[1] == '.' && s[2] == '\0' ) {	/* up .. */
				if( list == tNilAtom )
					Error("Invalid pathname '%s'", fname) ;
				else
					list = XListTail(list) ;
			}
			else
				list = MakeList(MakeTempAtom(s), list) ;
			if( sep ) *a = SEP ;
			else break ;
			s = a + 1 ;
		}
	}
	return list ;
}

Bool IsAbsoluteFileName(AtomPt at)
{
	CharPt s = AtomName(at) ;
#if _WIN32 || __MSDOS__
	return s[0] != '\0' && s[1] == ':' && s[2] == SEP ;
#else
	return s[0] == SEP ;
#endif
}

AtomPt AbsoluteFileName(AtomPt at)
{
	CharPt s = AtomName(at) ;
	Pt list = tNilAtom ;
	HSave() ;
	if( *s == SEP )
		list = NormalizeFileName(s, list) ;
	else {
		list = NormalizeFileName(AtomName(currPath), list) ;
		list = NormalizeFileName(s, list) ;
	}
	HRestore() ;
	return PathNameListToAtom(list) ;
}

static void SetCurrDir(AtomPt a)
{
	if( a != currPath ) {
		if( !OSSetCurrDir(AtomName(a)) )
			FileError("Could not change to the directory '%s'", AtomName(a)) ;
		currPath = a ;
	}
}

void GoHome()
{
	if( currPath != homePath ) {
		if( !OSSetCurrDir(AtomName(homePath)) )
			FileError("Could not change to the home directory") ;
		currPath = homePath ;
	}
}

static void FileSysBasicGCMark()
{
	ExtraGCMark(currPath) ;
}



/* CXPROLOG C'BUILTINS */

static void PFSExists()
{
	CharPt fname = XTestAtomName(X0) ;
	MustBe( OSExists(fname) ) ;
}

static void PFSRename(void)
{
	CharPt fname0 = XTestAtomName(X0) ;
	CharPt fname1 = XTestAtomName(X1) ;
	if( TestAtom(X1) == tNilAtom ) {
		if( remove(StrExternalize(fname0)) != 0 )
			FileError("Couldn't delete file '%s'", fname0) ;
	}
	elif( rename(StrExternalize(fname0), StrExternalize(fname1)) != 0 )
		FileError("Couldn't rename file '%s' as '%s'", fname0, fname1) ;
	JumpNext() ;
}

static void PFSDelete(void)
{
	CharPt fname = XTestAtomName(X0) ;
	if( remove(StrExternalize(fname)) != 0 )
		FileError("Couldn't delete file '%s'", fname) ;
	JumpNext() ;
}

static void PFSProperty(void)
{
	CharPt fname = XTestAtomName(X0) ;
	CharPt fprop = XTestAtomName(X1) ;
	Pt t = nil ;
	switch( fprop[0] ) {
		case 'r': if( StrEqual(fprop, "readable") )
					{ t = OSPropReadable(fname) ; break ; }
		case 's': if( StrEqual(fprop, "size") )
					{ t = OSPropSize(fname) ; break ; }
		case 't': if( StrEqual(fprop, "time") )
					{ t = OSPropTime(fname) ; break ; }
				  if( StrEqual(fprop, "type") ) 
					{ t = OSPropType(fname) ; break ; }
		default: FileError("Unknown property '%s'", fprop) ;
	}
	if( t == nil )
		FileError("Couldn't get property '%s' of file '%s'",
										fprop, fname) ;
	MustBe( Unify(X2, t) ) ;
}

static void PFSCd1(void)
{
	MustBe( Unify(X0, PathNameListFromAtom(currPath)) ) ;
}

static void PFSCd2(void)
{
	if( Drf(X0) == Drf(X1) )
		MustBe( Unify(X0, PathNameListFromAtom(currPath)) ) ;
	else {
		Ensure( Unify(X0, PathNameListFromAtom(currPath)) ) ;
		SetCurrDir(PathNameListToAtom(X1)) ;
	}
	JumpNext() ;
}

static void PFSHome(void)
{
	GoHome() ;
	JumpNext() ;
}

static void PFSFiles(void)
{
	Pt t ;
	if( (t = OSFiles()) == nil )
		FileError("Couldn't obtain current directory contents") ;
	MustBe( Unify(X0, t) ) ;
}

static void PAbsoluteFileName(void)
{
	MustBe( UnifyWithAtomic(X1, TagAtom(AbsoluteFileName(XTestAtom(X0)))) ) ;
}

static void PFSAtomPath(void)
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = PathNameListFromAtom(XAtom(t0)) ;
		MustBe( t != nil && Unify(X1, t) ) ;
	}
	elif( IsVar(t0) ) {
		MustBe( UnifyWithAtomic(t0, TagAtom(PathNameListToAtom(X1))) ) ;
	}
	else TypeError("ATOM", t0) ;

}

void FileSysInit()
{
	OSFileSysInit() ;

	ExtraGCHandlerInstall("FILESYS", FileSysBasicGCMark) ;
	currPath = homePath = LookupAtom(OSGetCurrDir()) ;

	InstallCBuiltinPred("fs_exists", 1, PFSExists) ;
	InstallCBuiltinPred("file_exists", 1, PFSExists) ;	/* quintus */
	InstallCBuiltinPred("exists", 1, PFSExists) ;		/* c-prolog */
	InstallCBuiltinPred("fs_rename", 2, PFSRename) ;
	InstallCBuiltinPred("rename", 2, PFSRename) ;		/* c-prolog */
	InstallCBuiltinPred("rename_file", 2, PFSRename) ;	/* quintus */
	InstallCBuiltinPred("fs_delete", 1, PFSDelete) ;
	InstallCBuiltinPred("delete_file", 1, PFSDelete) ;	/* quintus */
	InstallCBuiltinPred("fs_property", 3, PFSProperty) ;
	InstallCBuiltinPred("fs_cd", 1, PFSCd1) ;
	InstallCBuiltinPred("fs_cd", 2, PFSCd2) ;
	InstallCBuiltinPred("fs_home", 0, PFSHome) ;
	InstallCBuiltinPred("fs_files", 1, PFSFiles) ;
	InstallCBuiltinPred("absolute_file_name", 2, PAbsoluteFileName) ;
	InstallCBuiltinPred("$fs_atom_path", 2, PFSAtomPath) ;
}
