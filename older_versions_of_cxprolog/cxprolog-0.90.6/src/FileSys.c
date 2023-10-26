/*
 *   This file is part of the CxProlog system

 *   FileSys.c
 *   by A.Miguel Dias - 2002/01/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

/* CXPROLOG C'BUILTINS */

static void PFSExists()
{
	CharPt fname = XTestAtomName(X0) ;
	MustBe( OSExists(fname) )
}

static void PFSRename(void)
{
	CharPt fname0 = XTestAtomName(X0) ;
	CharPt fname1 = XTestAtomName(X1) ;
	if( TestAtom(X1) == tNilAtom ) {
		if( !OSDel(fname0) )
			FileError("Couldn't delete file '%s'", fname0) ;
	}
	elif( !OSRen(fname0, fname1) )
		FileError("Couldn't rename file '%s' as '%s'", fname0, fname1) ;
	JumpNext()
}

static void PFSDelete(void)
{
	CharPt fname = XTestAtomName(X0) ;
	if( !OSDel(fname) )
		FileError("Couldn't delete file '%s'", fname) ;
	JumpNext()
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
	MustBe( Unify(X2, t) )
}

static void PFSCd1(void)
{
	Pt t ;
	if( (t = OSGetCurrDir()) == nil )
		FileError("Couldn't get current directory") ;
	MustBe( Unify(X0, t) )
}

static void PFSCd2(void)
{
	Pt old, new ;
	if( (old = OSGetCurrDir()) == nil )
		FileError("Couldn't get current directory") ;
	Ensure( Unify(X0, old) )
	if( (new = Drf(X1)) != old && !OSSetCurrDir(new) )
		FileError("Invalid directory path specification '%s'", TermAsStr(new)) ;
	JumpNext()
}

static void PFSHome(void)
{
	OSGoHome() ;
	JumpNext()
}

static void PFSFiles(void)
{
	Pt t ;
	if( (t = OSFiles()) == nil )
		FileError("Couldn't obtain current directory contents") ;
	MustBe( Unify(X0, t) )
}

static void PFSAtomPath(void)
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = OSPathNameFromStr(XAtomName(t0)) ;
		MustBe( t != nil && Unify(X1, t) )	
	}
	elif( IsVar(t0) ) {
		MustBe( UnifyWithAtomic(t0, MakeTempAtom(OSPathNameToStr(X1))) )
	}
	else TypeError2("ATOM", t0) ;

}

void FileSysInit()
{
	OSFileSysInit() ;
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
	InstallCBuiltinPred("$fs_atom_path", 2, PFSAtomPath) ;
}
