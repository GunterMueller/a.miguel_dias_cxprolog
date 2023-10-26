/*
 *   This file is part of the CxProlog system

 *   OS.c
 *   by A.Miguel Dias - 2001/07/03
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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




/* INTERRUPT */

/* OS_DEPENDENT void InterruptOff(void) ; */
/* OS_DEPENDENT void InterruptOn(void) ; */




/* FILESYS */

/* OS_DEPENDENT Bool FSExists(CharPt fname) ; */
/* OS_DEPENDENT Bool FSRen(CharPt oname, CharPt nname) ; */
/* OS_DEPENDENT Bool FSDel(CharPt fname) ; */
/* OS_DEPENDENT Pt FSPropType(CharPt fname) ; */
/* OS_DEPENDENT Pt FSPropReadable(CharPt fname) ; */

/* OS_DEPENDENT Pt FSGetCurrDir(void) ; */
/* OS_DEPENDENT Bool FSSetCurrDir(Pt t) ; */
/* OS_DEPENDENT void FSGoHome(void) ; */
/* OS_DEPENDENT Pt FSFiles(void) ; */

/* CXPROLOG C'BUILTINS */

static void PFSExists()
{
	if( FSExists(XTestAtomName(X0)) ) JumpNext() 
	DoFail()
}

static void PFSRename(void)
{
	AtomPt ai = XTestAtom(X0) ;
	AtomPt ao = XTestAtom(X1) ;
	if( ao == XAtom(tNilAtom) ) {
		if( not FSDel(AtomName(ai)) )
			FileError("Couldn't delete file '%s'", AtomName(ai)) ;
		JumpNext()
	}
	if( not FSRen(AtomName(ai), AtomName(ao)) )
		FileError("Couldn't rename file '%s' as '%s'",
						AtomName(ai), AtomName(ao)) ;
	JumpNext()
}

static void PFSDelete(void)
{
	if( not FSDel(XTestAtomName(X0)) )
		FileError("Couldn't delete file '%s'", XTestAtomName(X0)) ;
	JumpNext()
}

static void PFSProperty(void)
{
	CharPt fname = XTestAtomName(X0) ;
	CharPt fprop = XTestAtomName(X1) ;
	Pt t ;
	if( strcmp(fprop, "type") == 0 )
		t = FSPropType(fname) ;
	elif( strcmp(fprop, "readable") == 0 )
		t = FSPropReadable(fname) ;
	else
		FileError("Unknown property '%s'", fprop) ;
	if( t == nil )
		FileError("Couldn't get property '%s' of file '%s'", fprop, fname) ;
	if( Unify(X2, t) ) JumpNext() 
	DoFail()
}

static void PFSCd1(void)
{
	Pt t ;
	if( (t = FSGetCurrDir()) == nil )
		FileError("Couldn't get current directory") ;
	if( Unify(X0, t) ) JumpNext() 
	DoFail()
}

static void PFSCd2(void)
{
	Pt old, new ;
	if( (old = FSGetCurrDir()) == nil )
		FileError("Couldn't get current directory") ;
	if( not Unify(X0, old) ) DoFail()

	if( (new = Drf(X1)) == old ) JumpNext()
	if( not FSSetCurrDir(new) )
		FileError("Invalid directory path specification '%s'", TermAsStr(new)) ;
	JumpNext()
}

static void PFSHome(void)
{
	FSGoHome() ;
	JumpNext()
}

static void PFSFiles(void)
{
	Pt t ;
	if( (t = FSFiles()) == nil )
		FileError("Couldn't obtain current directory contents") ;
	if( Unify(X0, t) ) JumpNext() 
	DoFail()
}




/* PROCESSES */

static int argc ;
static CharPt *argv ;
static CharPt bootFileName = nil ;

/* OS_DEPENDENT Bool OSRun(CharPt fname) ; */

CharPt OSGetEnv(CharPt var)
{
	return getenv(var) ;
}

void SpecifyBootFile(CharPt boot)
{
	bootFileName = boot ;
}

void OSArgsInit(int ac, CharPt av[])
{
	argc = ac ;
	argv = av ; 
}

CharPt OSGetArg(CharPt sw)
{
	register int i ;
	if( sw[0] == '\0' || sw[0] != '-' )
		Error("First argument should be an atom starting with a '-'") ;
	if( bootFileName != nil && EqualStr("-boot", sw) )
		return bootFileName ;
	for( i = 1 ; i < argc ; i++ )
		if( EqualStr(argv[i], sw) )
			if( i+1 < argc && argv[i+1][0] != '-' )
				return argv[i+1] ;
			else return "" ;
	return nil ;
}

/* CXPROLOG C'BUILTINS */

static void POSName()
{
	if( Unify(X0, MakeAtom(OSName())) ) JumpNext() 
	DoFail()
}

static void POSRun()
{
	if( OSRun(XTestAtomName(X0)) ) JumpNext()
	DoFail()
}

static void PSh()
{
	if( OSRun("sh") ) JumpNext()
	DoFail()
}

static void POSGetEnv()
{
	CharPt s ;
	if( (s = OSGetEnv(XTestAtomName(X0))) != nil
						&& Unify(X1, MakeTempAtom(s)) )
		JumpNext()
	DoFail()
}

static void POSGetArg()
{
	CharPt s ;
	if( (s = OSGetArg(XTestAtomName(X0))) != nil
						&& Unify(X1, MakeTempAtom(s)) )
		JumpNext()
	DoFail()
}

static void POSGetArgs()
{
	register int i ;
	register Pt list ;
	ZEnsureFreeSpaceOnStacks(2 * argc) ;
	list = tNilAtom ;
	if( bootFileName != nil ) {
		list = MakeBinStruct(listFunctor, MakeTempAtom(bootFileName), list) ;
		list = MakeBinStruct(listFunctor, MakeTempAtom("-boot"), list) ;
	}
	for( i = argc - 1 ; i >= 0 ; i-- )
		list = MakeBinStruct(listFunctor, MakeTempAtom(argv[i]), list) ;
	if( Unify(X0, list) )
		JumpNext()
	DoFail()
}




/* INIT */

void OSInit()
{
	OSDependentInit() ;

/* INTERRUPT */
	/* Nothing */

/* FILESYS */
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

/* PROCESSES */
	InstallCBuiltinPred("os_name", 1, POSName) ;
	InstallCBuiltinPred("os_run", 1, POSRun) ;
	InstallCBuiltinPred("system", 1, POSRun) ;			/* c-prolog */
	InstallCBuiltinPred("sh", 0, PSh) ;					/* c-prolog */
	InstallCBuiltinPred("os_env", 2, POSGetEnv) ;
	InstallCBuiltinPred("os_arg", 2, POSGetArg) ;
	InstallCBuiltinPred("os_args", 1, POSGetArgs) ;
}
