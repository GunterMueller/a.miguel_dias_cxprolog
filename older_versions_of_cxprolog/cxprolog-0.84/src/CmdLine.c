/*
 *   This file is part of the CxProlog system

 *   CmdLine.c
 *   by A.Miguel Dias - 2002/01/19
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

static int argc ;
static CharPt *argv ;
static CharPt bootFileName = nil ;

void SpecifyBootFile(CharPt boot)
{
	bootFileName = boot ;
}

void CmdLineInit(int ac, CharPt av[])
{
	argc = ac ;
	argv = av ; 
}

CharPt CmdLineArg(CharPt sw)
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

static void POSGetArg()
{
	CharPt s ;
	if( (s = CmdLineArg(XTestAtomName(X0))) != nil
						&& Unify(X1, MakeTempAtom(s)) )
		JumpNext()
	DoFail()
}

static void POSGetArgs()
{
	register int i ;
	register Pt list ;
	ZEnsureFreeSpaceOnStacks(2 * argc + 2, "os_args/1") ; /* stacks may grow */
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

void CmdLineInit2()
{
	InstallCBuiltinPred("os_arg", 2, POSGetArg) ;
	InstallCBuiltinPred("os_args", 1, POSGetArgs) ;
}