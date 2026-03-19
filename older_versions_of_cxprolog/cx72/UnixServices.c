/*
 *   This file is part of the CxProlog system

 *   UnixServices.c
 *   by A.Miguel Dias - 2000/08/03
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"
/*
#include <unix.h>
#include <signal.h>
*/

Bool GetArgvSwitch(int argc, CharPt argv[], CharPt sw)
{
	int i ;
	for( i = 1 ; i < argc ; i++ )
		if( argv[i] != nil && EqualStr(argv[i], sw) ) {
			argv[i] = nil ;
			return true ;
		}
	return false ;
}

CharPt GetArgvArg(int argc, CharPt argv[], CharPt sw)
{
	int i ;
	CharPt s ;
	for( i = 1 ; i < argc - 1 ; i++ )
		if( argv[i] != nil && EqualStr(argv[i], sw) ) {
			s = argv[i+1] ;
			argv[i] = nil ;
			argv[i+1] = nil ;
			return s ;
		}
	return nil ;
}

void PostArgvCheck(int argc, CharPt argv[])
{
	int i ;
	for( i = 1 ; i < argc ; i++ )
		if( argv[i] != nil )
			FatalError("Unknown parameter in command line '%s'", argv[i]) ;
}


/* CXPROLOG C'BUILTINS */

static void PUnixGetEnv()
{
	CharPt s ;

	if( (s = getenv(XTestAtomName(X0))) != nil && Unify(X1, MakeAtomOrText(s)) )
		JumpNext()
	DoFail()
}

static void PUnixRun()
{
	Int res = system(XTestAtomName(X0)) ;
	if( Unify(X1, MakeInt(res)) ) JumpNext()
	DoFail()
}

void InitUnixServices()
{
	InstallCBuiltinPred("unix_get_env", 2, PUnixGetEnv) ;
	InstallCBuiltinPred("unix_run", 2, PUnixRun) ;
}


/*
Bool UnixRun(CharPt command)
{
	int status, child, result ;
	__sig_func oldsig ;
	CharPt shell ;
	int fork(void), wait(int *) ;

	if( (child = fork()) == 0 )
	{
		CloseAllStreams() ;	
		shell = getenv("SHELL") ;
		if( shell == nil ) shell = "/bin/sh" ;
		if( command == nil ) execl(shell, shell, nil) ;
		else execl(shell, shell, "-c", command, nil) ;
		exit(1) ;
	}
	else
	{
		oldsig = signal(SIGINT, SIG_IGN) ;
		status = -1 ;
		result =  child < 0 || wait(&status) != child || status != 0 ;
		signal(SIGINT, oldsig) ;
		return( result == 0 ) ;
	}
}
*/