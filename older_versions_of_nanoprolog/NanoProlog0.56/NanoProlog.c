/*
 *   This file is part of the NanoProlog system

 *   NanoProlog.c
 *   by A.Miguel Dias - 89/11/14
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 970805: Now the name of the boot file may be obtained from an application
             argument or from the sysvar "NANO"
 940225: "	SetSourceModule(LookupModule(userAtom)) ;" removed. Now the user mode is set
  		with the new predicate "$user_mode".
 931117: release of version 0.5

*/

#include "NanoProlog.h"

static void Boot(CharPt bootFile)
{
	Pt t ;
	int n = 0 ;
	
	See(bootFile) ;
	while( (t = ReadTerm(nil)) != TagAtom(eofAtom) )
	{
		n++ ;
		if( t == nil )
		{
			Mesg("Error in clause number %d at file %s:\n\t", n, bootFile) ;
			WritelnTerm(t) ;
		}
		else
		{
#if 0
			WritelnTerm(t) ;
#endif
			CompileClause(t, true) ;
		}
	}
	Seen() ;
}	

static CharPt GetNanoEnvVar(int argc, CharPt argv[], CharPt envp[])
{
	int i ;
	CharPt *pt ;

	if( argc == 2 )
		return( argv[1] ) ;

	for( pt = envp ; *pt != nil ; pt++ )
		if( strncmp("NANO=", *pt, 5) == 0 )
			return( (*pt) + 5 ) ;

	return( bootFileName ) ;
}

main(int argc, CharPt argv[], CharPt envp[])
{
	ThreadPt th ;
	CharPt bootFile = GetNanoEnvVar(argc, argv, envp) ;
	static Bool booted = false ;

	Mesg("NanoProlog version 0.56") ;
	Mesg("Prolog 'til you drop") ;
	switch( StartEventHandler() )
	{
		case 0:
		{
			InitTime() ;
			InitStreams() ;
			CheckHost() ;
			InitInsts() ;
			InitIndex() ;
			CreateHeap(heapAreaSize Kb) ;
			CreateInitialAtoms() ;
			SetupInitialAtomPriorities() ;
			CreateInitialFunctors() ;
			InitModules() ;
			InstallCPredicates() ;
			InstallMoreCPredicates() ;
			StopInstallCPredicates() ;
			th = CreateThread(userAtom, stacksAreaSize Kb,
								userAtom, LookupAtom("$lux0"), LookupAtom("$lux1")) ;
			if( th == nil ) Error("Not enough memory") ;
			Boot(bootFile) ;
			PatchPredicates() ;
			
			booted = true ;
			TransferToThread(th) ;
			break ;
		}
		case 1:
		{
			if( not booted )
				FatalError("In boot") ;
			RecoverStreams() ;
			RestartCurrThread() ;
			break ;
		}
		case 2:
		{
			RecoverStreams() ;
			FinishCurrThread("completed") ;
			break ;
		}
		case 3:
		{
			RecoverStreams() ;
			FinishCurrThread("failed") ;
			break ;
		}
		case 4:
		{
			RecoverStreams() ;
			printf("Prolog halted.\n") ;
			break ;
		}
		default: InternalError("main") ;
	}
}
