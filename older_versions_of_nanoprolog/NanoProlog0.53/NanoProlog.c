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

 940225: "	SetSourceModule(LookupModule(userAtom)) ;" removed. Now the user mode is set
  		with the new predicate "$user_mode".
 931117: release of version 0.5

*/

#include "NanoProlog.h"

static Bool booted ;

static void Boot()
{
	Pt t ;
	int n = 0 ;
	
	See(bootFileName) ;
	while( (t = ReadTerm(nil)) != TagAtom(eofAtom) )
	{
		n++ ;
		if( t == nil )
		{
			Mesg("Error in clause number %d at file %s:\n\t", n, bootFileName) ;
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

main()
{
	ThreadPt th ;

	Mesg("NanoProlog version 0.53") ;
	Mesg("Prolog 'til you drop") ;
	booted = false ;
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
			Boot() ;
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
