/*
 *   This file is part of the NanoProlog system

 *   Module.c
 *   by A.Miguel Dias - 93/7/15
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

 931117: release of version 0.5

*/

/*
WARNING:
	BESIDES "system" AND "user", MODULES ARE NOT YET SUPPORTED BY
	THE ABSTRACT MACHINE AND THE COMPILER
*/

#include "NanoProlog.h"

#define moduleTableSize		16		/* Must be a power of 2 */

#define ModuleHash(pt)		( ( (cWord(pt)>>4) ) & ((moduleTableSize) - 1) )

static ModulePt moduleTable[moduleTableSize] = { nil } ;
ModulePt sourceModule, sysModule ;

static ModulePt NewModule(AtomPt name)
{
	ModulePt m = cModulePt(HeapAlloc(WordsOf(Module), true)) ;

	ModuleName(m) = name ;
	ModuleNext(m) = nil ;
	ModulePredicates(m) = nil ;
	return( m ) ;
}

ModulePt LookupModule(AtomPt name)
{
	ModulePt m ;
	register ModulePt *v ;

	for( v = moduleTable + ModuleHash(name) ; *v != nil ; v = &ModuleNext(*v) )
			if( ModuleName(*v) == name ) return( *v ) ;
	m = NewModule(name) ;
	v = moduleTable + ModuleHash(name) ;
	ModuleNext(m) = *v ;
	*v = m ;
	return( m ) ;
}

void InitModules()
{
	sysModule = NewModule(LookupAtom("%%")) ;
	SetSourceModule(sysModule) ;
}

void ListModules()
{
	register int i ;
	ModulePt m ;

	printf("  Modules:\n") ;
	dotimes(i, moduleTableSize)
		for( m = moduleTable[i] ; m != nil ; m = ModuleNext(m) )
			printf("     %s\n", ModuleText(m) ) ;
}

void ListModulePreds(ModulePt m)
{
	register PredicatePt pr ;

	foreach(pr, ModulePredicates(m), PredNextM(pr))
		printf("%s\n", PredNameArity(pr) ) ;
}

void ListModuleUndefPreds(ModulePt m)
{
	register PredicatePt pr ;

	foreach(pr, ModulePredicates(m), PredNextM(pr))
		if( PredNClauses(pr) == 0 )
			printf("%s\n", PredNameArity(pr) ) ;
}
