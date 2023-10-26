/*
 *   This file is part of the NanoProlog system

 *   Module.h
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

#ifndef _Module_
#define _Module_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif

typedef struct Module
{
	struct Module *next ;		/* Next module in the hash chain */
	AtomPt name ;				/* Module name */
	PredicatePt predicates ;	/* Module predicate list */
} Module, *ModulePt ;

#define cModulePt(p)			((ModulePt)(p))

extern ModulePt sourceModule, sysModule ;

#define SourceModule()				(sourceModule)
#define SetSourceModule(m)			(sourceModule = m) 

#define ModuleName(m)				(m)->name
#define ModuleText(m)				AtomName((m)->name)
#define ModuleNext(m)				(m)->next
#define ModulePredicates(m)			(m)->predicates

void InitModules(void) ;
ModulePt LookupModule(AtomPt name) ;
void ListModules(void) ;
void ListModulePreds(ModulePt m) ;
void ListModuleUndefPreds(ModulePt m) ;

#endif