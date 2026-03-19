/*
 *   This file is part of the NanoProlog system

 *   Compiler.h
 *   by A.Miguel Dias - 89/11/25
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

#ifndef _Compiler_
#define _Compiler_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif

typedef struct
{
	Pt var, new ;
	AtomPt name ;
	int nOccurrences ;
	int firstGoal, lastGoal, lastGoalArg ;
	Bool isTemp, isUnsafe, hasGlobalValue ;
	Bool wasReferenced ;
	int permIndex, tempIndex ;
} VarDescriptor ;
	
typedef struct
{
	VarDescriptor vars[maxVarsPerTerm] ;
	int nVars ;
	int nTempVars ;
} VarDescriptorTable ;

extern VarDescriptorTable varDic ;
extern Pt codeBuff[] ;

void ResetVarDic(void) ;
void Compiler(Pt term, Bool end, Hdl *cd, long *size) ;

#endif