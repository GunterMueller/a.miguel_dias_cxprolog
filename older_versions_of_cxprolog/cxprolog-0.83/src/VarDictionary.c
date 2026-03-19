/*
 *   This file is part of the CxProlog system

 *   VarDictionary.c
 *   by A.Miguel Dias - 2000/04/02
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


/* TEMPORARY VARIABLE ALLOCATION */

#define maxRegisters	maxX

Bool registerInUse[maxRegisters] ;

void TempVarAllocInit()
{
	register int i ;
	dotimes(i, maxRegisters) FreeR(i) ;
}

void ReserveFirstNRegs(int n)
{
	register int i ;
	dotimes(i, n) AllocR(i) ;
}

int FindR()
{
	register int i ;
	dotimes(i, maxRegisters)
		if( not InUseR(i) ) {
			AllocR(i) ;
			return i ;
		}
	DatabaseError("%s%s", "The abstract machine cannot handle so ",
						"large a clause because it only has 256 X-registers") ;
	return 0 ;
}


/* VAR DICTIONARY */

VarDescriptorPt varDic ;
int nVarsDic ;
static int varDicSize ;

void VarDictionaryInit()
{
	varDicSize = maxVarsPerTerm ;
	varDic = TempBlockAllocate(maxVarsPerTerm * WordsOf(VarDescriptor)) ;
}

static void VarDictionaryExpand()
{
	return ;
	varDicSize *= 2 ;
	varDic = TempBlockAllocate(maxVarsPerTerm * WordsOf(VarDescriptor)) ;
}

void ResetVarDic()
{
	nVarsDic = 0 ;
}

VarDescriptorPt FindVar(Pt var)
{
	register VarDescriptorPt vd ;
	doVarDic(vd)
		if( vd->var == var )
			return vd ;
	InternalError("FindVar") ;
	return nil ;
}

static VarDescriptorPt NewVar()
{
	if( nVarsDic >= maxVarsPerTerm )
		DatabaseError("Too many variables in clause") ;
	return varDic + nVarsDic++ ;
}

VarDescriptorPt FindVariableUsingTemp(int i)
{
	register VarDescriptorPt vd ;
	doVarDic(vd)
		if( vd->tempIndex == i )
			return vd ;
	return nil ;
}

void EnterVar(Pt var, int goalN, int argN)
{
	register VarDescriptorPt vd ;

	doVarDic(vd)
		if( vd->var == var )
			goto actualize ;

	vd = NewVar() ;
	vd->var = var ;
	vd->nOccurrences = 0 ;
	vd->firstGoal = goalN ;
	vd->wasReferenced = false ;
	vd->permIndex = vd->tempIndex = -1 ;
	vd->hasGlobalValue = false ;

actualize:
	vd->nOccurrences++ ;
	vd->lastGoal = goalN ;
	vd->lastGoalArg = argN ;
}

Bool WasReferenced(VarDescriptorPt vd)
{
	if( vd->wasReferenced )
		return true ;
	vd->wasReferenced = true ;
	if( vd->isTemp && vd->tempIndex == -1 )
		vd->tempIndex = FindR() ;
	return false ;
}

void ListVarDic()
{
	register VarDescriptorPt vd ;
	Write("\n") ;
	doVarDic(vd) {
		Write(VarName(vd->var)) ;
		Write(vd->isTemp ? " Temp" : vd->isUnsafe ? " Unsf" : " Perm") ;
		Write("%d,%d first=%d, last=%d, lastGoalArg=%d\n",
				vd->tempIndex, vd->permIndex,
				vd->firstGoal,
				vd->lastGoal,
				vd->lastGoalArg) ;
	}
	Write("\n") ;
}
