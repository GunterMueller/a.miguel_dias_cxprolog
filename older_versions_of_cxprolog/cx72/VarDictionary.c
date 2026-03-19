/*
 *   This file is part of the CxProlog system

 *   VarDictionary.c
 *   by A.Miguel Dias - 2000/04/02
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


/* TEMPORARY VARIABLE ALLOCATION */

#define maxRegisters	maxX

Bool registerInUse[maxRegisters] ;

void InitTempVarAlloc()
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
	InternalError("FindR") ;
	return 0 ;
}


/* VAR DICTIONARY */

VarDescriptor varDic[maxVarsPerTerm] ;
int nVarsDic ;

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

Pt LookupVar(CharPt id)
{
	register VarDescriptorPt vd ;
	register AtomPt varName ;

	if( id[0] == '_' && id[1] == '\0' )
		return MakeVar() ;

	varName = LookupAtom(id) ;
	doVarDic(vd)
		if( vd->name == varName ) return vd->var ;
	if( nVarsDic++ >= maxVarsPerTerm )
		Error("Too many variables in term") ;
	vd->name = varName ;
	/*if( (vd->var = LookupCurrUnitParameter(varName)) == nil ) */
	vd->var = MakeVar() ;
	return vd->var ;
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

	if( nVarsDic++ >= maxVarsPerTerm )
		Error("Too many variables in clause") ;
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

Pt TranslateVar(Pt old, Hdl where)
{
	register VarDescriptorPt vd ;

	doVarDic(vd)
		if( vd->var == old ) return vd->new ;
	if( nVarsDic++ >= maxVarsPerTerm )
		Error("Too many variables in term") ;
	vd->var = old ;
	vd->new = ResetVar(*where) ;
	return vd->new ;
}

Pt VarDicToEqList()
{
	register VarDescriptorPt vd ;
	Pt h, t ;

	if( nVarsDic == 0 )
		return tNilAtom ;

	t = tNilAtom ;
	doVarDic(vd)
		t = MakeList(
				MakeBinStruct(eqFunctor, TagAtom(vd->name), vd->var),
				t) ;
	return t ;
}

void ListVarDic()
{
	register VarDescriptorPt vd ;

	WriteStd("\n") ;
	doVarDic(vd) {
		WriteStd(VarName(vd->var)) ;
		WriteStd(vd->isTemp ? " Temp" : vd->isUnsafe ? " Unsf" : " Perm") ;
		WriteStd("%d,%d first=%d, last=%d, lastGoalArg=%d\n",
				vd->tempIndex, vd->permIndex,
				vd->firstGoal,
				vd->lastGoal,
				vd->lastGoalArg) ;
	}
	WriteStd("\n") ;
}
