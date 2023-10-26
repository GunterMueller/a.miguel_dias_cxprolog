/*
 *   This file is part of the CxProlog system

 *   VarDictionary.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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
	Error("The abstract machine cannot handle so large a clause because it only has 256 registers") ;
	return 0 ;
}


/* VAR DICTIONARY */

VarDescriptorPt varDic ;
int nVarsDic ;
static int varDicSize ;

void InitVarDictionary()
{
	varDicSize = maxVarsPerTerm ;
	varDic = TemporaryAllocate(maxVarsPerTerm * WordsOf(VarDescriptor)) ;
}

static void GrowVarDictionary()
{
	varDicSize *= 2 ;
	varDic = TemporaryAllocate(maxVarsPerTerm * WordsOf(VarDescriptor)) ;
/*	CopyBytes(buffer, oldBuffer, *b - oldBuffer) ;
	*b += buffer - oldBuffer ;
	Release(oldBuffer) ;

	if( memoryWarnings_flag )
		Warning("Index table size increased from %ld bytes to %ld bytes",
			indexTableSize * sizeof(IndexElem) / 2,
			indexTableSize * sizeof(IndexElem)) ;
	Release(indexTable) ;*/
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
		Error("Too many variables in term") ;
	return varDic + nVarsDic++ ;
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

	vd = NewVar() ;
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

Pt TranslateVar(Pt old, Hdl where)
{
	register VarDescriptorPt vd ;
	doVarDic(vd)
		if( vd->var == old ) return vd->new ;

	vd = NewVar() ;
	vd->var = old ;
	vd->new = ResetVar(*where) ;
	return vd->new ;
}

Pt VarDicToEqList()
{
	register VarDescriptorPt vd ;
	Pt t = tNilAtom ;
	doVarDic(vd)
		t = MakeBinStruct(
				listFunctor,
				MakeBinStruct(eqFunctor, TagAtom(vd->name), vd->var),
				t) ;
	return t ;
}

void ResetVarDicWithEnv(register Pt l)
{
	ResetVarDic() ;
	l = Drf(l) ;
	while( IsList(l) ) {
		VarDescriptorPt vd = NewVar() ;
		Pt var = Drf(XListHead(l)) ;
		if( not IsVar(var) )
			Error("Unexpected non-var in environment") ;
		vd->var = var ;
		vd->new = nil ;
		l = Drf(XListTail(l)) ;
	}
	if( l != tNilAtom )
		Error("Not a proper list") ;
}

Pt TranslateVarWithEnv(Pt old, Hdl where)
{
	register VarDescriptorPt vd ;
	doVarDic(vd)
		if( vd->var == old ) {
			if( vd->new == nil )
				vd->new = ResetVar(*where) ;
			return vd->new ;
		}
	return old ;
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
