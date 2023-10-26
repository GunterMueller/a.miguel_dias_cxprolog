/*
 *   This file is part of the CxProlog system

 *   VarDictionary.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _VarDictionary_
#define _VarDictionary_

/* TEMPORARY VARIABLES */

#define AllocR(i)		registerInUse[i] = true
#define FreeR(i)		registerInUse[i] = false
#define InUseR(i)		registerInUse[i]

extern Bool registerInUse[] ;

void InitTempVarAlloc(void) ;
void ReserveFirstNRegs(int n) ;
int FindR(void) ;


/* VAR DICTIONARY */

typedef struct
{
	Pt var, new ;
	AtomPt name ;
	int nOccurrences ;
	int firstGoal, lastGoal, lastGoalArg ;
	Bool isTemp, isUnsafe, hasGlobalValue ;
	Bool wasReferenced ;
	int permIndex, tempIndex ;
} VarDescriptor, *VarDescriptorPt ;

#define VarDicSize()		(nVarsDic)
#define doVarDic(vd)		dotable(vd, varDic, nVarsDic)

extern VarDescriptorPt varDic ;
extern int nVarsDic ;

void InitVarDictionary(void) ;
void ResetVarDic(void) ;
VarDescriptorPt FindVar(Pt var) ;
Pt LookupVar(CharPt id) ;
VarDescriptorPt FindVariableUsingTemp(int i) ;
void EnterVar(Pt var, int goalN, int argN) ;
Bool WasReferenced(VarDescriptorPt vd) ;
Pt TranslateVar(Pt old, Hdl where) ;
Pt VarDicToEqList(void) ;
void ListVarDic(void) ;

#endif
