/*
 *   This file is part of the CxProlog system

 *   Instructions.h
 *   by A.Miguel Dias - 2002/03/28
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Instructions_
#define _Instructions_

extern Pt
	Nop, FAllocate, FDeallocate, Proceed, DeallocProceed, LocalJump,
	Call, EnsureFreeSpace, Execute, CallVar, ExecuteVar,
	Cut, PutCutLevel, Fail, DeletedClause, Undef, NotRunning,

	GetYVariable, GetXValue, GetYValue, GetZValue, GetAtomic, GetNil,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,

	PutXValue, PutYValue, PutZValue, PutUnsafeValue, PutAtomic, PutNil, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,

	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue, UnifyZValue,
	UnifyAtomic, UnifyNil, BuildVoid, BuildVoidOne, BuildXVariable,

	BuildYVariable, BuildXValue, BuildYValue, BuildZValue, BuildXLocalValue,
	BuildYLocalValue, BuildAtomic, BuildNil,

	Import,

	MakeIndex, TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust,
	SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
	SwitchOnAtomic, SwitchOnStructure, DiscardAndFail,
	
	DebugExit, DebugRedo, DebugRetry,

	FirstInst, LastInst ;

#define IsInstruction(t)			( InRange(t,FirstInst,LastInst) )

#define Discard()					CutTo(Bf(B))

void SetupFinalizer(VFunV proc, VoidPt arg) ;
void CutTo(ChoicePointPt cp) ;
CharPt GetInstInfo(Pt inst, CharPt *types) ;
void UserModeInstructions(void) ;
void InstructionsInit(void) ;
void InstructionsInit2(void) ;

#endif
