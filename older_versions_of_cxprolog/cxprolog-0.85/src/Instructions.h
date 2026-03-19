/*
 *   This file is part of the CxProlog system

 *   Instructions.h
 *   by A.Miguel Dias - 2002/03/28
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Instructions_
#define _Instructions_

extern Pt
	Nop, FAllocate, FDeallocate, Proceed, DeallocProceed, LocalJump,
	Call, EnsureFreeSpace, Execute, CallVar, ExecuteVar, VisibleExecuteVar, 
	Cut, MetaCut, DynamicCut, PutCutLevel, Fail, Undef, NotRunning,

	GetYVariable, GetXValue, GetYValue, GetZValue, GetAtomic, GetNil,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,

	PutXValue, PutYValue, PutZValue, PutUnsafeValue, PutAtomic, PutNil, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,

	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue, UnifyZValue,
	UnifyAtomic, UnifyNil, BuildVoid, BuildVoidOne, BuildXVariable,

	BuildYVariable, BuildXValue, BuildYValue, BuildZValue, BuildXLocalValue,
	BuildYLocalValue, BuildAtomic, BuildNil,

	CtxExtension, CtxSwitch, Import, CtxEmpty, CtxDown, HCtxPush, HCtxEnter,

	MakeIndex, TryMeElse, RetryMeElse, TrustMe, Try, Retry, Trust,
	SwitchOnTerm0, SwitchOnTerm1, SwitchOnTerm2,
	SwitchOnAtomic, SwitchOnStructure, DiscardAndFail,
	
	DebugExit, DebugRedo, DebugRetry,

	FirstInst, LastInst ;

#define IsInstruction(t)			( InRange(t,FirstInst,LastInst) )

void SetupFinalizer(ProcV proc, VoidPt arg) ;
Bool GetInstInfo(Pt inst, CharPt *name, CharPt *types) ;
void UserModeInstructions(void) ;
void InstructionsInit(void) ;
void InstructionsInit2(void) ;

#endif
