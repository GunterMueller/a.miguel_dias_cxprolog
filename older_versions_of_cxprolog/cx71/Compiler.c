/*
 *   This file is part of the CxProlog system

 *   Compiler.c
 *   by A.Miguel Dias - 1989/11/25
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

#define DEBUG 0


/* INIT */

void InitCompiler()
{

/* The compiler generates special code when cuts (!/0) are used
	inside these builtin predicates  */

	FunctorIsMeta(LookupFunctorByName("!", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("call", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("not", 1)) = true ;	
	FunctorIsMeta(LookupFunctorByName("->", 2)) = true ;	
	FunctorIsMeta(LookupFunctorByName(";", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName(",", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName("<>", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName(">>", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName("call_in_empty_context", 1)) = true ;
}


/* GLOBAL INFO */

static int currGoalN, currArgN, nGoals, lastCut ;
static Pt currGoal ;
static Bool hasEnvironment  ;

#define InLastGoal()		( currGoalN == nGoals - 1 )

	
/* COMPLETE THE VAR DICTIONARY */

static void CompleteVarDic()
{
	register VarDescriptorPt vd ;
	register int nPermVars, goalN, index ;

/* Characterize vars */
	nPermVars = VarDicSize() ;
	doVarDic(vd) {
		vd->isTemp = vd->lastGoal <= 1 || vd->firstGoal == vd->lastGoal ;
		vd->isUnsafe = not vd->isTemp && vd->firstGoal != 0 ;
		if( vd->isTemp ) nPermVars-- ;
	}

/* Assign permanent indexes */
	for( goalN = nGoals - 1, index = 0 ; nPermVars > 0 ; goalN-- ) {
		doVarDic(vd)
			if( not vd->isTemp && vd->lastGoal == goalN ) {
				vd->permIndex = index++ ;
				nPermVars-- ;
			}
	}
}

static int EnvSizeAtGoal(int goalN)
{
	register VarDescriptorPt vd ;
	register int nVars ;
	
	nVars = 0 ;
	doVarDic(vd)
		if( not vd->isTemp && vd->lastGoal > goalN ) nVars++ ;	
	return nVars + WordsOf(Environment) ;
}


/* PREPARE HEAD OR GOAL COMPILATION */

static void InitHeadAndFirstGoalTempVars(Pt head, Pt firstGoal)
{
	Hdl argsH, argsFG ;
	int i, nArgsH, nArgsFG, nTempReserved ;
	VarDescriptorPt vd ;
	
	InitTempVarAlloc() ;
	argsH = TermArgs(head) ;
	nArgsH = TermArity(head) ;
	argsFG = TermArgs(firstGoal) ;
	nArgsFG = TermArity(firstGoal) ;
	nTempReserved = Max(nArgsH, nArgsFG) ;
	
	dotimes(i, nArgsH) {
		if( IsVar(argsH[i]) ) {
			vd = FindVar(argsH[i]) ;
			if( vd->isTemp && vd->tempIndex == -1 ) {
				vd->wasReferenced = true ;
				AllocR(vd->tempIndex = i) ;
			}
		}
	}
		
	dotimes(i, nArgsFG) {
		if( IsVar(argsFG[i]) ) {
			vd = FindVar(argsFG[i]) ;
			if( vd->isTemp &&  vd->tempIndex == -1 )
				if( not InUseR(i) ) AllocR(vd->tempIndex = i) ;
		}
	}

	ReserveFirstNRegs(nTempReserved) ;
}

static void ClearTempIndexOfPermVars()
{
	register VarDescriptorPt vd ;

	doVarDic(vd)
		if( not vd->isTemp ) vd->tempIndex = -1 ;
}

static void InitGoalTempVars(Pt goal)
{
	Hdl args ;
	int i, nArgs ;
	VarDescriptorPt vd ;
	
	InitTempVarAlloc() ;
	ClearTempIndexOfPermVars() ;
	args = TermArgs(goal) ;
	nArgs = TermArity(goal) ;
	dotimes(i, nArgs) {
		if( IsVar(args[i]) ) {
			vd = FindVar(args[i]) ;
			if( vd->isTemp && vd->tempIndex == -1 )
								AllocR(vd->tempIndex = i) ;
		}
	}
	ReserveFirstNRegs(nArgs) ;
}



/* CUT MACROS */

#define SetCutFlag(t)		(t = TagStruct(&cutFunctor))
#define IsCutFlag(t)		(XStructFunctor(t) == cutFunctor)
#define ClearCutFlag(t)		(t = tCutAtom)


/* BUILD VAR DICTIONARY */

static void ProcessGoalArgVars(Pt *t, Bool inMeta)
{
	register Pt arg = *t ;
	int i ;

	if( IsAtomic(arg) ) {
		if( arg == tCutAtom && inMeta ) {
			SetCutFlag(*t) ;
			lastCut = currGoalN ;
		}
	}

	elif( IsVar(arg) ) EnterVar(arg, currGoalN, currArgN) ;

	elif( IsStruct(arg) ) {
		FunctorPt f = XStructFunctor(arg) ;
		int n = FunctorArity(f) ;
		Bool meta = inMeta && FunctorIsMeta(f) ;
		Hdl args = XStructArgs(arg) ;
		dotimes(i, n)
			ProcessGoalArgVars(args+i, meta) ;
	}

	elif( IsList(arg) ) {
		ProcessGoalArgVars(&XListHead(arg), false) ;
		ProcessGoalArgVars(&XListTail(arg), false) ;
	}

	elif( IsExtra(arg) )
		/* nothing */ ;

	else InternalError("ProcessGoalArgVars") ;	
}

static void ProcessGoalVars(Pt goal)
{
	if( IsAtomic(goal) ) {
		if( goal == tCutAtom )
			lastCut = currGoalN ;
	}
 
	elif( IsVar(goal) ) {
		currArgN = 0 ;
		EnterVar(goal, currGoalN, currArgN) ;
	}

	elif( IsStruct(goal) ) {
		FunctorPt f = XStructFunctor(goal) ;
		int n = FunctorArity(f) ;
		Bool meta = FunctorIsMeta(f) ;
		Hdl args = XStructArgs(goal) ;
		dotimes(currArgN, n)
			ProcessGoalArgVars(args+currArgN, meta) ;
	}

	elif( IsList(goal) ) {
		currArgN = 0 ;
		ProcessGoalArgVars(&XListHead(goal), false) ;
		currArgN = 1 ;
		ProcessGoalArgVars(&XListTail(goal), false) ;
	}

	elif( IsExtra(goal) )
		/* nothing */ ;

	else InternalError("ProcessGoalVars") ;
}

static void BuildVarDic(Pt head, Pt body)
{
	ResetVarDic() ;
	currGoalN = 0 ;
	lastCut = 0 ;
	ProcessGoalVars(head) ;
	currGoalN++ ;
	if( body != tTrueAtom ) {
		while( IsThisStruct(body, commaFunctor) ) {
			ProcessGoalVars(XStructArg(body,0)) ;
			body = XStructArg(body,1) ;
			currGoalN++ ;
		}
		ProcessGoalVars(body) ;
		currGoalN++ ;
	}
	nGoals = currGoalN ;
	CompleteVarDic() ;
}



/* COMPILER */

/* COMPILE GOAL */

static void CompileCCall(PredicatePt pr)
{
	Gen0(PredStartInst(pr)) ;
	if( InLastGoal() ) {
		if( hasEnvironment )
			Gen0(DeallocProceed) ;
		else Gen0(Proceed) ;
	}
}

static void CompilePrologCall(PredicatePt pr)
{
	if( InLastGoal() ) {
		if( hasEnvironment )
			Gen0(Deallocate) ;
		Gen1(Execute, pr) ;
	}
	else Gen2(Call, pr, EnvSizeAtGoal(currGoalN)) ;
}

static void CompileCall(FunctorPt f)
{
	PredicatePt pr = LookupPredicate(f, false) ;
	
	if( PredIsC(pr) ) CompileCCall(pr) ;
	else CompilePrologCall(pr) ;
}

static Bool HandlePutConflicts(int idx)
{
	register VarDescriptorPt dest ;
	register int i ;
	register Hdl args ;
	
	if( currGoalN != 1 ||
		(dest = FindVariableUsingTemp(idx)) == nil ||
		dest->lastGoalArg < idx ) {
			AllocR(idx) ;
			return false ;
		}

	args = TermArgs(currGoal) ;
	for( i = idx + 1 ; i <= dest->lastGoalArg ; i++ )
		if( args[i] == dest->var &&
			( FindVariableUsingTemp(i) == nil || HandlePutConflicts(i) ) ) {
				dest->tempIndex = i ;
				Gen2(PutXValue, OutTemp(idx), OutTemp(i)) ;
				return true ;
			}
	dest->tempIndex = FindR() ;
	Gen2(PutXValue, OutTemp(idx), OutTemp(dest->tempIndex)) ;
	return true ;
}

static void CompileBuildValue(VarDescriptorPt vd)
{
	if( vd->isTemp ) {
		if( vd->hasGlobalValue )
			Gen1(BuildXValue, OutTemp(vd->tempIndex)) ;
		else {
			Gen1(BuildXLocalValue, OutTemp(vd->tempIndex)) ;
			vd->hasGlobalValue = true ;
		}
	}
	else {
		if( vd->hasGlobalValue )
			if( vd->tempIndex != -1 )
				Gen1(BuildXValue, OutTemp(vd->tempIndex)) ;
			else Gen1(BuildYValue, OutPerm(vd->permIndex)) ;
		else {
			if( vd->tempIndex != -1 ) {
				Gen1(BuildXLocalValue, OutTemp(vd->tempIndex)) ;
				vd->hasGlobalValue = true ;
			}
			else Gen1(BuildYLocalValue, OutPerm(vd->permIndex)) ;
		}
	}
}

static void CompileBuildVariable(VarDescriptorPt vd)
{
	if( vd->isTemp ) {
		if( vd->nOccurrences == 1 ) Gen0(BuildVoidOne) ;
		else Gen1(BuildXVariable, OutTemp(vd->tempIndex)) ;
	}
	else Gen1(BuildYVariable, OutPerm(vd->permIndex)) ;
	vd->hasGlobalValue = true ;
}

static void CompilePutYValue(VarDescriptorPt vd, int i)
{	
	if( vd->isUnsafe && not vd->hasGlobalValue && currGoalN == vd->lastGoal ) {
		Gen2(PutUnsafeValue, OutPerm(vd->permIndex), OutTemp(i)) ;
		vd->isUnsafe = false ;
	}
	else
		if( vd->tempIndex != -1 )
				Gen2(PutXValue, OutTemp(vd->tempIndex), OutTemp(i)) ;
		else Gen2(PutYValue, OutPerm(vd->permIndex), OutTemp(i)) ;
}

static void CompileGoalRecordVar(Pt var)
{
	VarDescriptorPt vd = FindVar(var) ;

	if( WasReferenced(vd) ) CompileBuildValue(vd) ;
	else CompileBuildVariable(vd) ;
}

static void CompileGoalVar(Pt var, int i)
{
	VarDescriptorPt vd = FindVar(var) ;
	
	if( vd->isTemp ) {
		if( vd->tempIndex != i ) HandlePutConflicts(i) ;
		if( WasReferenced(vd) )
			if( vd->tempIndex != i )
				Gen2(PutXValue, OutTemp(vd->tempIndex), OutTemp(i)) ;
			else ;
		else {
			if( vd->tempIndex != i )
				Gen2(PutXVariable, OutTemp(vd->tempIndex), OutTemp(i)) ;
			else Gen1(PutXVariableOne, OutTemp(i)) ;
			vd->hasGlobalValue = true ;
		}
	}
	else {
		HandlePutConflicts(i) ;
		if( WasReferenced(vd) ) CompilePutYValue(vd, i) ;
		else Gen2(PutYVariable, OutPerm(vd->permIndex), OutTemp(i)) ;
		if( vd->tempIndex == -1 ) vd->tempIndex = i ;
	}
}

#define nRecsMax	20

static int CompileGoalRecord(Pt term, int idx)
{
	int i, recs[nRecsMax], nRecs = 0 ;
	int nArgs = TermArity(term) ;
	Hdl args = TermArgs(term) ;
	register Pt t ;

	dotimes(i, nArgs) {
		t = args[i] ;

		if( IsRecord(t) ) {
			if( nRecs >= nRecsMax ) Error("Clause too big") ;
			if( IsCutFlag(t) ) {
				int j = FindR() ;
				Gen1(PutCutLevel, OutTemp(j)) ;
				recs[nRecs] = j ;
			}
			else
				recs[nRecs] = CompileGoalRecord(t, -1) ;
			nRecs++ ;
		}
	}

	if( idx == -1) idx = FindR() ;
	if( IsList(term) ) Gen1(PutList, OutTemp(idx)) ;
	else Gen2(PutStructure, XStructFunctor(term), OutTemp(idx)) ;

	nRecs = 0 ;
	dotimes(i, nArgs) {
		t = args[i] ;
		
		if( IsNilAtom(t) ) Gen0(BuildNil) ;

		elif( IsAtomic(t) ) Gen1(BuildAtomic, t) ;
			
		elif( IsVar(t) ) CompileGoalRecordVar(t) ;
			
		elif( IsRecord(t) ) {
			if( IsCutFlag(t) )
				ClearCutFlag(args[i]) ;
			Gen1(BuildXValue, OutTemp(recs[nRecs])) ;
			FreeR(recs[nRecs]) ;
			nRecs++ ;
		}
				
		elif( IsExtra(t) ) Gen1(BuildExtra, t) ;

		else InternalError("CompileGoalRecord") ;
	}

	return idx ;
}

static void CompileGoalArgs(Hdl args, int nArgs)
{
	register Pt t ;
	
	dotimes(currArgN, nArgs) {
		t = args[currArgN] ;
		
		if( IsNilAtom(t) ) {
			HandlePutConflicts(currArgN) ;
			Gen1(PutNil, OutTemp(currArgN)) ;
		}
		
		elif( IsAtomic(t) ) {
			HandlePutConflicts(currArgN) ;
			Gen2(PutAtomic, t, OutTemp(currArgN)) ;
		}
			
		elif( IsVar(t) ) CompileGoalVar(t, currArgN) ;
		
		elif( IsRecord(t) ) {
			HandlePutConflicts(currArgN) ;
			if( IsCutFlag(t) ) {
				Gen1(PutCutLevel, OutTemp(currArgN)) ;
				ClearCutFlag(args[currArgN]) ;
			}
			else
				CompileGoalRecord(t, currArgN) ;
		}
						
		elif( IsExtra(t) ) {
			HandlePutConflicts(currArgN) ;
			Gen2(PutExtra, t, OutTemp(currArgN)) ;
		}

		else InternalError("CompileGoalArgs") ;
	}
}

static void CompileGoal(Pt goal)
{
	if( IsAtom(goal) ) {
		if( goal == tCutAtom ) {
			Gen0(Cut) ;
			if( InLastGoal() ) {
				if( hasEnvironment )
					Gen0(DeallocProceed) ;
				else Gen0(Proceed) ;
			}
		}
		elif( goal == tFailAtom ) {
			Gen0(Fail) ;
			Gen0(Proceed) ; /* In order ListCode doesn't boom */
		}
		else CompileCall(LookupFunctor(XAtom(goal), 0)) ;
	}
				
	elif( IsVar(goal) ) { 
		CompileGoalVar(goal, currArgN = 0) ;
		if( InLastGoal() ) {
			if( hasEnvironment ) Gen0(Deallocate) ;
			Gen0(ExecuteVar) ;
		}
		else Gen1(CallVar, EnvSizeAtGoal(currGoalN)) ;
	}

	elif( IsStruct(goal) ) {
		CompileGoalArgs(XStructArgs(goal), XStructArity(goal)) ;
		CompileCall(XStructFunctor(goal)) ;
	}

	elif( IsList(goal) ) {
		CompileGoalArgs(XListArgs(goal), 2) ;
		CompileCall(listFunctor) ;
	}

	elif( IsNumber(goal) ) Error("Clause goal is a number") ;

	elif( IsExtra(goal) ) Error("Clause goal is an element of an extra type") ;

	else InternalError("CompileGoal") ;
}



/* COMPILE HEAD */

static Bool HandleUnifyXVariableConflicts(VarDescriptorPt vd)
{
	if( currArgN < vd->tempIndex &&
		vd->tempIndex < TermArity(currGoal) ) vd->tempIndex = FindR() ;
}

static void CompileUnifyValue(VarDescriptorPt vd)
{
	if( vd->isTemp ) {
		if( vd->hasGlobalValue )
			Gen1(UnifyXValue, OutTemp(vd->tempIndex)) ;
		else {
			Gen1(UnifyXLocalValue, OutTemp(vd->tempIndex)) ;
			vd->hasGlobalValue = true ;
		}
	}
	else {
		if( vd->hasGlobalValue )
			Gen1(UnifyYValue, OutPerm(vd->permIndex)) ;
		else Gen1(UnifyYLocalValue, OutPerm(vd->permIndex)) ;
	}
}

static void CompileUnifyVariable(VarDescriptorPt vd)
{
	if( vd->isTemp ) {
		if( vd->nOccurrences == 1 ) Gen0(UnifyVoidOne) ;
		else {
			HandleUnifyXVariableConflicts(vd) ;
			Gen1(UnifyXVariable, OutTemp(vd->tempIndex)) ;
		}
	}
	else Gen1(UnifyYVariable, OutPerm(vd->permIndex)) ;
	vd->hasGlobalValue = true ;
}

static void CompileHeadRecordVar(Pt var)
{
	VarDescriptorPt vd = FindVar(var) ;
	
	if( WasReferenced(vd) ) CompileUnifyValue(vd) ;
	else CompileUnifyVariable(vd) ;
}

static void CompileHeadVar(Pt var, int i)
{
	VarDescriptorPt vd = FindVar(var) ;
	
	if( vd->isTemp ) {
		if( WasReferenced(vd) && vd->tempIndex != i )
				Gen2(GetXValue, OutTemp(vd->tempIndex), OutTemp(i)) ;
	}
	else {
		if( WasReferenced(vd) )
			Gen2(GetYValue, OutPerm(vd->permIndex), OutTemp(i)) ;
		else Gen2(GetYVariable, OutPerm(vd->permIndex), OutTemp(i)) ;
	}
}

static void CompileHeadRecordArgs(Hdl args, int nArgs)
{
	int i ;
	struct {
		Pt term ;
		int tempIndex ;
	} recs[nRecsMax] ;
	int nRecs = 0 ;
	register Pt t ;

	dotimes(i, nArgs) {
		t = args[i] ;
		
		if( IsNilAtom(t) ) Gen0(UnifyNil) ;

		elif( IsAtomic(t) ) Gen1(UnifyAtomic, t) ;
			
		elif( IsVar(t) ) CompileHeadRecordVar(t) ;
			
		elif( IsRecord(t) ) {
			if( nRecs >= nRecsMax ) Error("Clause too big") ;
			recs[nRecs].term = t ;
			recs[nRecs].tempIndex = FindR() ;
			Gen1(UnifyXVariable, OutTemp(recs[nRecs].tempIndex)) ;
			nRecs++ ;
		}
		
		elif( IsExtra(t) ) Gen1(UnifyExtra, t) ;

		else InternalError("CompileHeadRecordArgs") ;
	}
	
	dotimes(i, nRecs)
		if( IsList(recs[i].term) ) {
			Gen1(GetList, OutTemp(recs[i].tempIndex)) ;
			FreeR(recs[i].tempIndex) ;
			CompileHeadRecordArgs(XListArgs(recs[i].term), 2) ;
		}
		else {
			StructPt st = XStruct(recs[i].term) ;
			
			Gen2(GetStructure, StructFunctor(st), OutTemp(recs[i].tempIndex)) ;
			FreeR(recs[i].tempIndex) ;
			CompileHeadRecordArgs(StructArgs(st), StructArity(st)) ;
		}
}

static void CompileHeadArgs(Hdl args, int nArgs)
{
	register Pt t ;
	
	dotimes(currArgN, nArgs) {
		t = args[currArgN] ;
		
		if( IsNilAtom(t) ) Gen1(GetNil, OutTemp(currArgN)) ;

		elif( IsAtomic(t) ) Gen2(GetAtomic, t, OutTemp(currArgN)) ;
			
		elif( IsVar(t) ) CompileHeadVar(t, currArgN) ;
			
		elif( IsList(t) ) {
			Gen1(GetList, OutTemp(currArgN)) ;
			CompileHeadRecordArgs(XListArgs(t), 2) ;
		}
		
		elif( IsStruct(t) ) {
			Gen2(GetStructure, XStructFunctor(t), OutTemp(currArgN)) ;
			CompileHeadRecordArgs(XStructArgs(t), XStructArity(t)) ;
		}
		
		elif( IsExtra(t) ) Gen2(GetExtra, t, OutTemp(currArgN)) ;

		else InternalError("CompileHeadArgs") ;
	}
}

static void CompileHead(Pt head)
{
	if( IsAtom(head) ) ;
				
	elif( IsStruct(head) )
		CompileHeadArgs(XStructArgs(head), XStructArity(head)) ;

	elif( IsList(head) ) CompileHeadArgs(XListArgs(head), 2) ;

	elif( IsNumber(head) ) Error("Clause head is a number") ;

	elif( IsExtra(head) ) Error("Clause head is an element of an extra type") ;

	elif( IsVar(head) ) Error("Clause head is a variable") ;

	else InternalError("CompileHead") ;
	
	if( nGoals == 1 ) Gen0(Proceed) ;
}



/* COMPILER MAIN */

static void CompilePrimitiveClause(int n, Hdl *cd, long *size) {
	CodeReset() ;
	switch( n ) {
		case 1:
			Gen0(Allocate) ; 
			Gen1(PushCtxCallVar, WordsOf(Environment)) ; 
			Gen0(DeallocProceed) ; 
			break ;
		case 2:
			Gen0(Allocate) ; 
			Gen1(SwitchCtxCallVar, WordsOf(Environment)) ; 
			Gen0(DeallocProceed) ; 
			break ;
		case 3:
			Gen0(Allocate) ; 
			Gen1(EmptyCtxCallVar, WordsOf(Environment)) ; 
			Gen0(DeallocProceed) ; 
			break ;
		case 4:
			Gen0(Allocate) ; 
			Gen0(PushHCtx) ; 
			Gen1(CallVar,WordsOf(Environment)) ; 
			Gen0(PopHCtx) ; 
			Gen0(DeallocProceed) ; 
			break ;
		case 5:
			Gen0(Allocate) ; 
			Gen0(EnterHCtx) ; 
			Gen1(CallVar,1 + WordsOf(Environment)) ; 
			Gen0(ExitHCtx) ; 
			Gen0(DeallocProceed) ; 
			break ;
		default:
			Error("Not a primitive special predicate (in CompilePrimitiveClause)") ;
	}
	*cd = CodeStart() ;
	*size = CodeSize() ;
}

void Compiler(Pt head, Pt body, Hdl *cd, long *size)
{
	Pt firstGoal = IsThisStruct(body, commaFunctor) ? XStructArg(body,0) : body ;

	if( IsThisStruct(body, primitiveFunctor) ) {
		CompilePrimitiveClause(XTestInt(XStructArg(body,0)), cd, size) ;
		return ;
	}

	BuildVarDic(head, body) ;
	hasEnvironment = nGoals > 2 || lastCut > 0 ;
	InitHeadAndFirstGoalTempVars(head, firstGoal) ;

#if DEBUG == 1
	WritelnTerm(head) ;
	WritelnTerm(body) ;
	ListVarDic() ;
#endif

	CodeReset() ;
	if( hasEnvironment ) Gen0(Allocate) ;
	CompileHead(currGoal = head) ;	
	if( nGoals > 1 ) {
		currGoalN = 1 ;
		CompileGoal(currGoal = firstGoal) ;
		currGoalN++ ;

		if( nGoals > 2 ) {
			body = XStructArg(body,1) ;
			while( IsThisStruct(body, commaFunctor) ) {
				if( CodeOverflow() )
					FatalError("Code overflow: clause too large") ;
				currGoal = XStructArg(body,0) ;
				body = XStructArg(body,1) ;
				InitGoalTempVars(currGoal) ;
				CompileGoal(currGoal) ;
				currGoalN++ ;
			}
			InitGoalTempVars(currGoal = body) ;
			CompileGoal(body) ;
			currGoalN++ ;
		}
	}
	*cd = CodeStart() ;
	*size = CodeSize() ;

#if DEBUG == 1
	ListCode(CodeStart()) ;
	exit(0) ;
#endif
}
