/*
 *   This file is part of the CxProlog system

 *   Compiler.c
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

/*
	PRE-CONDITIONS:
		The term to be compiled is all-deref
		The term to be compiled has no 'extras'
*/

/* INIT */

void CompilerInit()
{

/* The compiler generates special code when cuts (!/0) are used
	inside these builtin predicates  */

	FunctorIsMeta(LookupFunctorByName("!", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("call", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("not", 1)) = true ;	
	FunctorIsMeta(LookupFunctorByName("\\+", 1)) = true ;	
	FunctorIsMeta(LookupFunctorByName("->", 2)) = true ;	
	FunctorIsMeta(LookupFunctorByName(";", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName(",", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName("try", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("once", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("possible", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("gen", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("<>", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName(">>", 2)) = true ;
	FunctorIsMeta(LookupFunctorByName(">", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("<", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("call_in_empty_context", 1)) = true ;
	FunctorIsMeta(LookupFunctorByName("on_exception", 3)) = true ;
}


/* GLOBAL INFO */

static int currGoalN, nGoals, lastCut ;
static Pt currGoal ;
static Bool hasEnvironment  ;

#define InLastGoal()		( currGoalN == nGoals )

	
/* COMPLETE THE VAR DICTIONARY */

static void CompleteVarDic()
{
	register VarDescriptorPt vd ;
	register int nPermVars, goalN, index ;

/* Characterize vars */
	nPermVars = VarDicSize() ;
	doVarDic(vd) {
		vd->isTemp = vd->lastGoal <= 1 || vd->firstGoal == vd->lastGoal ;
		vd->isUnsafe = !vd->isTemp && vd->firstGoal > 0 ;
 		if( vd->isTemp ) nPermVars-- ;
	}

/* Assign permanent indexes */
	for( goalN = nGoals, index = 0 ; nPermVars > 0 ; goalN-- ) {
		doVarDic(vd)
			if( !vd->isTemp && vd->lastGoal == goalN ) {
				vd->permIndex = index++ ;
				nPermVars-- ;
			}
	}
}

static int EnvSizeAtGoal(int goalN)
{
	register VarDescriptorPt vd ;
	register int nVars = 0 ;
	doVarDic(vd)
		if( !vd->isTemp && vd->lastGoal > goalN )
			nVars++ ;
	return nVars + WordsOf(Environment) ;
}


/* PREPARE HEAD OR GOAL COMPILATION */

static void InitHeadAndFirstGoalTempVars(Pt head, Pt firstGoal)
{
	register Hdl args ;
	int i, nArgsH, nArgsFG ;
	VarDescriptorPt vd ;

	TempVarAllocInit() ;

	if( IsStruct(head) ) {
		args = XStructArgs(head) ;
		nArgsH = XStructArity(head) ;	
	}
	elif( IsList(head) ) {
		args = XListArgs(head) ;
		nArgsH = 2 ;
	}
	else nArgsH = 0 ;
	dotimes(i, nArgsH) {
		if( IsVar(args[i]) ) {
			vd = FindVar(args[i]) ;
			if( vd->isTemp && vd->tempIndex == -1 ) {
				vd->wasReferenced = true ;
				AllocR(vd->tempIndex = i) ;
			}
		}
	}
		
	if( IsStruct(firstGoal) ) {
		args = XStructArgs(firstGoal) ;
		nArgsFG = XStructArity(firstGoal) ;	
	}
	elif( IsList(firstGoal) ) {
		args = XListArgs(firstGoal) ;
		nArgsFG = 2 ;	
	}
	elif( IsVar(firstGoal) ) {
		args = &firstGoal ;
		nArgsFG = 1 ;
	}
	else nArgsFG = 0 ;
	dotimes(i, nArgsFG) {
		if( IsVar(args[i]) ) {
			vd = FindVar(args[i]) ;
			if( vd->isTemp && vd->tempIndex == -1 )
				if( !InUseR(i) ) AllocR(vd->tempIndex = i) ;
		}
	}

	ReserveFirstNRegs(Max(nArgsH, nArgsFG)) ;
}

static void ClearTempIndexOfPermVars()
{
	register VarDescriptorPt vd ;
	doVarDic(vd)
		if( !vd->isTemp ) vd->tempIndex = -1 ;
}

static void InitGoalTempVars(Pt goal)
{
	Hdl args ;
	int i, nArgs ;
	VarDescriptorPt vd ;
	TempVarAllocInit() ;
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

#define SetCutFlag(t)		(t = TagStruct(metaCutFunctor))
#define IsCutFlag(t)		(t == TagStruct(metaCutFunctor))
#define ClearCutFlag(t)		(t = tCutAtom)


/* BUILD VAR DICTIONARY */

static void ProcessGoalArgVars(Pt *t, int argN, Bool inMeta)
{
	register Pt arg = *t ;
	int i ;

	if( IsAtomic(arg) ) {
		if( arg == tCutAtom && inMeta ) {
			SetCutFlag(*t) ;
			lastCut = currGoalN ;
		}
	}

	elif( IsVar(arg) ) EnterVar(arg, currGoalN, argN) ;

	elif( IsStruct(arg) ) {
		FunctorPt f = XStructFunctor(arg) ;
		int n = FunctorArity(f) ;
		Bool meta = inMeta && FunctorIsMeta(f) ;
		Hdl args = XStructArgs(arg) ;
		dotimes(i, n)
			ProcessGoalArgVars(args+i, argN, meta) ;
	}

	elif( IsList(arg) ) {
		ProcessGoalArgVars(&XListHead(arg), argN, false) ;
		ProcessGoalArgVars(&XListTail(arg), argN, false) ;
	}

	else InternalError("ProcessGoalArgVars") ;	
}

static void ProcessGoalVars(Pt goal)
{
	if( IsAtomic(goal) ) {
		if( goal == tCutAtom )
			lastCut = currGoalN ;
	}
 
	elif( IsVar(goal) ) {
		EnterVar(goal, currGoalN, 0) ;
	}

	elif( IsStruct(goal) ) {
		FunctorPt f = XStructFunctor(goal) ;
		Hdl args = XStructArgs(goal) ;
		int nArgs = FunctorArity(f) ;
		Bool meta = FunctorIsMeta(f) ;
		int i ;
		dotimes(i, nArgs)
			ProcessGoalArgVars(args+i, i, meta) ;
	}

	elif( IsList(goal) ) {
		ProcessGoalArgVars(&XListHead(goal), 0, false) ;
		ProcessGoalArgVars(&XListTail(goal), 1, false) ;
	}

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
	nGoals = currGoalN - 1 ;
	CompleteVarDic() ;
}



/* COMPILER */

/* COMPILE GOAL */

#if 1
#define ensureThreshold		memReserve
#else
#define ensureThreshold		0
#endif

static void InsertEnsureFreeSpace(Size size)
{
	Hdl here = ScratchCurr() - size ;
	Gen2(0, 0, 0) ;
	ShiftWords(here, size, 3) ;
	here[0] = cPt(EnsureFreeSpace) ;
	here[1] = cPt(size) ;
	here[2] = hasEnvironment ? cPt(EnvSizeAtGoal(currGoalN-1)) : nil ;
}

static void CompileCall(FunctorPt f)
{
	if( InLastGoal() )
		if( lcoOptimization_flag ) {
			if( hasEnvironment ) Gen0(FDeallocate) ;
			Gen1(Execute, LookupPredicate(f)) ;
			Gen0(FDeallocate) ;
		}
		else {
			Gen2(Call, LookupPredicate(f), EnvSizeAtGoal(currGoalN)) ;
			Gen0(DeallocProceed) ;
		}
	else Gen2(Call, LookupPredicate(f), EnvSizeAtGoal(currGoalN)) ;
}

static void CompileCallVar(void)
{
	if( InLastGoal() )
		if( lcoOptimization_flag ) {
			if( hasEnvironment ) Gen0(FDeallocate) ;
			Gen0(ExecuteVar) ;
		}
		else {
			Gen1(CallVar, EnvSizeAtGoal(currGoalN)) ;
			Gen0(DeallocProceed) ;
		}
	else Gen1(CallVar, EnvSizeAtGoal(currGoalN)) ;
}

static Bool HandlePutConflicts(int idx)
{
	register VarDescriptorPt moveVar ;
	register int i ;
	register Hdl args ;
	
	if( currGoalN != 1 ||
		(moveVar = FindVariableUsingTemp(idx)) == nil ||
		moveVar->lastGoalArg < idx ) {
			AllocR(idx) ;
			return false ;
		}

	args = TermArgs(currGoal) ;
	for( i = idx + 1 ; i <= moveVar->lastGoalArg ; i++ )
		if( args[i] == moveVar->var &&
			( FindVariableUsingTemp(i) == nil || HandlePutConflicts(i) ) ) {
				moveVar->tempIndex = i ;
				Gen2(PutXValue, OutTemp(idx), OutTemp(i)) ;
				return true ;
			}
	moveVar->tempIndex = FindR() ;
	Gen2(PutXValue, OutTemp(idx), OutTemp(moveVar->tempIndex)) ;
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
		if( vd->hasGlobalValue ) Gen1(BuildYValue, OutPerm(vd->permIndex)) ;
		else Gen1(BuildYLocalValue, OutPerm(vd->permIndex)) ;
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
	if( currGoalN == vd->lastGoal && vd->isUnsafe && !vd->hasGlobalValue ) {
		Gen2(PutUnsafeValue, OutPerm(vd->permIndex), OutTemp(i)) ;
		vd->isUnsafe = false ;
	}
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
		/* @@@ if( vd->tempIndex != i ) */
			HandlePutConflicts(i) ;
		if( WasReferenced(vd) )
 			/*if( vd->tempIndex != i )*/ CompilePutYValue(vd, i) ;
			/*else ;*/
		else Gen2(PutYVariable, OutPerm(vd->permIndex), OutTemp(i)) ;
		if( vd->tempIndex == -1 ) vd->tempIndex = i ;
	}
}

#define maxRecs		( retBufferSize / sizeof(short) )
static short *recs = (short *)retBuffer ;
static int nRecs ;

static int CompileGoalRecord(Pt term, int idx)
{
	register Pt t ;
	register int i ;
	int nArgs = TermArity(term) ;
	Hdl args = TermArgs(term) ;

	dotimesrev(i, nArgs) {
		t = args[i] ;

		if( IsRecord(t) && !IsUnitParam(t) ) {
			if( nRecs >= maxRecs ) DatabaseError("Clause too big") ;
			if( IsCutFlag(t) ) {
				recs[nRecs] = FindR() ;
				Gen1(PutCutLevel, OutTemp(recs[nRecs])) ;
			}
			else
				recs[nRecs] = CompileGoalRecord(t, -1) ;
			nRecs++ ;
		}
	}

	if( idx == -1 ) idx = FindR() ;
	if( IsList(term) ) Gen1(PutList, OutTemp(idx)) ;
	else Gen2(PutStructure, XStructFunctor(term), OutTemp(idx)) ;

	dotimes(i, nArgs) {
		t = args[i] ;
		
		if( t == tNilAtom ) Gen0(BuildNil) ;

		elif( IsAtomic(t) ) Gen1(BuildAtomic, t) ;
			
		elif( IsVar(t) ) CompileGoalRecordVar(t) ;
			
		elif( IsRecord(t) ) {
			if( IsUnitParam(t) )
				Gen1(BuildZValue, OutParam(XUnitParam(t))) ;
			else {
				if( IsCutFlag(t) )
					ClearCutFlag(args[i]) ;
				nRecs-- ;
				Gen1(BuildXValue, OutTemp(recs[nRecs])) ;
				FreeR(recs[nRecs]) ;
			}
		}
				
		else InternalError("CompileGoalRecord") ;
	}
	return idx ;
}

static void CompileGoalArgs(Hdl args, int nArgs)
{
	register int i ;
	dotimes(i, nArgs) {
		register Pt t = args[i] ;
		
		if( t == tNilAtom ) {
			HandlePutConflicts(i) ;
			Gen1(PutNil, OutTemp(i)) ;
		}
		
		elif( IsAtomic(t) ) {
			HandlePutConflicts(i) ;
			Gen2(PutAtomic, t, OutTemp(i)) ;
		}
			
		elif( IsVar(t) ) CompileGoalVar(t, i) ;
		
		elif( IsRecord(t) ) {
			HandlePutConflicts(i) ;
			if( IsCutFlag(t) ) {
				Gen1(PutCutLevel, OutTemp(i)) ;
				ClearCutFlag(args[i]) ;
			}
			elif( IsUnitParam(t) )
				Gen2(PutZValue, OutParam(XUnitParam(t)), OutTemp(i)) ;
			else {
				nRecs = 0 ;
				CompileGoalRecord(t, i) ;
			}
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
				if( hasEnvironment ) Gen0(DeallocProceed) ;
				else Gen0(Proceed) ;
			}
		}
		else CompileCall(LookupFunctor(XAtom(goal), 0)) ;
	}
				
	elif( IsVar(goal) ) {
		CompileGoalVar(goal, 0) ;
		CompileCallVar() ;
	}

	elif( IsStruct(goal) ) {
		if( IsUnitParam(goal) ) {
			Gen2(PutZValue, OutParam(XUnitParam(goal)), OutTemp(0)) ;
			CompileCallVar() ;
		}
		else {
			ScratchSave() ;
			CompileGoalArgs(XStructArgs(goal), XStructArity(goal)) ;
			CompileCall(XStructFunctor(goal)) ;
			if( ScratchDistToSaved() > ensureThreshold )
				InsertEnsureFreeSpace(ScratchDistToSaved()) ;
		}
	}
	
	elif( IsList(goal) ) {
		ScratchSave() ;
		CompileGoalArgs(XListArgs(goal), 2) ;
		CompileCall(listFunctor) ;
		if( ScratchDistToSaved() > ensureThreshold )
			InsertEnsureFreeSpace(ScratchDistToSaved()) ;
	}

	elif( IsNumber(goal) ) DatabaseError("Clause goal is a NUMBER") ;

	else InternalError("CompileGoal") ;
}



/* COMPILE HEAD */

static void HandleUnifyXVariableConflicts(VarDescriptorPt vd, int argN)
{
	if( argN < vd->tempIndex &&
		vd->tempIndex < TermArity(currGoal) ) vd->tempIndex = FindR() ;
}

static void CompileUnifyValue(VarDescriptorPt vd)
{
	if( vd->isTemp )
		if( vd->hasGlobalValue )
			Gen1(UnifyXValue, OutTemp(vd->tempIndex)) ;
		else {
			Gen1(UnifyXLocalValue, OutTemp(vd->tempIndex)) ;
			vd->hasGlobalValue = true ;
		}
	else
		if( vd->hasGlobalValue )
			Gen1(UnifyYValue, OutPerm(vd->permIndex)) ;
		else Gen1(UnifyYLocalValue, OutPerm(vd->permIndex)) ;
}

static void CompileUnifyVariable(VarDescriptorPt vd, int argN)
{
	if( vd->isTemp ) {
		if( vd->nOccurrences == 1 ) Gen0(UnifyVoidOne) ;
		else {
			HandleUnifyXVariableConflicts(vd, argN) ;
			Gen1(UnifyXVariable, OutTemp(vd->tempIndex)) ;
		}
	}
	else Gen1(UnifyYVariable, OutPerm(vd->permIndex)) ;
	vd->hasGlobalValue = true ;
}

static void CompileHeadRecordVar(Pt var, int argN)
{
	VarDescriptorPt vd = FindVar(var) ;	
	if( WasReferenced(vd) ) CompileUnifyValue(vd) ;
	else CompileUnifyVariable(vd, argN) ;
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
		/* if( vd->tempIndex == -1 ) vd->tempIndex = i ; @@@ */
	}
}

static void CompileHeadRecordArgs(Hdl args, int nArgs, int argN)
{
	register Pt t ;
	register int i ;

	dotimes(i, nArgs) {
		t = args[i] ;
		
		if( t == tNilAtom ) Gen0(UnifyNil) ;

		elif( IsAtomic(t) ) Gen1(UnifyAtomic, t) ;
			
		elif( IsVar(t) ) CompileHeadRecordVar(t, argN) ;
			
		elif( IsRecord(t) ) {	
			if( IsUnitParam(t) )
				Gen1(UnifyZValue, OutParam(XUnitParam(t))) ;
			else {  
				if( nRecs >= maxRecs ) DatabaseError("Clause too big") ;
				recs[nRecs] = FindR() ;
				Gen1(UnifyXVariable, OutTemp(recs[nRecs])) ;
				nRecs++ ;
			}
		}
		
		else InternalError("CompileHeadRecordArgs") ;
	}
	
	dotimesrev(i, nArgs) {
		t = args[i] ;

		if( IsRecord(t) ) {
			nRecs-- ;
			if( IsList(t) ) {
				Gen1(GetList, OutTemp(recs[nRecs])) ;
				FreeR(recs[nRecs]) ;
				CompileHeadRecordArgs(XListArgs(t), 2, argN) ;
			}
			elif( !IsUnitParam(t) ) {
				Gen2(GetStructure, XStructFunctor(t),
										OutTemp(recs[nRecs])) ;
				FreeR(recs[nRecs]) ;
				CompileHeadRecordArgs(XStructArgs(t), XStructArity(t), argN) ;
			}
		}
	}
}

static void CompileHeadArgs(Hdl args, int nArgs)
{
	register int i ;
	dotimes(i, nArgs) {
		register Pt t = args[i] ;
		
		if( t == tNilAtom ) Gen1(GetNil, OutTemp(i)) ;

		elif( IsAtomic(t) ) Gen2(GetAtomic, t, OutTemp(i)) ;
			
		elif( IsVar(t) ) CompileHeadVar(t, i) ;
			
		elif( IsList(t) ) {
			Gen1(GetList, OutTemp(i)) ;
			nRecs = 0 ;
			CompileHeadRecordArgs(XListArgs(t), 2, i) ;
		}
		
		elif( IsStruct(t) )
			if( IsUnitParam(t) )
				Gen2(GetZValue, OutParam(XUnitParam(t)), OutTemp(i)) ;
			else {
				Gen2(GetStructure, XStructFunctor(t), OutTemp(i)) ;
				nRecs = 0 ;
				CompileHeadRecordArgs(XStructArgs(t), XStructArity(t), i) ;
			}
		
		else InternalError("CompileHeadArgs") ;
	}
}

static void CompileHead(Pt head)
{
	if( IsAtom(head) ) ;
				
	elif( IsStruct(head) ) {
		if( IsUnitParam(head) )
			Error("Clause head is a unit parameter") ;
		else {
			ScratchSave() ;
			CompileHeadArgs(XStructArgs(head), XStructArity(head)) ;
			if( ScratchDistToSaved() > ensureThreshold )
				InsertEnsureFreeSpace(ScratchDistToSaved()) ;
		}
	}

	elif( IsList(head) ) {
		ScratchSave() ;
		CompileHeadArgs(XListArgs(head), 2) ;
		if( ScratchDistToSaved() > ensureThreshold )
			InsertEnsureFreeSpace(ScratchDistToSaved()) ;
	}

	elif( IsNumber(head) ) DatabaseError("Clause head is a NUMBER") ;

	elif( IsVar(head) ) DatabaseError("Clause head is a VARIABLE") ;

	else InternalError("CompileHead") ;
}



/* COMPILER MAIN */

void Compiler(Pt head, Pt body, Hdl *cd, Size *size)
{
	Pt firstGoal = IsThisStruct(body, commaFunctor) ? XStructArg(body,0) : body ;
	Pt remainder ;

	BuildVarDic(head, body) ;
	hasEnvironment = nGoals > 1 || lastCut > 0 || (!lcoOptimization_flag && nGoals ) ;
	InitHeadAndFirstGoalTempVars(head, firstGoal) ;

#if 0
	if( !Booting() ) {
		Write("%s\n", TermAsStr(head)) ;
		Write("%s\n", TermAsStr(body)) ;
		ListVarDic() ;
	}
#endif

	UseScratch() ;
	if( hasEnvironment ) Gen0(FAllocate) ;

	currGoalN = 0 ;
	CompileHead(currGoal = head) ;	
	currGoalN++ ;

	if( nGoals == 0 )
		Gen0(Proceed) ;
	else {
		CompileGoal(currGoal = firstGoal) ;
		currGoalN++ ;

		if( nGoals > 1 ) {
			remainder = XStructArg(body,1) ;
			while( IsThisStruct(remainder, commaFunctor) ) {
				currGoal = XStructArg(remainder,0) ;
				remainder = XStructArg(remainder,1) ;
				InitGoalTempVars(currGoal) ;
				CompileGoal(currGoal) ;
				currGoalN++ ;
			}
			InitGoalTempVars(currGoal = remainder) ;
			CompileGoal(remainder) ;
			currGoalN++ ;
		}
	}
	*cd = ScratchBegin() ;
	*size = ScratchUsed() ;
	FreeScratch() ;

#if 0
	if( !Booting() )
		ListCode(ScratchBegin()) ;
#endif
}
