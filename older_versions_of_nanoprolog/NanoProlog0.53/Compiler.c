/*
 *   This file is part of the NanoProlog system

 *   Compiler.c
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

 931203: macro PatchCode removed
 931117: release of version 0.5

*/

#include "NanoProlog.h"

#define DEBUG 0


/* TEMPORARY VARIABLE ALLOCATION */

#define maxRegisters	maxX

#define AllocR(i)		registerInUse[i] = true
#define FreeR(i)		registerInUse[i] = false
#define InUseR(i)		registerInUse[i]

static Bool registerInUse[maxRegisters] ;

static void InitTempVarAlloc()
{
	register int i ;
	
	dotimes(i, maxRegisters) FreeR(i) ;
}

static void ReserveFirstNRegs(int n)
{
	register int i ;
	
	dotimes(i, n) AllocR(i) ;
}

static int FindR()
{
	register int i ;
	
	dotimes(i, maxRegisters)
		if( not InUseR(i) )
		{
			AllocR(i) ;
			return( i ) ;
		}
	InternalError("FindR") ;
	return( 0 ) ;
}



/* VAR DICTIONARY */

static int currGoalN, currArgN, nGoals ;
static Pt currGoal ;
Bool hasEnvironment ;
VarDescriptorTable varDic ;
Pt codeBuff[maxCodePerClause] ;

#define NPermVars()			( varDic.nVars - varDic.nTempVars )
#define InLastGoal()		( currGoalN == nGoals - 1 )

void ResetVarDic()
{
	varDic.nVars = 0 ;
}

static VarDescriptor *FindVar(Pt var)
{
	register VarDescriptor *vd ;

	dotable(vd, varDic.vars, varDic.nVars)
		if( vd->var == var ) return( vd ) ;
	InternalError("FindVar") ;
	return( nil ) ;
}

static VarDescriptor *FindVariableUsingTemp(int i)
{
	register VarDescriptor *vd ;

	dotable(vd, varDic.vars, varDic.nVars)
		if( vd->tempIndex == i ) return( vd ) ;
	return( nil ) ;
}

static void EnterVar(Pt var)
{
	register VarDescriptor *vd ;

	dotable(vd, varDic.vars, varDic.nVars)
		if( vd->var == var ) goto actualize ;

	if( varDic.nVars++ >= maxVarsPerTerm )
					Error("Too many variables in clause") ;
	vd->var = var ;
	vd->nOccurrences = 0 ;
	vd->firstGoal = currGoalN ;
	vd->wasReferenced = false ;
	vd->permIndex = vd->tempIndex = -1 ;
	vd->hasGlobalValue = false ;

actualize:
	vd->nOccurrences++ ;
	vd->lastGoal = currGoalN ;
	vd->lastGoalArg = currArgN ;
}

static Bool WasReferenced(VarDescriptor *vd)
{
	if( vd->wasReferenced ) return( true ) ;

	vd->wasReferenced = true ;
	if( vd->isTemp && vd->tempIndex == -1 )
		vd->tempIndex = FindR() ;
	return( false ) ;
}
	 


/* COMPLETE VAR DICTIONARY */

static void CharacterizeVars()
{
	register VarDescriptor *vd ;

	varDic.nTempVars = 0 ;
	dotable(vd, varDic.vars, varDic.nVars)
	{
		vd->isTemp = vd->lastGoal <= 1 || vd->firstGoal == vd->lastGoal ;
		vd->isUnsafe = not vd->isTemp && vd->firstGoal != 0 ;
		if( vd->isTemp ) varDic.nTempVars++ ;
	}
}

static void AssignPermIndexes()
{
	register VarDescriptor *vd ;
	int nPermVars, goalN, index ;

	for( nPermVars = NPermVars(), goalN = nGoals - 1, index = 0
		 ; nPermVars > 0 ; goalN-- )
			dotable(vd, varDic.vars, varDic.nVars)
			if( not vd->isTemp && vd->lastGoal == goalN )
			{
				vd->permIndex = index++ ;
				nPermVars-- ;
			}
}

static int EnvSizeAtGoal(int goalN)
{
	register VarDescriptor *vd ;
	register int nVars ;
	
	nVars = 0 ;
	dotable(vd, varDic.vars, varDic.nVars)
		if( not vd->isTemp && vd->lastGoal > goalN ) nVars++ ;	
	return( nVars + WordsOf(Environment) ) ;
}

static void CompleteVarDic()
{
	CharacterizeVars() ;
	AssignPermIndexes() ;
}

static void ListVarDic()
{
	register VarDescriptor *vd ;

	printf("\n") ;
	dotable(vd, varDic.vars, varDic.nVars)
	{
		printf(VarName(vd->var)) ;
		printf(vd->isTemp ? " Temp" : vd->isUnsafe ? " Unsf" : " Perm") ;
		printf("%d,%d first=%d, last=%d, lastGoalArg=%d\n",
				vd->tempIndex, vd->permIndex,
				vd->firstGoal,
				vd->lastGoal,
				vd->lastGoalArg) ;
	}
	printf("\n") ;
}



/* PREPARE HEAD OR GOAL COMPILATION */

static void InitHeadAndFirstGoalTempVars(Pt head, Pt firstGoal)
{
	Hdl argsH, argsFG ;
	int i, nArgsH, nArgsFG, nTempReserved ;
	VarDescriptor *vd ;
	
	InitTempVarAlloc() ;
	argsH = TermArgs(head) ;
	nArgsH = TermArity(head) ;
	argsFG = TermArgs(firstGoal) ;
	nArgsFG = TermArity(firstGoal) ;
	nTempReserved = Max(nArgsH, nArgsFG) ;
	
	dotimes(i, nArgsH)
	{
		if( IsVar(argsH[i]) )
		{
			vd = FindVar(argsH[i]) ;
			if( vd->isTemp && vd->tempIndex == -1 )
			{
				vd->wasReferenced = true ;
				AllocR(vd->tempIndex = i) ;
			}
		}
	}
		
	dotimes(i, nArgsFG)
	{
		if( IsVar(argsFG[i]) )
		{
			vd = FindVar(argsFG[i]) ;
			if( vd->isTemp &&  vd->tempIndex == -1 )
				if( not InUseR(i) ) AllocR(vd->tempIndex = i) ;
		}
	}

	ReserveFirstNRegs(nTempReserved) ;
}

static void ClearTempIndexOfPermVars()
{
	register VarDescriptor *vd ;

	dotable(vd, varDic.vars, varDic.nVars)
		if( not vd->isTemp ) vd->tempIndex = -1 ;
}

static void InitGoalTempVars(Pt goal)
{
	Hdl args ;
	int i, nArgs ;
	VarDescriptor *vd ;
	
	InitTempVarAlloc() ;
	ClearTempIndexOfPermVars() ;
	args = TermArgs(goal) ;
	nArgs = TermArity(goal) ;
	dotimes(i, nArgs)
	{
		if( IsVar(args[i]) )
		{
			vd = FindVar(args[i]) ;
			if( vd->isTemp && vd->tempIndex == -1 )
								AllocR(vd->tempIndex = i) ;
		}
	}
	ReserveFirstNRegs(nArgs) ;
}



/* BUILD VAR DICTIONARY */

static void ProcessGoalStructArgVars(Pt arg)
{
	int i, n ;

	if( IsAtomic(arg) ) ;
 
	elif( IsVar(arg) ) EnterVar(arg) ;

	elif( IsStruct(arg) )
	{
		n = XStructArity(arg) ;
		dotimes(i, n)
			ProcessGoalStructArgVars(XStructArg(arg,i)) ;
	}

	elif( IsList(arg) )
	{
		ProcessGoalStructArgVars(XListHead(arg)) ;
		ProcessGoalStructArgVars(XListTail(arg)) ;
	}

	else InternalError("ProcessGoalStructArgVars") ;	
}

static void ProcessGoalArgVars(Pt arg)
{
	int i, n ;

	if( IsAtomic(arg) ) ;
 
	elif( IsVar(arg) ) EnterVar(arg) ;

	elif( IsStruct(arg) )
	{
		n = XStructArity(arg) ;
		dotimes(i, n)
			ProcessGoalStructArgVars(XStructArg(arg,i)) ;
	}

	elif( IsList(arg) )
	{
		ProcessGoalStructArgVars(XListHead(arg)) ;
		ProcessGoalStructArgVars(XListTail(arg)) ;
	}

	else InternalError("ProcessGoalArgVars") ;	
}

static void ProcessGoalVars(Pt goal)
{
	int n ;

	if( IsAtomic(goal) ) ;
 
	elif( IsVar(goal) )
	{
		currArgN = 0 ;
		EnterVar(goal) ;
	}

	elif( IsStruct(goal) )
	{
		n = XStructArity(goal) ;
		dotimes(currArgN, n)
			ProcessGoalArgVars(XStructArg(goal,currArgN)) ;
	}

	elif( IsList(goal) )
	{
		currArgN = 0 ;
		ProcessGoalArgVars(XListHead(goal)) ;
		currArgN = 1 ;
		ProcessGoalArgVars(XListTail(goal)) ;
	}

	else InternalError("ProcessGoalVars") ;
}

static void BuildVarDic(Pt head, Pt body)
{
	ResetVarDic() ;
	currGoalN = 0 ;
	ProcessGoalVars(head) ;
	currGoalN++ ;
	if( body != TagAtom(trueAtom) )
	{
		while( IsThisStruct(body, commaFunctor) )
		{
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

#define AddInst0(c)			if( codePt >= codeEnd )			\
								Error("Clause too big") ;	\
							else *codePt++ = cPt(c)

#define AddInst1(c1,c2)		if( codePt >= codeEnd )			\
								Error("Clause too big") ;	\
							else (*codePt++ = cPt(c1),		\
								  *codePt++ = cPt(c2))

#define AddInst2(c1,c2,c3)	if( codePt >= codeEnd )			\
								Error("Clause too big") ;	\
							else (*codePt++ = cPt(c1),		\
								  *codePt++ = cPt(c2),		\
								  *codePt++ = cPt(c3))
			
#define GetCode()		(*codePt++)
#define CodeSize()		(codePt - code)

static Hdl code, codePt, codeEnd ;

static void ResetCode()
{
	code = codePt = codeBuff ;
	codeEnd = code + maxCodePerClause - 3 ;
}

#define OutTemp(tempIndex)		( X + tempIndex )
#define OutPerm(permIndex)		( - WordsOf(Environment) - 1 - permIndex )



/* COMPILE GOAL */

static void CompileCCall(Predicate *pr)
{
	AddInst0(PredProc(pr)) ;
	if( InLastGoal() )
	{
		if( hasEnvironment ) AddInst0(Deallocate) ;
		AddInst0(Proceed) ;
	}
}

static void CompilePrologCall(Predicate *pr)
{
	if( InLastGoal() )
	{
		if( hasEnvironment ) AddInst0(Deallocate) ;
		AddInst1(Execute, pr) ;
	}
	else AddInst2(Call, pr, EnvSizeAtGoal(currGoalN)) ;
}

static void CompileCall(char *predName, int predArity)
{
	Predicate *pr = LookupPredicate2(predName, predArity) ;
	
	if( PredIsC(pr) ) CompileCCall(pr) ;
	else CompilePrologCall(pr) ;
}

static Bool HandlePutConflicts(int idx)
{
	register VarDescriptor *dest ;
	register int i ;
	register Hdl args ;
	
	if( currGoalN != 1 ||
		(dest = FindVariableUsingTemp(idx)) == nil ||
		dest->lastGoalArg < idx )
		{
			AllocR(idx) ;
			return( false ) ;
		}

	args = TermArgs(currGoal) ;
	for( i = idx + 1 ; i <= dest->lastGoalArg ; i++ )
		if( args[i] == dest->var &&
			( FindVariableUsingTemp(i) == nil || HandlePutConflicts(i) ) )
			{
				dest->tempIndex = i ;
				AddInst2(PutXValue, OutTemp(idx), OutTemp(i)) ;
				return( true ) ;
			}
	dest->tempIndex = FindR() ;
	AddInst2(PutXValue, OutTemp(idx), OutTemp(dest->tempIndex)) ;
	return( true ) ;
}

static void CompileBuildValue(VarDescriptor *vd)
{
	if( vd->isTemp )
	{
		if( vd->hasGlobalValue )
			AddInst1(BuildXValue, OutTemp(vd->tempIndex)) ;
		else
		{
			AddInst1(BuildXLocalValue, OutTemp(vd->tempIndex)) ;
			vd->hasGlobalValue = true ;
		}
	}
	else
	{
		if( vd->hasGlobalValue )
			if( vd->tempIndex != -1 )
				AddInst1(BuildXValue, OutTemp(vd->tempIndex)) ;
			else AddInst1(BuildYValue, OutPerm(vd->permIndex)) ;
		else
		{
			if( vd->tempIndex != -1 )
			{
				AddInst1(BuildXLocalValue, OutTemp(vd->tempIndex)) ;
				vd->hasGlobalValue = true ;
			}
			else AddInst1(BuildYLocalValue, OutPerm(vd->permIndex)) ;
		}
	}
}

static void CompileBuildVariable(VarDescriptor *vd)
{
	if( vd->isTemp )
	{
		if( vd->nOccurrences == 1 ) AddInst0(BuildVoidOne) ;
		else AddInst1(BuildXVariable, OutTemp(vd->tempIndex)) ;
	}
	else AddInst1(BuildYVariable, OutPerm(vd->permIndex)) ;
	vd->hasGlobalValue = true ;
}

static void CompilePutYValue(VarDescriptor *vd, int i)
{	
	if( vd->isUnsafe && not vd->hasGlobalValue && currGoalN == vd->lastGoal )
	{
		AddInst2(PutUnsafeValue, OutPerm(vd->permIndex), OutTemp(i)) ;
		vd->isUnsafe = false ;
	}
	else
		if( vd->tempIndex != -1 )
				AddInst2(PutXValue, OutTemp(vd->tempIndex), OutTemp(i)) ;
		else AddInst2(PutYValue, OutPerm(vd->permIndex), OutTemp(i)) ;
}

static void CompileGoalRecordVar(Pt var)
{
	VarDescriptor *vd = FindVar(var) ;

	if( WasReferenced(vd) ) CompileBuildValue(vd) ;
	else CompileBuildVariable(vd) ;
}

static void CompileGoalVar(Pt var, int i)
{
	VarDescriptor *vd = FindVar(var) ;
	
	if( vd->isTemp )
	{
		if( vd->tempIndex != i ) HandlePutConflicts(i) ;
		if( WasReferenced(vd) )
			if( vd->tempIndex != i )
				AddInst2(PutXValue, OutTemp(vd->tempIndex), OutTemp(i)) ;
			else ;
		else
		{
			if( vd->tempIndex != i )
				AddInst2(PutXVariable, OutTemp(vd->tempIndex), OutTemp(i)) ;
			else AddInst1(PutXVariableOne, OutTemp(i)) ;
			vd->hasGlobalValue = true ;
		}
	}
	else
	{
		HandlePutConflicts(i) ;
		if( WasReferenced(vd) ) CompilePutYValue(vd, i) ;
		else AddInst2(PutYVariable, OutPerm(vd->permIndex), OutTemp(i)) ;
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

	dotimes(i, nArgs)
	{
		if( IsRecord(args[i]) )
		{
			if( nRecs >= nRecsMax ) Error("Clause too big") ;
			recs[nRecs] = CompileGoalRecord(args[i], -1) ;
			nRecs++ ;
		}
	}

	if( idx == -1) idx = FindR() ;
	if( IsList(term) ) AddInst1(PutList, OutTemp(idx)) ;
	else AddInst2(PutStructure, XStructFunctor(term), OutTemp(idx)) ;

	nRecs = 0 ;
	dotimes(i, nArgs)
	{
		t = args[i] ;
		
		if( IsNilAtom(t) ) AddInst0(BuildNil) ;

		elif( IsAtomic(t) ) AddInst1(BuildConstant, t) ;
			
		elif( IsVar(t) ) CompileGoalRecordVar(t) ;
			
		elif( IsRecord(t) )
		{
			AddInst1(BuildXValue, OutTemp(recs[nRecs])) ;
			FreeR(recs[nRecs]) ;
			nRecs++ ;
		}
				
		else InternalError("CompileGoalRecord") ;
	}

	return( idx ) ;
}

static void CompileGoalArgs(Hdl args, int nArgs)
{
	register Pt t ;
	
	dotimes(currArgN, nArgs)
	{
		t = args[currArgN] ;
		
		if( IsNilAtom(t) )
		{
			HandlePutConflicts(currArgN) ;
			AddInst1(PutNil, OutTemp(currArgN)) ;
		}
		
		elif( IsAtomic(t) )
		{
			HandlePutConflicts(currArgN) ;
			AddInst2(PutConstant, t, OutTemp(currArgN)) ;
		}
			
		elif( IsVar(t) ) CompileGoalVar(t, currArgN) ;
		
		elif( IsRecord(t) )
		{
			HandlePutConflicts(currArgN) ;
			CompileGoalRecord(t, currArgN) ;
		}
						
		else InternalError("CompileGoalArgs") ;
	}
}

static void CompileGoal(Pt goal)
{
	if( IsAtom(goal) )
	{
		if( goal == tCutAtom )
		{
			AddInst0(Cut) ;
			if( InLastGoal() )
			{
				if( hasEnvironment ) AddInst0(Deallocate) ;
				AddInst0(Proceed) ;
			}
		}
		elif( goal == tFailAtom )
		{
			AddInst0(Fail) ;
			AddInst0(Proceed) ; /* In order ListCode doesn't boom */
		}
		else CompileCall(XAtomName(goal), 0) ;
	}
				
	elif( IsVar(goal) )
	{ 
		CompileGoalVar(goal, currArgN = 0) ;
		if( InLastGoal() )
		{
			if( hasEnvironment ) AddInst0(Deallocate) ;
			AddInst0(ExecuteVar) ;
		}
		else AddInst1(CallVar, EnvSizeAtGoal(currGoalN)) ;
 	}

	elif( IsStruct(goal) )
	{
		CompileGoalArgs(XStructArgs(goal), XStructArity(goal)) ;
		CompileCall(XStructName(goal), XStructArity(goal)) ;
	}

	elif( IsList(goal) )
	{
		CompileGoalArgs(XListArgs(goal), 2) ;
		CompileCall(".", 2) ;
	}

	elif( IsNumber(goal) ) Error("Clause goal is a number") ;

	else InternalError("CompileGoal") ;
}



/* COMPILE HEAD */

static Bool HandleUnifyXVariableConflicts(VarDescriptor *vd)
{
	if( currArgN < vd->tempIndex &&
		vd->tempIndex < TermArity(currGoal) ) vd->tempIndex = FindR() ;
}

static void CompileUnifyValue(VarDescriptor *vd)
{
	if( vd->isTemp )
	{
		if( vd->hasGlobalValue )
			AddInst1(UnifyXValue, OutTemp(vd->tempIndex)) ;
		else
		{
			AddInst1(UnifyXLocalValue, OutTemp(vd->tempIndex)) ;
			vd->hasGlobalValue = true ;
		}
	}
	else
	{
		if( vd->hasGlobalValue )
			AddInst1(UnifyYValue, OutPerm(vd->permIndex)) ;
		else AddInst1(UnifyYLocalValue, OutPerm(vd->permIndex)) ;
	}
}

static void CompileUnifyVariable(VarDescriptor *vd)
{
	if( vd->isTemp )
	{
		if( vd->nOccurrences == 1 ) AddInst0(UnifyVoidOne) ;
		else
		{
			HandleUnifyXVariableConflicts(vd) ;
			AddInst1(UnifyXVariable, OutTemp(vd->tempIndex)) ;
		}
	}
	else AddInst1(UnifyYVariable, OutPerm(vd->permIndex)) ;
	vd->hasGlobalValue = true ;
}

static void CompileHeadRecordVar(Pt var)
{
	VarDescriptor *vd = FindVar(var) ;
	
	if( WasReferenced(vd) ) CompileUnifyValue(vd) ;
	else CompileUnifyVariable(vd) ;
}

static void CompileHeadVar(Pt var, int i)
{
	VarDescriptor *vd = FindVar(var) ;
	
	if( vd->isTemp )
	{
		if( WasReferenced(vd) && vd->tempIndex != i )
				AddInst2(GetXValue, OutTemp(vd->tempIndex), OutTemp(i)) ;
	}
	else
	{
		if( WasReferenced(vd) )
			AddInst2(GetYValue, OutPerm(vd->permIndex), OutTemp(i)) ;
		else AddInst2(GetYVariable, OutPerm(vd->permIndex), OutTemp(i)) ;
	}
}

static void CompileHeadRecordArgs(Hdl args, int nArgs)
{
	int i ;
	struct
	{
		Pt term ;
		int tempIndex ;
	} recs[nRecsMax] ;
	int nRecs = 0 ;
	register Pt t ;

	dotimes(i, nArgs)
	{
		t = args[i] ;
		
		if( IsNilAtom(t) ) AddInst0(UnifyNil) ;

		elif( IsAtomic(t) ) AddInst1(UnifyConstant, t) ;
			
		elif( IsVar(t) ) CompileHeadRecordVar(t) ;
			
		elif( IsRecord(t) )
		{
			if( nRecs >= nRecsMax ) Error("Clause too big") ;
			recs[nRecs].term = t ;
			recs[nRecs].tempIndex = FindR() ;
			AddInst1(UnifyXVariable, OutTemp(recs[nRecs].tempIndex)) ;
			nRecs++ ;
		}
		
		else InternalError("CompileHeadRecordArgs") ;
	}
	
	dotimes(i, nRecs)
		if( IsList(recs[i].term) )
		{
			AddInst1(GetList, OutTemp(recs[i].tempIndex)) ;
			FreeR(recs[i].tempIndex) ;
			CompileHeadRecordArgs(XListArgs(recs[i].term), 2) ;
		}
		else
		{
			Struct *st = XStruct(recs[i].term) ;
			
			AddInst2(GetStructure, StructFunctor(st), OutTemp(recs[i].tempIndex)) ;
			FreeR(recs[i].tempIndex) ;
			CompileHeadRecordArgs(StructArgs(st), StructArity(st)) ;
		}
}

static void CompileHeadArgs(Hdl args, int nArgs)
{
	register Pt t ;
	
	dotimes(currArgN, nArgs)
	{
		t = args[currArgN] ;
		
		if( IsNilAtom(t) )
		{
			AddInst1(GetNil, OutTemp(currArgN)) ;
		}

		elif( IsAtomic(t) )
		{
			AddInst2(GetConstant, t, OutTemp(currArgN)) ;
		}
			
		elif( IsVar(t) ) CompileHeadVar(t, currArgN) ;
			
		elif( IsList(t) )
		{
			AddInst1(GetList, OutTemp(currArgN)) ;
			CompileHeadRecordArgs(XListArgs(t), 2) ;
		}
		
		elif( IsStruct(t) )
		{
			AddInst2(GetStructure, XStructFunctor(t), OutTemp(currArgN)) ;
			CompileHeadRecordArgs(XStructArgs(t), XStructArity(t)) ;
		}
		
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

	elif( IsVar(head) ) Error("Clause head is a variable") ;

	else InternalError("CompileHead") ;
	
	if( nGoals == 1 ) AddInst0(Proceed) ;
}



/* COMPILER MAIN */

static void DivideClause(Pt clause, Pt *head, Pt *body, Pt *fGoal, Pt *rGoals)
{
	SplitClauseTerm(clause, head, body) ;
	if( IsThisStruct(*body, commaFunctor) )
	{
		*fGoal = XStructArg(*body,0) ;
		*rGoals = XStructArg(*body,1) ;
	}
	else
	{
		*fGoal = *body ;
		*rGoals = nil ;
	}
}

void Compiler(Pt clause, Bool end, Hdl *cd, long *size)
{
	Pt head, body, firstGoal, remainderGoals ;

	DivideClause(clause, &head, &body, &firstGoal, &remainderGoals) ;
	BuildVarDic(head, body) ;
	hasEnvironment = nGoals > 2 || body == tCutAtom ;

	ResetCode() ;
	AddInst2(TryMeElse, &PFailAddr, TermArity(head)) ;
	InitHeadAndFirstGoalTempVars(head, firstGoal) ;

#if DEBUG == 1
	WritelnTerm(clause) ;
	ListVarDic() ;
#endif

	if( hasEnvironment ) AddInst0(IAllocate) ;
	CompileHead(currGoal = head) ;	
	if( nGoals > 1 )
	{
		currGoalN = 1 ;
		CompileGoal(currGoal = firstGoal) ;
		currGoalN++ ;

		if( remainderGoals != nil )
		{
			while( IsThisStruct(remainderGoals, commaFunctor) )
			{
				currGoal = XStructArg(remainderGoals,0) ;
				remainderGoals = XStructArg(remainderGoals,1) ;
				InitGoalTempVars(currGoal) ;
				CompileGoal(currGoal) ;
				currGoalN++ ;
			}
			InitGoalTempVars(currGoal = remainderGoals) ;
			CompileGoal(remainderGoals) ;
			currGoalN++ ;
		}
	}
	*cd = code ;
	*size = CodeSize() ;

#if DEBUG == 1
	ListCode(code) ;
	exit(0) ;
#endif
}
