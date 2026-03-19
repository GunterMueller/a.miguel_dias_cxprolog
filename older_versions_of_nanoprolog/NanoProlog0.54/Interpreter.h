/*
 *   This file is part of the NanoProlog system

 *   Interpreter.h
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

#ifndef _Interpreter_
#define _Interpreter_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif


/* FRAME DEFINITION E ACCESS */

typedef struct Environment
{
/*	Pt Y[] ;	*/
	struct Environment *E ;
	Hdl CP ;
	struct ChoicePoint* B ;
} Environment, *EnvironmentPt ;
	
typedef struct ChoicePoint
{
	struct Environment *E ;
	Hdl CP ;
	struct ChoicePoint *B, *B0 ;
	Hdl P, TR, H ;
/*	Pt A[] ;	*/
} ChoicePoint, *ChoicePointPt ;
	
#define cEnvironmentPt(x)	((EnvironmentPt)(x))
#define cChoicePointPt(x)	((ChoicePointPt)(x))

#define X(pt)				( *(pt) )
#define Xc(c)				( X[c] )
#define Ef(field)			( E[-1].field )
#define Bf(field)			( B->field )
#define A(idx)				( cHdl(B + 1)[idx] )
#define Y(idx)				( cHdl(E)[idx] )

#define IsToTrailVar(v)		( LTVar(B,v) || LTVar(v,HB) )
#define IsGlobalVar(v)		LTVar(v, H)
#define IsLocalVar(v)		LEVar(H, v)
#define IsCurrEnvVar(v)		( LTVar(v, E) && IsLocalVar(v) )

#define Assign(v, term)		if( IsToTrailVar(v) )					\
							{										\
								TestTrailOverflow()					\
								Push(TR, cPt(v)) ;					\
								SetVar(v, term) ;					\
							}										\
							else SetVar(v, term)

#define Bind(v1,v2)			if( LTVar(v1, v2) )							\
								if( IsGlobalVar(v1) ) Assign(v2, v1) ;	\
								else Assign(v1, v2) ;					\
							else										\
								if( IsGlobalVar(v2) ) Assign(v1, v2) ;	\
								else Assign(v2, v1)

#define RestoreTrail(base,v)								\
							while( TR != (base) )			\
							{								\
								v = cVar(Pop(TR)) ;			\
								ResetVar(*v) ;				\
							}

#define DoFail()		{ P = Bf(P) ; JumpNext() }

#define maxX	256

extern Hdl P, CP, H, HB, TR ;
extern EnvironmentPt E ;
extern ChoicePointPt B ;
extern Pt X[] ;
extern Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
extern Bool trace, hideTrace ;
extern Pt PFailAddr ;

typedef struct PrologHashElem
{
	Pt value ;
	Hdl address ;
	struct PrologHashElem* next ;
} PrologHashElem, *PrologHashTable ;

#define PrologHash(t, size)		( (cWord(t) >> 4) & ((size) - 1) )

extern Pt
	Nop, Proceed, IAllocate, Call, CallVar,
	Execute, ExecuteVar, Deallocate,
	Cut, PutCutLevel, Fail,
	GetYVariable, GetXValue, GetYValue, GetConstant, GetNil,
	GetStructure, GetList, PutXVariable, PutXVariableOne, PutYVariable,
	PutXValue, PutYValue, PutUnsafeValue, PutConstant, PutNil, PutStructure,
	PutList, UnifyVoid, UnifyVoidOne, UnifyXVariable, UnifyYVariable,
	UnifyXLocalValue, UnifyYLocalValue, UnifyXValue, UnifyYValue,
	UnifyConstant, UnifyNil, BuildVoid, BuildVoidOne, BuildXVariable,
	BuildYVariable, BuildXValue, BuildYValue, BuildXLocalValue,
	BuildYLocalValue, BuildConstant, BuildNil, TryMeElse, RetryMeElse,
	TrustMe, Try, Retry, Trust, SwitchOnTerm, DiscardAndFail,
	SwitchOnConstant, SwitchOnStructure, MakeIndex ;

void InitInsts(void) ;
Bool GetInstInfo(Pt inst, char **name, char **types) ;
void RunInterpreter(void) ;

#endif
