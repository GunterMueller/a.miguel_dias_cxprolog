/*
 *   This file is part of the CxProlog system

 *   CallProlog.c
 *   by A.Miguel Dias - 2003/06/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL

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
#include <setjmp.h>

#define PE_Continue		1
#define PE_ForceFail	2
#define PE_Restart		3
#define PE_SetSuccess	4
#define PE_Return		5
#define PE_Exit			6
#define PE_Shrink		7

static jmp_buf mainJB ;
static jmp_buf *currJB = nil ;
static int shrinkLevel ;
static Bool callThruProlog = false, halted = false ;
static long threadId = 0 ;
int callLevel = 0 ;
		
static void CleanUp(void)
{
	ProcessesFinish() ;
	StreamsSane() ;
	GoHome(false) ; /* for DJGPP */
}

Bool InMainThread(Bool warn)
{
	if( OSGetTid() == threadId )
		return true ;
	else {
		if( warn )
			Warning("Ignored illegal reentrant call from thread %ld. Main thread is %ld",
					threadId, OSGetTid()) ;
		return false ;
	}
}

void PrologEvent(int ev)
{
	switch( ev ) {
		case PE_Restart:
			if( callThruProlog )
				break ;
			elif( Booting() ) {
				EventHalt() ;
				return ;
			}
			else
				break ;
		case PE_Exit:
			if( callThruProlog )
				break ;
			elif( currJB == nil ) {
				longjmp(mainJB, -1) ;
				return ;
			}
			elif( !InMainThread(true) ) { /* Stop now */
				CleanUp() ;
				exit(0) ;
				return ;
			}
			else
				break ;
		}
	longjmp(*currJB, ev) ;	
}

void EventContinue()	{ PrologEvent(PE_Continue) ; }
void EventForceFail()	{ PrologEvent(PE_ForceFail) ; }
void EventRestart()		{ PrologEvent(PE_Restart) ; }
void EventExit()		{ PrologEvent(PE_Exit) ; }
void EventHalt()		{ halted = true ; PrologEvent(PE_Exit) ; }
void EventShrink(int l) { shrinkLevel = l ; PrologEvent(PE_Shrink) ; }

/* CallProlog is a reentrant function that allows C code to invoke Prolog code.
   The function returns true if the goal succeeds and returns false otherwise */
Bool CallProlog(Pt goal)
{
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	Hdl saveP = P ;
	Hdl saveCP = CP ;
	Pt saveX0 = X0 ;
	volatile Bool result = false ; /* must be volatile */
	if( callLevel > 0 )
		Info(3, "Reentrant Prolog call, level %d", callLevel + 1) ;
	if( !InMainThread(true) )
		return false ;
	currJB = &newJB ;
	callLevel++ ;
	switch( setjmp(*currJB) ) {
		case 0: {	/* startup */
			X0 = goal ;
			P = PredCode(CheckPredicateByName("$$_call_prolog", 1)) ;
			MachineRun() ;
			break ;
		}
		case PE_Continue: {
			MachineRun() ;
			break ;
		}
		case PE_ForceFail: {
			ScratchRestart() ;
			P = Bf(P) ;
			MachineRun() ;
			break ;
		}
		case PE_Restart: {
			if( saveJB == nil ) { /* at first level? */
				ScratchRestart() ;
				DebugRestart() ;
				FlagsRestart() ;
				InterLineRestart() ;
				ConsultRestart() ;
				ClauseGC(true) ;
				ActiveThreadRestart() ;
			}
			else {
				currJB = saveJB ;
				callLevel-- ;
				EventRestart() ; /* pop C frame */
			}
			break ;
		}
		case PE_SetSuccess: {
			result = true ;
			MachineRun() ;
			break ;
		}
		case PE_Return: {
			P = saveP ;
			CP = saveCP ;
			X0 = saveX0 ;
			currJB = saveJB ;
			callLevel-- ;
			return result ;
		}
		case PE_Exit: {
			if( saveJB == nil ) { /* at first level? */
				CleanUp() ;
				return false ;
			}
			else {
				currJB = saveJB ;
				callLevel-- ;
				PrologEvent(PE_Exit) ; /* pop C frame */
			}
			break ;
		}
		case PE_Shrink: {	/* shrink C runtime stack. Only used by throw/1. */
			if( callLevel <= shrinkLevel )
				MachineRun() ;
			else {
				currJB = saveJB ;
				callLevel-- ;
				PrologEvent(PE_Shrink) ; /* pop C frame */
			}
			break ;
		}
		default: InternalError("CallProlog") ;
	}
	FatalError("CallProlog") ;																		
	return false ;
}

int CallFunThruProlog(Fun fun)
{
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	int res ;
	currJB = &newJB ;
	if( (res = setjmp(*currJB)) == 0 ) {
		callThruProlog = true ;
		fun() ;
	}
	callThruProlog = false ;
	currJB = saveJB ;
	return res ;
}

Bool CallPrologStr(CharPt goal)
{
	Pt t = ZTermFromStr(goal) ;
	return t != nil && CallProlog(t) ;
}

Bool CallPrologLinear(CharPt fmt, ...)
{
#if 0
	va_list v ;
	va_start(v, fmt) ;
	while( *fmt )
		switch(*fmt++ ) {
			case '.':
				if( !EventQueueMakeSpace(1) ) goto abort ;
				*qLast++ = '.' ;
				break ;
			case 'i':
				if( !EventQueueMakeSpace(1 + sizeof(int)) ) goto abort ;
				*qLast++ = 'i' ;
				*(int *)qLast = va_arg(v, int) ;
				qLast += sizeof(int) ;
				break ;
			case 'f':
				if( !EventQueueMakeSpace(1 + sizeof(double)) ) goto abort ;
				*qLast++ = 'f' ;
				*(double *)qLast = va_arg(v, double) ;
				qLast += sizeof(double) ;
				break ;
			case 't':
				if( !EventQueueMakeSpace(1 + sizeof(Pt)) ) goto abort ;
				*qLast++ = 't' ;
				*(Pt *)qLast = va_arg(v, Pt) ;
				qLast += sizeof(Pt) ;
				break ;
			case 'o':
				if( !EventQueueMakeSpace(1 + sizeof(VoidPt)) ) goto abort ;
				*qLast++ = 'o' ;
				*(VoidPt *)qLast = va_arg(v, VoidPt) ;
				qLast += sizeof(VoidPt) ;
				break ;
			case 's': {
				CharPt s = va_arg(v, CharPt) ;
				int len = strlen(s) + 1 ;
				if( !EventQueueMakeSpace(1 + len) ) goto abort ;
				*qLast++ = 's' ;
				strcpy(qLast, s) ;
				qLast += len ;
				break ;
			}
			default:
				goto abort ;
				break ;
		}

	if( !EventQueueMakeSpace(1) ) goto abort ;
	*qLast++ = 'z' ;
    va_end(v) ;
	nEvents++ ;
	if( nEvents == 1 ) {
		qCondition.Signal() ;
		if( eventNotifier != nil )
			eventNotifier() ;
	}
	return ;
abort:
    va_end(v) ;

	
	Pt elem, t ;
	FunctorPt f ;
	ScratchSave() ;
	elem = nil ;
	for(;;) {
		elem = EventQueueGetItem() ;
		{
			int a = ScratchCurr() - ScratchStart() - 1 ;
			t = *ScratchStart() ;
			if( a > 0 ) {
				if( !IsAtom(t) ) {
					EventQueueReset() ;
					Error("Invalid wxWidgets linearized tree") ;
				}
				f = LookupFunctor(XAtom(t), a) ;
				t = MakeStruct(f, ScratchStart() + 1) ;
			}
			FreeScratch() ;
			if( ScratchDistToSaved() == 0 ) break ;
			else ScratchPush(t) ;	
		}
		else {
			UseScratch() ;
			ScratchPush(elem) ;
		}
	}
	nEvents-- ;
	return t ;
#endif
	return false ;
}

int RunCxProlog(int argc, char **argv)
{
	threadId = OSGetTid() ;
	switch( setjmp(mainJB) ) {
		case 0:
			if( callLevel > 0 )
				FatalError("Cannot activate two CxProlog threads in the same process") ;
			SetVersion("CxProlog", "0.97", "0", nil, CONTEXTS) ;
			Boot(argc, (CharPt *)argv) ;
			ThreadRootStart() ;
			if( halted )
				BasicInfo("CxProlog halted") ;
			return 0 ;
		default:
			return -1 ;
	}
}



/* CXPROLOG C'BUILTINS */

static void PRestart()
{
	EventRestart() ;
	JumpNext() ;
}

static void PExit()
{
	EventExit() ;
	JumpNext() ;
}

static void PHalt()
{
	EventHalt() ;
	JumpNext() ;
}

static void PSetSuccess()
{
	PrologEvent(PE_SetSuccess) ;
	JumpNext() ;
}

static void PReturn()
{
	PrologEvent(PE_Return) ;
	JumpNext() ;
}

static void PLevel()
{
	Mesg("%d", callLevel) ;
	JumpNext() ;
}

static void PCallPrologThroughC()	/* For testing reentrant calls */
{
	MustBe( CallProlog(X0) ) ;
}

void CallPrologInit()
{
	InstallCBuiltinPred("abort", 0, PRestart) ;
	InstallCBuiltinPred("restart", 0, PRestart) ;
	InstallCBuiltinPred("exit", 0, PExit) ;
	InstallCBuiltinPred("halt", 0, PHalt) ;
	InstallCBuiltinPred("$$_set_success", 0, PSetSuccess) ;
	InstallCBuiltinPred("$$_return", 0, PReturn) ;
	InstallCBuiltinPred("$$_level", 0, PLevel) ;
	InstallCBuiltinPred("$$_call_prolog_through_c", 1, PCallPrologThroughC) ;
}
