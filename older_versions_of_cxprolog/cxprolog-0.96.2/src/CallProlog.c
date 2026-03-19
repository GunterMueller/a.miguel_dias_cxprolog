/*
 *   This file is part of the CxProlog system

 *   CallProlog.c
 *   by A.Miguel Dias - 2003/06/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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

static jmp_buf mainJB ;
static jmp_buf *currJB = nil ;
static int shrinkLevel ;
static Bool halted = false ;
int callLevel = 0 ;
Bool restrictedMode = false ;

static void CleanUp(void)
{
	ProcessesFinish() ;
	StreamsSane() ;
	GoHome(false) ; /* for DJGPP */
}

static void StopNow(void)
{
	CleanUp() ;
	exit(0) ;
}

void EventContinue()	{ longjmp(*currJB, 1) ; }
void EventForceFail()	{ longjmp(*currJB, 2) ; }
void EventRestart()	{ if( Booting() ) EventHalt() ; longjmp(*currJB, 3) ; }
void EventSetSuccess()	{ longjmp(*currJB, 4) ; }
void EventReturn()		{ longjmp(*currJB, 5) ; }
void EventExit()		{ longjmp(*currJB, 6) ; }
void EventHalt()		{ if( restrictedMode ) StopNow() ;
						  elif( currJB == nil ) longjmp(mainJB, -1) ;
						  else { halted = true ; longjmp(*currJB, 6) ; } }
void EventShrink(int l) { shrinkLevel = l ; longjmp(*currJB, 7) ; }

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
	if( restrictedMode )
		FatalError("Forbidden reentrant prolog call") ;
	currJB = &newJB ;
	callLevel++ ;
#if 0
	Mesg("CallProlog %d", callLevel) ;
#endif
	switch( setjmp(*currJB) ) {
		case 0: {	/* startup */
			X0 = goal ;
			P = PredCode(CheckPredicateByName("$$_call_prolog", 1)) ;
			MachineRun() ;
			break ;
		}
		case 1: {	/* continue */
			MachineRun() ;
			break ;
		}
		case 2: {	/* force fail */
			ScratchRestart() ;
			P = Bf(P) ;
			MachineRun() ;
			break ;
		}
		case 3: {	/* restart current thread */
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
		case 4: {	/* set success */
			result = true ;
			MachineRun() ;
			break ;
		}
		case 5: {	/* return */
			P = saveP ;
			CP = saveCP ;
			X0 = saveX0 ;
			currJB = saveJB ;
			callLevel-- ;
			return result ;
		}
		case 6: {	/* exit/halt */
			if( saveJB == nil ) { /* at first level? */
				CleanUp() ;
				return false ;
			}
			else {
				currJB = saveJB ;
				callLevel-- ;
				EventExit() ; /* pop C frame */
			}
			break ;
		}
		case 7: {	/* shrink C runtime stack. Only used by throw/1. */
			if( callLevel <= shrinkLevel )
				MachineRun() ;
			else {
				currJB = saveJB ;
				callLevel-- ;
				EventShrink(shrinkLevel) ; /* pop C frame */
			}
			break ;
		}
		default: InternalError("CallProlog") ;
	}
	FatalError("CallProlog") ;																		
	return false ;
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
	switch( setjmp(mainJB) ) {
		case 0:
			if( callLevel > 0 )
				FatalError("Cannot activate two CxProlog threads in the same process") ;
			SetVersion("CxProlog", "0.96", "2", nil, CONTEXTS) ;
			Boot(argc, (CharPt *)argv) ;
			ThreadRootStart() ;
			if( halted && infoMessages_flag )
				WriteStd("%% CxProlog halted.\n") ;
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
	EventSetSuccess() ;
	JumpNext() ;
}

static void PReturn()
{
	EventReturn() ;
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
