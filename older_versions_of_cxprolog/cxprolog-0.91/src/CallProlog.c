/*
 *   This file is part of the CxProlog system

 *   CallProlog.c
 *   by A.Miguel Dias - 2003/06/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

static jmp_buf *currJB = nil ;
static int shrinkLevel ;
int callLevel = 0 ;

static void Stop(CharPt mesg)
{
	if( currJB != nil  )
		ProcessesFinish() ;
	StreamsSane() ;
	if( infoMessages_flag )
		WriteStd(mesg) ;
	exit(0) ;
}

void EventContinue()	{ longjmp(*currJB, 1) ; }
void EventForceFail()	{ longjmp(*currJB, 2) ; }
void EventRestart()		{ if( Booting() ) EventHalt() ; longjmp(*currJB, 3) ; }
void EventSetSuccess()	{ longjmp(*currJB, 4) ; }
void EventReturn()		{ longjmp(*currJB, 5) ; }
void EventShrink(int toLevel) { shrinkLevel = toLevel ; longjmp(*currJB, 6) ; }
void EventExit()		{ Stop("") ; }
void EventHalt()		{ Stop("%% CxProlog halted.\n") ; }

/* CallProlog is a reentrant function that allows C code to invoke Prolog code.
   The function returns true if the goal succeeds and returns false otherwise */
Bool CallProlog(Pt goal)
{
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	Hdl saveP = P ;
	Hdl saveCP = CP ;
	volatile Bool result = false ; /* must be volatile */
	currJB = &newJB ;
	callLevel++ ;
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
			if( saveJB == nil ) { /* at first level ? */
				ScratchRestart() ;
				DebugRestart() ;
				FlagsRestart() ;
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
			currJB = saveJB ;
			callLevel-- ;
			return result ;
		}
		case 6: {	/* shrink C runtime stack. Only used by throw/1. */
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

int RunCxProlog(int argc, char **argv)
{
	SetVersion("CxProlog", "0.91", "", "") ;
	Boot(argc, (CharPt *)argv) ;
	ThreadRootStart() ;
	FatalError("RunCxProlog") ;
	return 0 ;
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

static void PZZTest()
{
	CallProlog(MakeAtom("a")) ;
	JumpNext() ;
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
	InstallCBuiltinPred("zztest", 0, PZZTest) ;
}
