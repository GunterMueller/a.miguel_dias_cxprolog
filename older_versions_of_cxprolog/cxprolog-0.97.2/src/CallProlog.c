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

static jmp_buf *currJB = nil ;
static int shrinkLevel ;
static long threadId = 0 ;
int callLevel = 0 ;
Bool hasATopLevel = false ;

static CharPt PrologEventAsStr(PrologEvent ev)
{
	switch( ev ) {
		case peNone: return "peNone" ;
		case peSucc: return "peSucc" ;
		case peFailure: return "peFailure" ;
		case peException: return "peException" ;
		case peExit: return "peExit" ;
		case peHalt: return "peHalt" ;
		case peRestart: return "peRestart" ;
		case peFatalError: return "peFatalError" ;
		case peOtherError: return "peOtherError" ;
		case peContinue: return "peContinue" ;
		case peForceFail: return "peForceFail" ;
		case peSetSuccess: return "peSetSuccess" ;
		case peReturn: return "peReturn" ;
		case peShrink: return "peShrink" ;
		default: return "Unknown PrologEvent" ;
	}
}

static void RestartAll(void)
{
	CutAll() ;
	ScratchRestart() ;
	DebugRestart() ;
	FlagsRestart() ;
	InterLineRestart() ;
	ConsultRestart() ;
	ClauseGC(true) ;
	ActiveThreadReset() ;
}
	
static void EmergencyExit(void)
{
	Mesg("Emergency exit") ;
	StopProlog() ;
	exit(0) ;
}

Bool InMainThread(Bool warn)
{
	if( OSGetTid() == threadId )
		return true ;
#if COMPASS
	RefreshProlog() ;
	if( OSGetTid() == threadId )
		return true ;	
#endif
	if( warn )
		Warning("Ignored illegal reentrant call from thread %ld. Main thread is %ld",
					threadId, OSGetTid()) ;
	return false ;
}

/* If this instance of CxProlog resulted form a fork you
   may need to call this right after the fork */

void RefreshProlog()
{
	static Bool done = false ;	/* only once */
	if( !done ) {
		threadId = OSGetTid() ;
		done = true ;
	}
}

void SendPrologEvent(PrologEvent ev) {
	if( currJB == nil )
		EmergencyExit() ;
	longjmp(*currJB, ev) ;
}

void EventContinue()	{ SendPrologEvent(peContinue) ; }
void EventForceFail()	{ SendPrologEvent(peForceFail) ; }
void EventRestart()		{ SendPrologEvent(peRestart) ; }
void EventExit()		{ SendPrologEvent(peExit) ; }
void EventHalt()		{ SendPrologEvent(peHalt) ; }
void EventFatalError()	{ SendPrologEvent(peFatalError) ; }
void EventShrink(int l) { shrinkLevel = l ; SendPrologEvent(peShrink) ; }

/* CallProlog is a reentrant function that allows C code to invoke Prolog code */
PrologEvent CallProlog(Pt goal, Pt *res)
{
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	Hdl saveP = P ;
	Hdl saveCP = CP ;
	Pt saveX0 = X0 ;
/* Choice point of $$_call_prolog/1 */
	ChoicePointPt theB = cChoicePointPt(TopOfLocalStack() - 1) - 1 ;
	PrologEvent ev ;
	LvrPush(&goal) ;	/* Register variable for relocation */
	LvrPush(&saveX0) ;
	LvrPush(&theB) ;
	volatile PrologEvent evRes = peFailure ; /* Must be volatile */
	if( callLevel > 0 )
		Info(3, "Reentrant Prolog call, level %d", callLevel + 1) ;
	if( !InMainThread(true) )
		return peOtherError ;
	callLevel++ ;
	currJB = &newJB ;
	switch( (ev = setjmp(*currJB)) ) {
		case peNone: {	/* Startup */
			X0 = goal ;
			P = PredCode(CheckPredicateByName("$$_call_prolog", 1)) ;
			MachineRun() ;
			break ;
		}
		case peContinue: {
			MachineRun() ;
			break ;
		}
		case peForceFail: {
			P = Bf(P) ;
			MachineRun() ;
			break ;
		}
		case peSetSuccess: {
			evRes = peSucc ;
			MachineRun() ;
			break ;
		}
		case peRestart:
		case peExit:
		case peHalt:
		case peFatalError: {
			CutTo(theB) ;
			evRes = ev ;
			P = Bf(P) ;
			MachineRun() ;
			break ;
		}
		case peReturn: {
			currJB = saveJB ;
			callLevel-- ;
			LvrPop(&theB) ;
			LvrPop(&saveX0) ;
			LvrPop(&goal) ;
			P = saveP ;
			CP = saveCP ;
			X0 = saveX0 ;
			if( callLevel > 0 )
				Info(3, "Exit reentrant Prolog call, level %d", callLevel + 1) ;
			if( res != nil )
				*res = goal ;
			return evRes ;
		}
		case peShrink: {	/* Shrink C runtime stack. Only used by throw/1. */
			if( callLevel <= shrinkLevel ) {
				LvrAdjust(&theB) ;	/* Adjust lvr-stack */
				MachineRun() ;
			}
			else {
				currJB = saveJB ;
				callLevel-- ;
				SendPrologEvent(peShrink) ; /* Pop C frame */
			}
			break ;
		}
		default:
			break ;
	}
	FatalError("CallProlog") ;
	return peOtherError ;
}

void CallPrologV(Pt goal) /* Return void */
{
	CallProlog(goal, nil) ;
}

PrologEvent CallFunThruProlog(Fun fun)
{
	PrologEvent ev ;
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	currJB = &newJB ;
	if( (ev = setjmp(*currJB)) == peNone )
		fun() ;
	currJB = saveJB ;
	return ev ;
}

PrologEvent CallPrologStr(CharPt goal)
{
	Pt t = nil ;
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;	
	currJB = &newJB ;
	if( setjmp(*currJB) == peNone )
		t = ZTermFromStr(goal) ;
	currJB = saveJB ;
	return t == nil ? peOtherError : CallProlog(t, nil) ;
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

PrologEvent StartProlog(int argc, char **argv)
{
	static Bool started = false ;	/* only once */
	PrologEvent res = peOtherError ;
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;	
	if( started )
		FatalError("Cannot activate two CxProlog instances in the same process") ;
	currJB = &newJB ;
	if( setjmp(*currJB) == peNone ) {
		threadId = OSGetTid() ;
		SetVersion("CxProlog", "0.97", "2", nil, CONTEXTS) ;
		Boot(argc, (CharPt *)argv) ;
		res = CallPrologStr("'$$_lux0'") ;
	}
	currJB = saveJB ;
#if 0
		Mesg("RES0 = %s", PrologEventAsStr(res)) ;
#endif
	return res ;
}

void StopProlog()
{
	ProcessesFinish() ;
	StreamsSane() ;
	InterLineFinish();
	GoHome(false) ; /* for DJGPP */
}

int RunInteractiveProlog(int argc, char **argv)
{
	PrologEvent res = StartProlog(argc, argv) ;
	while( res == peSucc || res == peRestart ) {
		RestartAll() ;
		res = CallPrologStr("'$$_lux1'") ;
#if 0
		Mesg("RES1 = %s", PrologEventAsStr(res)) ;
#endif
	}
	if( res == peHalt )
		BasicInfo("CxProlog halted") ;
	StopProlog() ;
	return res == peFatalError || res == peOtherError ? -1 : 0 ;
}		

void AtTopLevel()
{
	if( !HasATopLevel() )
		HasATopLevel() = true ;
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
static void PCopyright()
{
	ShowVersion() ;
	Write("Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL\n") ;
	Write("CxProlog is free software and is distributed in the hope that it\n") ;
	Write("will be useful, but WITHOUT ANY WARRANTY; without even the implied\n") ;
	Write("warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n") ;
	Write("See the GNU General Public License for more details.\n") ;
	JumpNext() ;
}

static void PSetSuccess()
{
	SendPrologEvent(peSetSuccess) ;
	JumpNext() ;
}

static void PReturn()
{
	SendPrologEvent(peReturn) ;
	JumpNext() ;
}

static void PLevel()
{
	Mesg("%d", callLevel) ;
	JumpNext() ;
}

static void PCallPrologThroughC()	/* For testing reentrant calls */
{
	MustBe( CallProlog(X0, nil) == peSucc ) ;
}

static void PFatalError()	/* For testing CallProlog */
{
	FatalError("%s", XTestAtomName(X0)) ;
	JumpNext() ;
}

static void test()
{
	ZTermFromStr("o la") ;
}

static void CallPrologTestAux(Bool isStr, VoidPt arg)
{
	WriteStd("B=%lx E=%lx H=%lx\n", B, E, H) ;
	if( isStr ) {
		WriteStd("GOAL= %s\n", arg) ;
		WriteStd("RES = %s\n\n", PrologEventAsStr(CallPrologStr(arg))) ;
	}
	else {	
		WriteStd("GOAL= CallFunThruProlog\n") ;
		WriteStd("RES = %s\n\n", PrologEventAsStr(CallFunThruProlog(arg))) ;
	}
}
static void PCallPrologTest()	/* For testing CallProlog */
{
	CallPrologTestAux(true, "X = ola, writeln(X)") ;
	CallPrologTestAux(true, "fail") ;
	CallPrologTestAux(true, "throw(exc(1))") ;
	CallPrologTestAux(true, "exit") ;
	CallPrologTestAux(true, "halt") ;
	CallPrologTestAux(true, "restart") ;
	CallPrologTestAux(true, "'$$_fatal_error'('fatal error')") ;
	CallPrologTestAux(true, "e r r o r") ;
	CallPrologTestAux(false, test) ;
	CallPrologTestAux(true, "repeat, fail") ;
	CallPrologTestAux(true, "true") ;
	JumpNext() ;
}

void CallPrologInit()
{
	InstallCBuiltinPred("abort", 0, PRestart) ;
	InstallCBuiltinPred("restart", 0, PRestart) ;
	InstallCBuiltinPred("exit", 0, PExit) ;
	InstallCBuiltinPred("halt", 0, PHalt) ;
	InstallCBuiltinPred("copyright", 0, PCopyright) ;
	InstallCBuiltinPred("$$_set_success", 0, PSetSuccess) ;
	InstallCBuiltinPred("$$_return", 0, PReturn) ;
	InstallCBuiltinPred("$$_level", 0, PLevel) ;
	InstallCBuiltinPred("$$_call_prolog_through_c", 1, PCallPrologThroughC) ;
	InstallCBuiltinPred("$$_fatal_error", 1, PFatalError) ;
	InstallCBuiltinPred("$$_call_prolog_test", 0, PCallPrologTest) ;
	InstallCBuiltinPred("$$_cpt", 0, PCallPrologTest) ;
}
