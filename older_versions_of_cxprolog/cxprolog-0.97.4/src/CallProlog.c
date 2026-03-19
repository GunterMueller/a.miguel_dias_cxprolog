/*
 *   This file is part of the CxProlog system

 *   CallProlog.c
 *   by A.Miguel Dias - 2003/06/20
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2009 A.Miguel Dias, CITI, DI/FCT/UNL

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

/*
Possible outcomes of the functions CallProlog, CallPrologMulti, CallPrologMultiNext, CallPrologMultiStop.

PrologEvent = 
	peSucc			->	. the goal has succeed
						. a result becomes available if there was one setup by set_result/1
						. extra instantiation of the goal becomes available only in the "multi" version
	peFailure		->	. the goal has failed
						. there is no result, even if there is one setup by set_result/1
						. no extra instantiation of the goal becomes available
	peException		->	. an uncaught exception has occurred
						. no result and no extra instantiation, like in peFailure
						. a subsequent call to WriteException would prints the
							usual "uncaught exception" message
	peInterrupt		->  . the user has issued a Ctrl-C interrupt.
						. no result and no extra instantiation, like in peFailure
	peAbort			->  . the predicate abort/0 has been called, or the user issued a Ctrl-C interrupt.
						. no result and no extra instantiation, like in peFailure
	peExit			->  . the predicate exit/0 has been called
						. no result and no extra instantiation, like in peFailure
	peHalt			->	. the predicate halt/0 has been called.
						. no result and no extra instantiation, like in peFailure
	peFatalError	->  . there was an internal fatal error.
						. no result and no extra instantiation, like in peFailure
	peOtherError	->  . call from outside the CxProlog thread in CallProlog/CallPrologMulti
						. call of CallPrologMultiNext without a previous CallPrologMulti
						. call of CallPrologMultiStop without a previous CallPrologMulti
						. call of CallPrologStr with an invalid term-string
} PrologEvent ;
*/

#include "CxProlog.h"
#include <setjmp.h>

#define DEBUG	0

static jmp_buf *currJB = nil ;
static long threadId = 0 ;
static Bool ignoreEmergencyExit = false ;
static Pt currentResult ;

int callLevel = 0 ;
Bool hasATopLevel = false ;
Bool3 normalFinish = undefined3 ;

CharPt PrologEventAsStr(PrologEvent ev)
{
	switch( ev ) {
		case peNone: return "peNone" ;
		case peSucc: return "peSucc" ;
		case peFailure: return "peFailure" ;
		case peException: return "peException" ;
		case peInterrupt: return "peInterrupt" ;
		case peAbort: return "peAbort" ;
		case peExit: return "peExit" ;
		case peHalt: return "peHalt" ;
		case peFatalError: return "peFatalError" ;
		case peOtherError: return "peOtherError" ;
		case peContinue: return "peContinue" ;
		case peForceFail: return "peForceFail" ;
		case peMoreSuccess: return "peMoreSuccess" ;
		case peReturnSuccess: return "peReturnSuccess" ;
		case peReturnFailure: return "peReturnFailure" ;
		default: return "Unknown PrologEvent" ;
	}
}

PrologEvent CallFunThruProlog(Fun fun)
{		 //Internally called at OnGuiApp::GuiCall()
	PrologEvent ev ;
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	currJB = &newJB ;
	if( (ev = setjmp(*currJB)) == peNone )
		fun() ;
	currJB = saveJB ;
	return ev ;
}

static Pt ZTermFromStrThruProlog(Str goal) {
	volatile Pt t = nil ;
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	currJB = &newJB ;
	if( setjmp(*currJB) == peNone )
		t = ZTermFromStr(goal) ;
	currJB = saveJB ;
	return t ;
}

static void RestartAll(void)
{
	CutAll() ;
	ScratchRestart() ;
	DebugRestart() ;
	FlagsRestart() ;
	InterLineRestart() ;
	ConsultRestart() ;
	LvrRestart() ;
	ClauseGC(true) ;
	ActiveThreadReset() ;
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

static void ExitProlog(void)
{
	static Bool done = false ; 
	if( !done ) {
		done = true ;
		ProcessesFinish() ;
		StreamsSane() ;
		InterLineFinish() ;
		DeleteConsole() ;
		GoHome(false) ; /* for DJGPP */
	}
}

void SendPrologEvent(PrologEvent ev)
{
	if( currJB == nil ) {
		if( !ignoreEmergencyExit ) {
			if( ev == peException )
				WriteException() ;
			Mesg("Emergency exit: No event handler for '%s'",
				PrologEventAsStr(ev)) ;
			exit(0) ;
		}
	}
	else
		longjmp(*currJB, ev) ;
}

void EventContinue()	{ SendPrologEvent(peContinue) ; }
void EventForceFail()	{ SendPrologEvent(peForceFail) ; }
void EventException()	{ SendPrologEvent(peException) ; }
void EventInterrupt()	{ SendPrologEvent(peInterrupt) ; }
void EventAbort()		{ SendPrologEvent(peAbort) ; }
void EventExit()		{ SendPrologEvent(peExit) ; }
void EventHalt()		{ SendPrologEvent(peHalt) ; }
void EventFatalError()	{ SendPrologEvent(peFatalError) ; }

static Pt GatherVarsPrepare(Pt t)
{
	Pt v = MakeUnStruct(LookupFunctorByName("$vars", 1), VarNames()) ;
	Pt a = MakeUnStruct(LookupFunctorByName("assert", 1), v) ;
	AbolishPredicate(LookupPredicateByName("$vars", 1), true) ;
	return MakeBinStruct(commaFunctor, t, a) ;
}

static CharPt GatherVars(void)
{
	Pt v = ClauseSource(PredClauses(LookupPredicateByName("$vars", 1))) ;
	Pt t = XStructArg(v, 0) ;
	BigStr2Open() ;	
#if 1
	for( t = Drf(t) ; IsList(t) ; t = Drf(XListTail(t)) ) {
		Hdl args ;
		XTestStruct(XListHead(t), &args) ;
		BigStr2AddStr(SubtermAsStrN(args[0], v)) ;
		BigStr2AddStr(" = ") ;
		BigStr2AddStr(SubtermAsStrN(args[1], v)) ;
		BigStr2AddStr("\n") ;
	}
#endif
	AbolishPredicate(LookupPredicateByName("$vars", 1), true) ;
	return BigStr2Close() ;
}

static int LevelPointCount(void)
{
	ChoicePointPt b ;
	int n = 0 ;
	for( b = L ; !IsEndOfChain(b) ; b = cChoicePointPt(ChoicePointArg(b, 5)) )
		n++ ;
	return n ;
}

#define LevelPointSize		6

static void LevelPointPush(Hdl saveH)
{
	if( DEBUG || callLevel > 0 )
		Info(4, "Reentrant Prolog call, level %d", callLevel) ;
	X3 = tNoResult ;				/* Result */
	X4 = cPt(P) ;					/* Save P */
	X5 = cPt(L) ;					/* Save L */
	CreateChoicePoint(PredCode(CheckPredicateByName("$$_return_failure", 0)), LevelPointSize) ;
	Bf(H) = saveH ;
	L = B ;
	callLevel++ ;
}

static void LevelPointPop(void)
{
	callLevel-- ;
	if( B != L )
		InternalError("PopLevelPoint") ;
	RestoreState(LevelPointSize) ;
	Discard() ;
	P = cHdl(X4) ;					/* Restore P */
	L = cChoicePointPt(X5) ;		/* Restore L */
	if( DEBUG || callLevel > 0 )
		Info(4, "Exit reentrant Prolog call, level %d", callLevel) ;
}

static PrologEvent CallPrologHandleEvents(void)
{
	PrologEvent ev ;
	int lvrSize = LvrGetSize() ;
    jmp_buf newJB ;
    jmp_buf *saveJB = currJB ;
	currJB = &newJB ;
	ev = setjmp(*currJB) ;
	LvrSetSize(lvrSize) ;
#if DEBUG
	Mesg("%s %lx", PrologEventAsStr(ev), L) ;
#endif	
	switch( ev ) {
		case peNone: {	/* Startup */
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
		case peException:
		case peInterrupt:
		case peAbort:
		case peExit:
		case peHalt:
		case peFatalError: {
			currentResult = tNoResult ;
			currJB = saveJB ;
			CutTo(L) ;
			LevelPointPop() ;
			return ev ;
		}
		case peMoreSuccess: {
			currentResult = ChoicePointArg(L, 3) ;
			currJB = saveJB ;
			return peSucc ;
		}
		case peReturnSuccess: {
			currentResult = ChoicePointArg(L, 3) ;
			currJB = saveJB ;
			LevelPointPop() ;
			return peSucc ;			
		}
		case peReturnFailure: {
			currentResult = tNoResult ;

			currJB = saveJB ;
			LevelPointPop() ;
			return peFailure ;			
		}
		default:
			break ;
	}
	FatalError("CallPrologHandleEvents") ;
	return peOtherError ;
}

static PrologEvent CallPrologEnter(Pt goal, Bool multi, Hdl saveH)
{
	PrologEvent res ;
#if DEBUG
	Mesg(" CALL[%d] - %s ", callLevel, TermAsStr(goal)) ;
	DumpRegisters() ;
#endif
	if( !InMainThread(true) )
		return peOtherError ;

	LevelPointPush(saveH) ;
	X0 = goal ;
	P = PredCode(CheckPredicateByName(
					multi ? "$$_call_prolog_multi" : "$$_call_prolog", 1)) ;
	res = CallPrologHandleEvents() ;
#if DEBUG
	Mesg(" EXIT[%d] - EV = %s ", callLevel, PrologEventAsStr(res)) ;
	DumpRegisters() ;
#endif
	return res ; 
}

static PrologEvent CallPrologNext(void)
{
	if( IsEndOfChain(L) )
		return peOtherError ;
	else {
		P = Bf(P) ;
#if DEBUG
		Mesg(" RECALL[%d]", callLevel) ;
		DumpRegisters() ;
#endif
		return CallPrologHandleEvents() ;
	}
}

static PrologEvent CallPrologStop(void)
{
	if( IsEndOfChain(L) )
		return peOtherError ;
	else {
		currentResult = tNoResult ;
		CutTo(L) ;
		LevelPointPop() ;
		return peFailure ;
	}
}

void CallPrologSetResult(Pt t)
{
	if( IsEndOfChain(L) )
		FatalError("CallPrologSetResult") ;
	else {
		Pt p = ChoicePointArg(L, 3)	;
		ReleaseTerm(p) ;
		p = AllocateTermForAssign(t) ;
		ChoicePointArg(L, 3) = p ;
	}
}

Pt CallPrologGetResult(void)
{
	return currentResult	;	
}


/* Call once
    - Runs the goal once and only for side effects.
*/

PrologEvent CallProlog(Pt goal)
{		 //Internally called at portray/1, format/1/2/3, on_interrupt/1
	return CallPrologEnter(goal, false, H) ;
}

void CallPrologTransparent(Pt goal) /* Ignore result. */
{		//Internally called from call_cleanup/2
#if 0
	Mesg("CallPrologTransparent %s", TermAsStr(goal)) ;
	LvrPush(&goal) ;
	if( CallProlog(goal) == peException ) {
		Mesg("CallPrologTransparent Rethrow %s", TermAsStr(goal)) ;
		Rethrow() ;
	}
	else {
		Mesg("CallPrologTransparent Return %s", TermAsStr(goal)) ;
		LvrPop(&goal) ;
	}
#else
	if( CallProlog(goal) == peException )
		Rethrow() ;
#endif	
}


PrologEvent CallPrologAtom(Str atom)
{
	return CallProlog(MakeTempAtom(atom)) ;
}

PrologEvent CallPrologStr(Str goal)
{		 //Internally called at Java.c
	Hdl saveH = H ;
	Pt t = ZTermFromStrThruProlog(goal) ;
	return t == nil ? peOtherError : CallPrologEnter(t, false, saveH) ;
}

PrologEvent CallPrologStrTop(Str goal, CharPt *vars, CharPt *out)
{
	PrologEvent res ;
	Hdl saveH = H ;
	Pt t = ZTermFromStrThruProlog(goal) ;
	if( t == nil )
		res = peOtherError ;	
	else {
		if( vars != nil )
			t = GatherVarsPrepare(t) ;
		if( out != nil )
			CaptureOutputStart() ;	
		res = CallPrologEnter(t, false, saveH) ;
		if( out != nil )
			*out  = CaptureOutputEnd() ;
		if( vars != nil )
			*vars = res == peSucc ? GatherVars() : "" ;		
	}
	return res ;
}

PrologEvent CallPrologLinear(Str fmt, ...)
{
	Unused(fmt) ;
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
				if( !EventQueueMakeSpace(1 + len) goto abort ;
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
	return peSucc ;
}


/* Call multi
 - Iterates through all the solutions for the goal.
 - For each solution, the new logical variables instatiations becomes available.
 - The iteration ends with failure and the restoration of the initial Prolog state.
*/

PrologEvent CallPrologMulti(Pt goal)
{
	return CallPrologEnter(goal, true, H) ;
}

PrologEvent CallPrologMultiNext(void)
{
	return CallPrologNext() ;
}

PrologEvent CallPrologMultiStop(void)
{
	return CallPrologStop() ;
}

PrologEvent CallPrologMultiAtom(Str atom)
{
	return CallPrologMulti(MakeTempAtom(atom)) ;
}

PrologEvent CallPrologMultiStr(Str goal)
{
	Hdl saveH = H ;
	Pt t = ZTermFromStrThruProlog(goal) ;
	return t == nil ? peOtherError : CallPrologEnter(t, true, saveH) ;
}

PrologEvent CallPrologMultiStrTop(Str goal, CharPt *vars, CharPt *out)
{
	PrologEvent res ;
	Hdl saveH = H ;
	Pt t = ZTermFromStrThruProlog(goal) ;
	if( t == nil ) {
		res = peOtherError ;
		if( out != nil )
			*out  = "" ;
		if( vars != nil )
			*vars  = "" ;
	}		
	else {
		if( vars != nil )
			t = GatherVarsPrepare(t) ;
		if( out != nil )
			CaptureOutputStart() ;	
		res = CallPrologEnter(t, true, saveH) ;
		if( out != nil )
			*out  = CaptureOutputEnd() ;
		if( vars != nil )
			*vars = res == peSucc ? GatherVars() : "" ;		
	}
	return res ;
}

PrologEvent CallPrologMultiNextTop(CharPt *vars, CharPt *out)
{
	PrologEvent res ;
	if( out != nil )
		CaptureOutputStart() ;	
	res = CallPrologNext() ;
	if( out != nil )
		*out  = CaptureOutputEnd() ;
	if( vars != nil )
		*vars = res == peSucc ? GatherVars() : "" ;		
	return res ;
}

PrologEvent CallPrologMultiStopTop(CharPt *vars, CharPt *out)
{
	if( out != nil )
		*out = "" ;		
	if( vars != nil )
		*vars = "" ;		
	return CallPrologStop() ;
}



/* Start/Stop */

PrologEvent StartProlog(int argc, CharHdl argv, Fun yourExt)
{
	volatile PrologEvent res = peOtherError ;
	jmp_buf newJB ;
	jmp_buf *saveJB = currJB ;
	static Bool started = false ;
/* StartProlog is a no-op if called for a second time */
	if( started  )
		return peSucc ;
	started = true ;
	atexit(ExitProlog) ;
	currentResult = tNoResult ;
	currJB = &newJB ;
	if( setjmp(*currJB) == peNone ) {
/* The CxProlog interpreter can only be used by the thread that has created it */
		threadId = OSGetTid() ;
		VersionSet("CxProlog", 0, 97, 4, 0, CONTEXTS) ;
		Boot(argc, argv) ;
		CheckPredicateByName("$$_lux0", 0) ;
		CheckPredicateByName("$$_lux1", 0) ;
		if( yourExt != nil )
			yourExt() ;
		res = CallPrologAtom("$$_lux0") ;
		CheckPredicateByName("$cxprolog_initialise", 0) ;
		CheckPredicateByName("$cxprolog_top_level_goal", 0) ;
	}
	currJB = saveJB ;
#if DEBUG
		Mesg("RES0 = %s", PrologEventAsStr(res)) ;
#endif
	if( Booting() ) {
		ignoreEmergencyExit = true ;
		Error("Bad boot predicate") ;
		ignoreEmergencyExit = false ;
		res = peFatalError ;
	}
	return res ;
}

void StopProlog()
{
	/* Nothing */
}

int RunInteractiveProlog(int argc, CharHdl argv)
{
	PrologEvent res = StartProlog(argc, argv, nil) ;
	if( res == peException )
		WriteException() ;
	while( res == peSucc || res == peAbort || res == peException ) {
		RestartAll() ;
		HasATopLevel() = true ;
		if( (res = CallPrologAtom("$$_lux1")) == peException )
			WriteException() ;
#if DEBUG
		Mesg("RES1 = %s", PrologEventAsStr(res)) ;
#endif
	}
	if( NormalFinish() == undefined3 )
		NormalFinish() = true3 ;
	if( res == peHalt || res == peFatalError )
		BasicInfo("CxProlog halted") ;
	return res == peFatalError || res == peOtherError ? -1 : 0 ;
}		



/* CXPROLOG C'BUILTINS */

static void PAbort()
{
	EventAbort() ;
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

static void PExitScriptFast()
{
	NormalFinish() = false3 ;
	EventExit() ;
	JumpNext() ;
}

static void PCopyright()
{
	ShowVersion() ;
	Write("Copyright (C) 1990-2009 A.Miguel Dias, CITI, DI/FCT/UNL\n") ;
	Write("CxProlog is free software and is distributed in the hope that it\n") ;
	Write("will be useful, but WITHOUT ANY WARRANTY; without even the implied\n") ;
	Write("warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n") ;
	Write("See the GNU General Public License for more details.\n") ;
	JumpNext() ;
}

static void PMoreSuccess()
{
	SendPrologEvent(peMoreSuccess) ;
	JumpNext() ;
}

static void PReturnSuccess()
{
	SendPrologEvent(peReturnSuccess) ;
	JumpNext() ;
}

static void PWriteException() /* for code::blocks */
{
	WriteException() ;
	JumpNext() ;
}

static void PReturnFailure()
{
	SendPrologEvent(peReturnFailure) ;
	JumpNext() ;
}

static void PLevel()
{
	if( LevelPointCount() != callLevel )
		InternalError("PLevel") ;
	MustBe( UnifyWithNumber(X0, MakeInt(callLevel)) ) ;
}

static void PSetResult()
{
	CallPrologSetResult(X0) ;
	JumpNext() ;
}

static void PCallPrologThroughC()	/* For testing reentrant calls */
{
	PrologEvent ev ;
	if( (ev = CallPrologEnter(X0, false, H)) == peException )
		Rethrow() ;
	MustBe( ev == peSucc ) ;
}

static void PCallPrologThroughC2()	/* For testing reentrant calls */
{
	MustBe( CallProlog(X0) == peSucc ) ;
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
	CallPrologTestAux(true, "abort") ;
	CallPrologTestAux(true, "'$$_fatal_error'('fatal error')") ;
	CallPrologTestAux(true, "e r r o r") ;
	CallPrologTestAux(false, test) ;
	CallPrologTestAux(true, "true") ;
	JumpNext() ;
}

static void PSizes()
{
	ShowSizes() ;
	JumpNext() ;
}

void CallPrologInit()
{
	InstallCBuiltinPred("abort", 0, PAbort) ;
	InstallCBuiltinPred("restart", 0, PAbort) ;
	InstallCBuiltinPred("exit", 0, PExit) ;
	InstallCBuiltinPred("halt", 0, PHalt) ;
	InstallCBuiltinPred("$$_exit_script_fast", 0, PExitScriptFast) ;
	InstallCBuiltinPred("copyright", 0, PCopyright) ;
	InstallCBuiltinPred("$$_more_success", 0, PMoreSuccess) ;
	InstallCBuiltinPred("$$_return_success", 0, PReturnSuccess) ;
	InstallCBuiltinPred("$$_write_exception", 0, PWriteException) ;
	InstallCBuiltinPred("$$_return_failure", 0, PReturnFailure) ;
	InstallCBuiltinPred("$$_level", 1, PLevel) ;
	InstallCBuiltinPred("set_result", 1, PSetResult) ;
	InstallCBuiltinPred("$$_call_prolog_through_c", 1, PCallPrologThroughC) ;
	InstallCBuiltinPred("$$_call_prolog_through_c2", 1, PCallPrologThroughC2) ;
	InstallCBuiltinPred("$$_fatal_error", 1, PFatalError) ;
	InstallCBuiltinPred("$$_call_prolog_test", 0, PCallPrologTest) ;
	InstallCBuiltinPred("$$_sizes", 0, PSizes) ;
}

/* http://docs.python.org/c-api/init.html */
