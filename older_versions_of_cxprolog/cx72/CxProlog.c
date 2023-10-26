/*
 *   This file is part of the CxProlog system

 *   CxProlog.c
 *   by A.Miguel Dias - 2000/04/02
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

void Boot(CharPt bootFile)
{
	Pt t ;
	int n = 0 ;
	Hdl saveH = H ;
	
	if( not silent_flag )
		Mesg("Booting from '%s'", bootFile) ;
	See(bootFile) ;
	while( (t = ReadTerm(nil)) != tEofAtom ) {
		n++ ;
		if( t == nil ) {
			Write("Error in clause number %d at file %s:\n\t", n, bootFile) ;
			WriteTerm(t) ; Nl() ;
		}
		else {
#if 0
			WriteTerm(t) ; Nl() ;
#endif
			CompileClause(t, true) ;
			H = saveH ;
		}
	}
	Seen() ;
}	

static long stacksSize = stacksSizeDefault ;/* default stacks size	*/
static CharPt bootFileName = "CxBoot.pl" ;	/* default boot file name */

static void CxPrologue(int argc, CharPt argv[])
{
	CharPt s ;
	int i ;

	if( (s = getenv("CXPROLOG")) != nil ||
		(s = getenv("CXPROLOG_BOOT")) != nil ||
		(s = GetArgvArg(argc, argv, "-boot")) != nil )
			bootFileName = s ;

	if( (s = getenv("CXPROLOG_SIZE")) != nil ||
		(s = GetArgvArg(argc, argv, "-size")) != nil )
	{
		if( sscanf(s, "%d", &i) != 1 )
			Error("Invalid size") ;
		if( not InRange(i, 0, 100000) )
			Error("size out of range (0..100000)") ;
		stacksSize = i Kb ;		
	}
}

static jmp_buf eventHandler ;

void PrologEvent(int i)
{
	longjmp(eventHandler, i) ;
}

main(int argc, CharPt argv[])
{
	ThreadPt th ;
	
	/*silent_flag = true ;*/
	switch( setjmp(eventHandler) ) {
		case 0: {	/* startup */
			InitStreams() ;
			CxPrologue(argc, argv) ;
			YourPrologue(argc, argv) ;
			PostArgvCheck(argc, argv) ;
			if( not silent_flag )
				WriteStd("CxProlog version 0.72\n") ;

			InitMemory() ;
			CheckHost(codeBuffer) ;
			InitReals() ;
			InitTime() ;
			InitIndex() ;
			InitAtoms() ;
			InitFunctors() ;
			InitMachine() ;
			InitCompiler() ;			

			InitUnits() ;
			InitFlags() ;
			InitTerms() ;
			InitArith() ;
			InitUnify() ;
			InitOperators() ;
			InitPredicates() ;
			InitThreads() ;
			InitTermRead() ;
			InitTermWrite() ;
			InitCxProlog() ;

			InitUnixServices() ;
			InitFileSys() ;
			InitImperativeVars() ;

			th = CreateThread(LookupAtom("root_thread"),
						stacksSize, MakeAtom("@@_lux0"), MakeAtom("@@_lux1")) ;
			if( th == nil ) Error("Not enough memory") ;

			Boot(bootFileName) ;
			CheckHostSpeed() ;

			YourExtensions() ;
			TransferToThread(th) ;
			break ;
		}
		case 1: {	/* restart current thread */
			RecoverStreams() ;
			FlagsForRestart() ;
			if( not MachineIsOn() )
				PrologEvent(4) ; /* an error ocurred at boot time */
			RestartCurrThread() ;
			break ;
		}
		case 2: {	/* finished current thread; restore father-thread */
			RecoverStreams() ;
			FinishCurrThread(tCompletedAtom) ;
			break ;
		}
		case 3: {	/* failed current thread; restore father-thread */
			RecoverStreams() ;
			FinishCurrThread(tFailedAtom) ;
			break ;
		}
		case 4: {	/* halt */
			RecoverStreams() ;
			FlagsForRestart() ;
			if( not silent_flag )
				WriteStd("CxProlog halted.\n") ;
			break ;
		}
		case 5: {	/* abort */
			RecoverStreams() ;
			FlagsForRestart() ;
			WriteStd("Execution aborted\n") ;
			RestartCurrThread() ;
			break ;
		}
		case 6: {	/* force fail */
			P = Bf(P) ;
			ContinueMachine() ;
			break ;
		}
		default: Default("main") ;
	}
}

/* CXPROLOG C'BUILTINS */

static void PEvent()
{
	PrologEvent(XTestInt(X0)) ;
	JumpNext()
}

static void PHostSpeed()
{
	if( Unify(X0, MakeInt(hostSpeed)) ) JumpNext()
	DoFail()
}

static void PCut()
{
	Error("Dynamic '!/0' is not supported") ;
	JumpNext()
}

static void PGetLevel()
{
	if( UnifyWithAtomic(X0, TagAtom(Ef(B0))) ) JumpNext()
	DoFail()
}

static void PCutTo()
{
	Pt t0 = Drf(X0) ;
	if( cChoicePointPt(XPt(t0)) > B ) {
		B = cChoicePointPt(XPt(t0)) ;
		HB = Bf(H) ;
	
	/* TidyTrail
		t0 = cPt(Bf(TR)) ;
		while( cHdl(t0) < TR )
			if( IsToTrailVar(*cHdl(t0)) ) t0++ ;
			else *t0 = cWord(Pop(TR)) ; */
	}
	JumpNext()
}

void InitCxProlog()
{
	InstallCBuiltinPred("@@_event", 1, PEvent) ;
	InstallCBuiltinPred("host_speed", 1, PHostSpeed) ;
	InstallCBuiltinPred("!", 0, PCut) ;
	InstallCBuiltinPred("get_level", 1, PGetLevel) ;	/* @@ */
	InstallCBuiltinPred("cut", 1, PCutTo) ;	/* @@ */
}

