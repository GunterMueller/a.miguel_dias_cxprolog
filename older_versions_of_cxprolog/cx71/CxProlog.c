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

Bool GetArgvSwitch(int argc, CharPt argv[], CharPt sw)
{
	int i ;
	for( i = 1 ; i < argc ; i++ )
		if( argv[i] != nil && EqualStr(argv[i], sw) ) {
			argv[i] = nil ;
			return true ;
		}
	return false ;
}

CharPt GetArgvArg(int argc, CharPt argv[], CharPt sw)
{
	int i ;
	CharPt s ;
	for( i = 1 ; i < argc - 1 ; i++ )
		if( argv[i] != nil && EqualStr(argv[i], sw) ) {
			s = argv[i+1] ;
			argv[i] = nil ;
			argv[i+1] = nil ;
			return s ;
		}
	return nil ;
}

void PostArgvCheck(int argc, CharPt argv[])
{
	int i ;
	for( i = 1 ; i < argc ; i++ )
		if( argv[i] != nil )
			FatalError("Unknown parameter in command line '%s'", argv[i]) ;
}

void Boot(CharPt bootFile)
{
	Pt t ;
	int n = 0 ;
	Hdl saveH = H ;
	
	if( not CheckSilent() )
		Mesg("Booting from '%s'", bootFile) ;
	See(bootFile) ;
	while( (t = ReadTerm(nil)) != tEofAtom ) {
		n++ ;
		if( t == nil ) {
			Write("Error in clause number %d at file %s:\n\t", n, bootFile) ;
			WritelnTerm(t) ;
		}
		else {
#if 0
			WritelnTerm(t) ;
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
	
	//SetSilent(true) ;
	switch( setjmp(eventHandler) ) {
		case 0: {	/* startup */
			InitStreams() ;
			CxPrologue(argc, argv) ;
			YourPrologue(argc, argv) ;
			PostArgvCheck(argc, argv) ;
			if( not CheckSilent() )
				WriteStd("CxProlog version 0.71\n") ;

			InitMemory() ;
			CheckHost(codeBuffer) ;
			InitReals() ;
			InitTime() ;
			InitIndex() ;
			InitAtoms() ;
			InitFunctors() ;
			InitUnits() ;
			InitMachine() ;
			InitOperators() ;
			InitImperativeVars() ;
			InitCompiler() ;

			th = CreateThread(LookupAtom("root_thread"), stacksSize,
								MakeAtom("@@_lux0"), MakeAtom("@@_lux1")) ;
			if( th == nil ) Error("Not enough memory") ;

			InstallCBuiltinPreds() ;
			InstallExtraCBuiltinPreds() ;
			Boot(bootFileName) ;

			YourExtensions() ;
			TransferToThread(th) ;
			break ;
		}
		case 1: {	/* restart current thread */
			RecoverStreams() ;
			SetInTopGoal(false) ;
			if( not MachineIsOn() )
				PrologEvent(4) ; // an error ocurred at boot time
			RestartCurrThread() ;
			break ;
		}
		case 2: {	/* finish current thread; restore father-thread */
			RecoverStreams() ;
			FinishCurrThread("completed") ;
			break ;
		}
		case 3: {	/* failed current thread; restore father-thread */
			RecoverStreams() ;
			FinishCurrThread("failed") ;
			break ;
		}
		case 4: {	/* halt */
			RecoverStreams() ;
			if( not CheckSilent() )
				WriteStd("CxProlog halted.\n") ;
			break ;
		}
		case 5: {	/* abort */
			RecoverStreams() ;
			WriteStd("Execution aborted\n") ;
			RestartCurrThread() ;
			break ;
		}
		default: Default("main") ;
	}
}

static CharPt YesNo(int b)
{
	return( b ? "true" : "false" ) ;
}

void PrintFlags()
{
	Write("%20s: %s\n", "trace", YesNo(CheckTrace())) ;
	Write("%20s: %s\n", "silent", YesNo(CheckSilent())) ;
	Write("%20s: %s\n", "force_visibility", YesNo(CheckForceVisibility())) ;
	Write("%20s: %s\n", "local_operators", YesNo(CheckLocalOperators())) ;
	Write("%20s: %s\n", "in_top_level_goal", YesNo(CheckInTopGoal())) ;
}
