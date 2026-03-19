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

static void Boot(CharPt bootFile)
{
	Pt t ;
	int n = 0 ;
	Hdl saveH = H ;
	
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

static void CheckHost(void)
{
	if( cWord(&stacksBegin) % sizeof(Word) != 0 )
		Warning("Machine registers are not aligned") ;
	if( cWord(stacksBegin) < 0 )
		Warning("This machine is backwards") ;
	if( GetTag(stacksBegin) != 0 )
		Warning("Tags are clobbered by Pt values") ;
	if( cWord(stacksBegin) % sizeof(Word) != 0 )
		Warning("Memory areas are not aligned") ;
}

static CharPt GetCxEnvVar(int argc, CharPt argv[])
{
	CharPt s ;

	if( argc == 2 )
		return argv[1] ;
	if( (s = getenv("CXPROLOG")) != nil )
		return s ;
	return "CxBoot.pl" ;
}

static void PatchPredicates()
{
	PatchBuiltinPredicate("repeat", 0) ;
	PatchBuiltinPredicate("$$_clause", 3) ;
	PatchBuiltinPredicate("$$_retract", 2) ;
	PatchBuiltinPredicate("$$_current_predicate", 2) ;
	PatchBuiltinPredicate("$$_visible_predicate", 2) ;
	PatchBuiltinPredicate("$$_imported_predicate", 3) ;
	PatchBuiltinPredicate("$$_builtin_predicate", 2) ;
	PatchBuiltinPredicate("$$_current_unit", 2) ;
	PatchBuiltinPredicate("$$_current_ivar", 2) ;
	PatchBuiltinPredicate("$$_current_op", 2) ;
}


static jmp_buf eventHandler ;

void PrologEvent(int i)
{
	longjmp(eventHandler, i) ;
}

main(int argc, CharPt argv[])
{
	ThreadPt th ;
	CharPt bootFile = GetCxEnvVar(argc, argv) ;
	
	printf("CxProlog version 0.70\n") ;
	switch( setjmp(eventHandler) ) {
		case 0: {	/* startup */
			InitStreams() ;
			InitSpace() ;
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

			InstallCBuiltinPreds() ;		/* The basic C builtin predicates */
			InstallExtraCBuiltinPreds() ;	/* The C builtin predicates for extra term types */
			InstallMoreCBuiltinPreds() ;	/* The C builtin predicates added by the user */
			FinishInstallingCBuiltinPreds() ;

			th = CreateThread(LookupAtom("root_thread"), stacksAreaSize,
								MakeAtom("$$_lux0"), MakeAtom("$$_lux1")) ;
			if( th == nil ) Error("Not enough memory") ;
			CheckHost() ;
			Mesg("Booting from '%s'", bootFile) ;
			Boot(bootFile) ;
			PatchPredicates() ;
			
			TransferToThread(th) ;
			break ;
		}
		case 1: {	/* restart current thread */
			RecoverStreams() ;
			if( not MachineIsOn() )
				FatalError("In boot") ;
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
	return( b ? "yes" : "no" ) ;
}

void PrintStatus()
{
	Write("\tStatus:\n") ;
	Write("%20s: %s\n", "trace", YesNo(GetTraceOnFlag())) ;
	Write("%20s: %s\n", "force_visibility", YesNo(GetForceVisibilityFlag())) ;
	Write("%20s: %s\n", "local_operators", YesNo(GetLocalOperatorsFlag())) ;
}
