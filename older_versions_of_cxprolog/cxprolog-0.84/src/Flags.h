/*
 *   This file is part of the CxProlog system

 *   Flags.h
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Flags_
#define _Flags_

extern int
	debug_flag,
	interrupt_flag,
	failOnError_flag,
	
	compatibleIfThen_flag,
	compatibleStrings_flag,
	barIsSemicolon_flag,
	keepSource_flag,
	undefWarnings_flag,
	memoryWarnings_flag,
	indexParams_flag,
	gCollection_flag,
	floatDisplayPrecision_flag,
	testStacks_flag,
	testGCollection_flag,
	forceVisibility_flag,
	localOperators_flag,

/* secret flag */
	reconsulting_flag ;

void FlagsRestart(void) ;
void FlagsInit(void) ;

#endif
