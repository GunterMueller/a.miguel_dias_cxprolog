/*
 *   This file is part of the CxProlog system

 *   Flags.h
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Flags_
#define _Flags_


extern int
	debug_flag,
	interrupt_flag,
	occursCheck_flag,
	keepSource_flag,
	floatDisplayPrec_flag,
	eofCode_flag,
	gCollection_flag,
	indexParams_flag,

	compatibleIfThen_flag,
	compatibleStrings_flag,
	barIsSemicolon_flag,
	extraSpacesInTerms_flag,
	forceQuoted_flag,
	fileNameVariables_flag,

	onError_flag,
	unknown_flag,
	infoMessages_flag,
		
	javaAvailable_flag,
	wxWidgetsAvailable_flag,

	sysTrace_flag,
	testRelocation_flag,
	testGCollection_flag,

	allDynamic_flag,
	oldUpdate_flag,

	memoryWarnings_flag,
	oldFashionedSeeing_flag,
	forceVisibility_flag,
	semanticVariant_flag,
	localOperators_flag,
	undefWarnings_flag ;

extern int
	stacks_debugging,
	trail_debugging,
	scratch_debugging,
	index_debugging,
	gc_debugging,
	sysTrace_debugging,
	testRelocation_debugging,
	mesg_debugging ;

void FlagsSetDebugging(CharPt str) ;
void FlagsRestart(void) ;
void FlagsInit(void) ;

#endif
