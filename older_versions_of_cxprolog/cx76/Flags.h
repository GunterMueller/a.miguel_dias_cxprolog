/*
 *   This file is part of the CxProlog system

 *   Flags.h
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Flags_
#define _Flags_

extern Bool
	trace_flag,
	debugging_flag,
	failOnError_flag,
	smartAtoms_flag,
	compatibleIfThen_flag,
	keepSource_flag,
	undefWarnings_flag,
	memoryWarnings_flag,
	showText_flag,
	superIndexes_flag,
	forceVisibility_flag,
	localOperators_flag ;

void InitFlags(void) ;

#endif