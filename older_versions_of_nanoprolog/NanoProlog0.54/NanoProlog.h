/*
 *   This file is part of the NanoProlog system

 *   Prolog.h
 *   by A.Miguel Dias - 89/11/14
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931203: lots of changes
 931117: release of version 0.5

*/

#ifndef _NanoProlog_
#define _NanoProlog_

#define Mips		/* <- #DEFINE HERE THE MACHINE/COMPILER SYMBOL */   1

#define bootFileName		"NanoBoot.pl"
#define stacksAreaSize		 150	/* kbytes */
#define heapAreaSize		 300	/* kbytes */
#define maxVarsPerTerm		  64
#define maxCodePerClause	1024	/* words */

/**********************************************************************************
 *	Each machine/compiler combination requires the following symbols:
 *	tagSet: The set of tags used for term representation (as defined in file "Term.h")
 *	ieeeFloats: Two formats for reals are suported (in file "Term.h")
 *	clockDef: [default=2] Controls the method of accessing the system clock (file "Arith.c")
 *	threadedCode: [default=0] Enables the threaded code (file "ThreadedCode.h") 
 */

#ifdef Mac			/* Mac, ThinkC 5.0 */
#define tagSet			1
#define ieeeFloats		1
#define threadedCode	1
#define clockDef		1
#endif

#ifdef Vax			/* Vax, gcc */
#define tagSet			1
#define ieeeFloats		0
#endif

#ifdef Next			/* Next, cc */
#define tagSet			1
#define ieeeFloats		1
#endif

#ifdef Mips			/* Mips, gcc */
#define tagSet			2
#define ieeeFloats		1
#define clockDef		1
#endif

#ifdef Alpha		/* Alpha, gcc */
#define tagSet			4
#define ieeeFloats		1
#endif

#ifdef Sparc		/* Sparc, gcc */
#define tagSet			1
#define ieeeFloats		1
#endif

#ifdef R6000		/* R6000, gcc */
#define tagSet			2
#define ieeeFloats		1
#endif

#ifdef Hp			/* Hp, gcc */
#define tagSet			2
#define ieeeFloats		1
#endif

#ifdef Avion		/* Avion, gcc */
/*** NOT YET TESTED ***/
#define tagSet			1
#define ieeeFloats		1
#endif

#ifdef Transputer	/* Transputer, gcc */
/*** NOT YET TESTED ***/
#define tagSet			3
#define ieeeFloats		1
#endif

/**********************************************************************************/

#ifndef clockDef
#define clockDef		2
#endif
#ifndef threadedCode
#define threadedCode	0
#endif

/* ALL INCLUDES */

#include "ThreadedCode.h"
#include "Util.h"
#include "Stream.h"
#include "Heap.h"
#include "Term.h"
#include "Arith.h"
#include "TermOp.h"
#include "TermRead.h"
#include "TermWrite.h"
#include "Predicates.h"
#include "Unify.h"
#include "Interpreter.h"
#include "Thread.h"
#include "ListCode.h"
#include "Index.h"
#include "Compiler.h"
#include "Module.h"

#endif
