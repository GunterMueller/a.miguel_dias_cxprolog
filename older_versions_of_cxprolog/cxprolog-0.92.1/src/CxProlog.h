/*
 *   This file is part of the CxProlog system

 *   CxProlog.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _CxProlog_
#define _CxProlog_

#if __linux__ || __APPLE__
/* Compiler: gcc >= 2.95 */
#define OS_UNIX				1
#define USE_WIDECHARS		1

#elif _WIN32
/* Compiler: Microsoft Visual Studio >= 7.0 */
#define OS_WIN				1
#define USE_WIDECHARS		1

#elif __MSDOS__
/* Compiler: djgpp-gcc >= 2.04 */
#define OS_UNIX				1

#elif macintosh
/* Compiler: thinkc >= 5.0 */
#define OS_CLASSIC_MAC		1

#else
#define OS_UNKNOWN			1

#endif


/* LIMITS & DEFAULTS */

#define maxFunctorArity		64
#define maxPredArity		32
#define maxTermSize			64 K /* symbols */

/* ALL INCLUDES */

#include <errno.h>		/* Must be first because it undefines __USE_ISOC99 */

#define __USE_ISOC99		1
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

/* Configure */
#include "Configure.h"

/* Utilities */
#include "Util.h"
#include "Memory.h"
#include "Utf8.h"
#include "String.h"
#include "Scratch.h"
#include "Clock.h"
#include "Version.h"

/* Prolog Types */
#include "Atom.h"
#include "Number.h"
#include "Extra.h"
#include "Term.h"

/* Extra Types */
#include "Array.h"
#include "Buffer.h"
#include "Dict.h"
#include "Process.h"
#include "Queue.h"
#include "Stack.h"
#include "Thread.h"

/* Input & Output */
#include "Locale.h"
#include "Stream.h"
#include "Mesg.h"

/* Reader & Writer */
#include "Character.h"
#include "VarDictionary.h"
#include "Operator.h"
#include "TermRead.h"
#include "TermWrite.h"

/* Prolog Database */
#include "Clause.h"
#include "Predicate.h"
#include "PredicateProperty.h"
#include "Consult.h"
#include "Unit.h"
#include "IVar.h"

/* Compiler */
#include "CodeGen.h"
#include "Compiler.h"
#include "Disassembler.h"

/* Abstract Machine */
#include "Arith.h"
#include "Unify.h"
#include "GCollection.h"
#include "Flags.h"
#include "Machine.h"
#include "Instructions.h"
#include "Index.h"
#include "Exception.h"

/* Debug */
#include "Attention.h"
#include "Debug.h"
#include "SysTrace.h"

/* Startup */
#include "Boot.h"

/* Inter-Operatibility */
#include "CallProlog.h"
#include "Java.h"
#include "WXWidgets.h"

/* Graphical User Interface */
#include "GuiJava.h"
#include "GuiWXWidgets.h"

/* OS */
#include "FileSys.h"
#include "Net.h"
#include "CmdLine.h"
#include "OS.h"

#endif
