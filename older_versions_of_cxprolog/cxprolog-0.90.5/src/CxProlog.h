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

/* LIMITS & DEFAULTS */

#define maxArity			64
#define maxPredArity		32
#define maxVarsPerTerm		64
#define maxTermSize			64 K /* symbols */

/* ALL INCLUDES */

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#define __USE_ISOC99	1

#if linux
/* The symbol "unix" would not discriminate
     because gcc defines 'unix' on every plataform */
#define OS_UNIX			1
#define USE_WIDECHARS	1

#else
#if __APPLE__
#define OS_UNIX			1
#define USE_WIDECHARS	1

#else
#if macintosh
#define OS_CLASSIC_MAC	1

#else
#define OS_UNKNOWN		1

#endif /* macintosh */
#endif /* __APPLE__ */
#endif /* linux */


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
#include "TermChars.h"
#include "VarDictionary.h"
#include "Operator.h"
#include "TermRead.h"
#include "TermWrite.h"

/* Prolog Database */
#include "Clause.h"
#include "Predicate.h"
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

/* Inter-Operatibility & Graphical User Interface */
#include "CallProlog.h"
#include "Java.h"
#include "WXWidgets.h"
#include "Gui.h"

/* OS */
#include "FileSys.h"
#include "Net.h"
#include "CmdLine.h"
#include "OS.h"

#endif
