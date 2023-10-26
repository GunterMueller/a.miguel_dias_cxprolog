/*
 *   This file is part of the CxProlog system

 *   CxProlog.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _CxProlog_
#define _CxProlog_


/* LIMITS & DEFAULTS */

#define maxArity			64
#define maxPredArity		32
#define maxVarsPerTerm		64
#define maxTermSize			64 K /* symbols */

/* ALL INCLUDES */

#include "Util.h"
#include "Stream.h"
#include "Mesg.h"
#include "Memory.h"
#include "Buffer.h"
#include "Atom.h"
#include "GCollection.h"
#include "HashTable.h"
#include "Term.h"
#include "Number.h"
#include "Extra.h"
#include "IVar.h"
#include "Clock.h"
#include "Arith.h"
#include "TermChars.h"
#include "VarDictionary.h"
#include "Operator.h"
#include "Stream2.h"
#include "TermRead.h"
#include "TermWrite.h"
#include "Clause.h"
#include "Predicate.h"
#include "Unit.h"
#include "Unify.h"
#include "Flags.h"
#include "Machine.h"
#include "Instructions.h"
#include "Thread.h"
#include "Disassembler.h"
#include "Attention.h"
#include "Debug.h"
#include "CodeGen.h"
#include "Index.h"
#include "Compiler.h"
#include "Queue.h"
#include "Stack.h"
#include "Dict.h"
#include "FileSys.h"
#include "Socket.h"
#include "Process.h"
#include "CmdLine.h"
#include "OS.h"

void EventContinue(void) ;
void EventForceFail(void) ;
void EventRestart(void) ;
void EventHalt(void) ;
void EventExit(void) ;
void EventForceFail(void) ;
void VersionShow(void) ;

void YourPrologue(void) ;
void YourExtensions(void) ;

#endif
