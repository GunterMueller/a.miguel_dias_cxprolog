/*
 *   This file is part of the CxProlog system

 *   CxProlog.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _CxProlog_
#define _CxProlog_


/* LIMITS & DEFAULTS */

#define maxArity			64
#define maxVarsPerTerm		64
#define trailFactor			20
#define stacksSizeDefault	512 Kb
#define maxCodePerClause	20 Kb

/* ALL INCLUDES */

#include "Util.h"
#include "Stream.h"
#include "Mesg.h"
#include "Memory.h"
#include "Atom.h"
#include "HashTable.h"
#include "Extra.h"
#include "Term.h"
#include "ImperativeVar.h"
#include "Clock.h"
#include "Arith.h"
#include "TermChars.h"
#include "VarDictionary.h"
#include "Operator.h"
#include "TermRead.h"
#include "TermWrite.h"
#include "Clause.h"
#include "Predicate.h"
#include "Unit.h"
#include "Unify.h"
#include "Flags.h"
#include "Machine.h"
#include "Thread.h"
#include "ListCode.h"
#include "Debug.h"
#include "CodeGen.h"
#include "Index.h"
#include "Compiler.h"
#include "UnixServices.h"
#include "FileSys.h"

void Boot(CharPt bootFile) ;
void PrologEvent(int i) ;
void InitCxProlog(void) ;

void YourPrologue(int argc, CharPt argv[]) ;
void YourExtensions(void) ;

#endif
