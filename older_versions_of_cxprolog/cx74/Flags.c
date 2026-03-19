/*
 *   This file is part of the CxProlog system

 *   Flags.c
 *   by A.Miguel Dias - 2000/08/22
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

Bool
	trace_flag = false,
	traceActive_flag = false,
	inTopGoal_flag = false,

	failOnError_flag = false,
	smartAtoms_flag = true,
	silent_flag = false,

	keepSource_flag = true,
	undefWarnings_flag = true,
	memoryWarnings_flag = true,
	showText_flag = false,
	superIndexes_flag = false,

	forceVisibility_flag = false,
	localOperators_flag = false ;

static void SetFlag(CharPt s, Bool b)
{
 switch( s[0] ) {
  case '@':	if( EqualStr(s, "@@_in_top_goal") ) { inTopGoal_flag = b ; return ; }
			break ;
  case 'f':	if( EqualStr(s, "fail_on_error") ) { failOnError_flag = b ; return ; }
			if( EqualStr(s, "force_visibility") ) { forceVisibility_flag = b ; return ; }
			break ;
  case 'k':	 if( EqualStr(s, "keep_source") ) { keepSource_flag = b ; return ; }
			break ;
 case 'l':	if( EqualStr(s, "local_operators") ) { localOperators_flag = b ; return ; }
			break ;
  case 'm':	if( EqualStr(s, "memory_warnings") ) { memoryWarnings_flag = b ; return ; }
			break ;
  case 't':	if( EqualStr(s, "trace") ) { traceActive_flag = trace_flag = b ; return ; }
			break ;
  case 's':	if( EqualStr(s, "smart_atoms") ) { smartAtoms_flag = b ; return ; }
			if( EqualStr(s, "show_text") ) { showText_flag = b ; return ; }
			if( EqualStr(s, "super_indexes") ) { superIndexes_flag = b ; return ; }
			if( EqualStr(s, "silent") ) { silent_flag = b ; return ; }
			break ;
  case 'u':	if( EqualStr(s, "undef_warnings") ) { undefWarnings_flag = b ; return ; }
			break ;
  }
  Error("Unknown flag: '%s'", s) ;
}

static Bool GetFlag(CharPt s)
{
 switch( s[0] ) {
  case '@':	if( EqualStr(s, "@@_in_top_goal") ) return inTopGoal_flag ;
			break ;
  case 'f':	if( EqualStr(s, "fail_on_error") ) return failOnError_flag ;
			if( EqualStr(s, "force_visibility") ) return forceVisibility_flag ;
			break ;
  case 'k':	if( EqualStr(s, "keep_source") ) return keepSource_flag ;
			break ;
  case 'l':	if( EqualStr(s, "local_operators") ) return localOperators_flag ;
			break ;
  case 'm':	if( EqualStr(s, "memory_warnings") ) return memoryWarnings_flag ;
			break ;
  case 't':	if( EqualStr(s, "trace") ) return trace_flag ;
			break ;
  case 's':	if( EqualStr(s, "smart_atoms") ) return smartAtoms_flag ;
			if( EqualStr(s, "show_text") ) return showText_flag ;
			if( EqualStr(s, "super_indexes") ) return superIndexes_flag ;
			if( EqualStr(s, "silent") ) return silent_flag ;
			break ;
  case 'u':	if( EqualStr(s, "undef_warnings") ) return undefWarnings_flag ;
			break ;
  }
  Error("Unknown flag: '%s'", s) ;
  return false ;
}

static void PrintFlag(CharPt s, Bool b)
{
	Write("%20s: %s\n", s, b ? "true" : "false") ;
}

void PrintFlags()
{
	PrintFlag("trace", trace_flag) ;
	PrintFlag("fail_on_error", failOnError_flag) ;
	PrintFlag("smart_atoms", smartAtoms_flag) ;
	PrintFlag("silent", silent_flag) ;
	PrintFlag("keep_source", keepSource_flag) ;
	PrintFlag("undef_warnings", undefWarnings_flag) ;
	PrintFlag("memory_warnings", memoryWarnings_flag) ;
	PrintFlag("show_text", showText_flag) ;
	PrintFlag("super_indexes", superIndexes_flag) ;
/*
	PrintFlag("force_visibility", forceVisibility_flag) ;
	PrintFlag("local_operators", localOperators_flag) ;
*/
}

void FlagsForRestart()
{
	traceActive_flag = trace_flag ;
	inTopGoal_flag = false ;
}


/* CXPROLOG C'BUILTINS */

static void PFlagOn()
{
	SetFlag(XTestAtomName(X0), true) ;
	JumpNext()
}

static void PFlagOff()
{
	SetFlag(XTestAtomName(X0), false) ;
	JumpNext()
}

static void PFlagCheck()
{
	if( GetFlag(XTestAtomName(X0)) ) JumpNext()
	DoFail()
}

static void PFlags()
{
	PrintFlags() ;
	JumpNext()
}




static void PSetTrace()
{
	traceActive_flag = trace_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckTrace()
{
	if( trace_flag ) JumpNext()
	DoFail()
}

static void PSetFailOnError()
{
	failOnError_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckFailOnError()
{
	if( failOnError_flag ) JumpNext()
	DoFail()
}

static void PSetSmartAtoms()
{
	smartAtoms_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckSmartAtoms()
{
	if( smartAtoms_flag ) JumpNext()
	DoFail()
}

static void PSetSilent()
{
	silent_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckSilent()
{
	if( silent_flag ) JumpNext()
	DoFail()
}

static void PSetKeepSource()
{
	keepSource_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckKeepSource()
{
	if( keepSource_flag ) JumpNext()
	DoFail()
}

static void PSetUndefWarnings()
{
	undefWarnings_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckUndefWarnings()
{
	if( undefWarnings_flag ) JumpNext()
	DoFail()
}

static void PSetMemoryWarnings()
{
	memoryWarnings_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckMemoryWarnings()
{
	if( memoryWarnings_flag ) JumpNext()
	DoFail()
}

static void PSetShowText()
{
	showText_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckShowText()
{
	if( showText_flag ) JumpNext()
	DoFail()
}

static void PSetSuperIndexes()
{
	superIndexes_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckSuperIndexes()
{
	if( superIndexes_flag ) JumpNext()
	DoFail()
}

static void PSetInTopGoal()
{
	inTopGoal_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckInTopGoal()
{
	if( inTopGoal_flag ) JumpNext()
	DoFail()
}

static void PSetForceVisibility()
{
	forceVisibility_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckForceVisibility()
{
	if( forceVisibility_flag ) JumpNext()
	DoFail()
}

static void PSetLocalOperators()
{
	localOperators_flag = XTestBool(X0) ;
	JumpNext()
}

static void PCheckLocalOperators()
{
	if( localOperators_flag ) JumpNext()
	DoFail()
}


void InitFlags()
{
	InstallCBuiltinPred("flag_on", 1, PFlagOn) ;
	InstallCBuiltinPred("flag_off", 1, PFlagOff) ;
	InstallCBuiltinPred("flag_check", 1, PFlagCheck) ;
	InstallCBuiltinPred("flags", 0, PFlags) ;


	InstallCBuiltinPred("set_trace", 1, PSetTrace) ;
	InstallCBuiltinPred("check_trace", 0, PCheckTrace) ;

	InstallCBuiltinPred("@@_set_in_top_goal", 1, PSetInTopGoal) ;
	InstallCBuiltinPred("@@_check_in_top_goal", 0, PCheckInTopGoal) ;

	InstallCBuiltinPred("set_fail_on_error", 1, PSetFailOnError) ;
	InstallCBuiltinPred("check_fail_on_error", 0, PCheckFailOnError) ;

	InstallCBuiltinPred("set_smart_atoms", 1, PSetSmartAtoms) ;
	InstallCBuiltinPred("check_smart_atoms", 0, PCheckSmartAtoms) ;

	InstallCBuiltinPred("set_silent", 1, PSetSilent) ;
	InstallCBuiltinPred("check_silent", 0, PCheckSilent) ;

	InstallCBuiltinPred("set_keep_source", 1, PSetKeepSource) ;
	InstallCBuiltinPred("check_keep_source", 0, PCheckKeepSource) ;

	InstallCBuiltinPred("set_undef_warnings", 1, PSetUndefWarnings) ;
	InstallCBuiltinPred("check_undef_warnings", 0, PCheckUndefWarnings) ;

	InstallCBuiltinPred("set_memory_warnings", 1, PSetMemoryWarnings) ;
	InstallCBuiltinPred("check_memory_warnings", 0, PCheckMemoryWarnings) ;

	InstallCBuiltinPred("set_show_text", 1, PSetShowText) ;
	InstallCBuiltinPred("check_show_text", 0, PCheckShowText) ;

	InstallCBuiltinPred("set_super_indexes", 1, PSetSuperIndexes) ;
	InstallCBuiltinPred("check_super_indexes", 0, PCheckSuperIndexes) ;

	InstallCBuiltinPred("set_force_visibility", 1, PSetForceVisibility) ;
	InstallCBuiltinPred("check_visibility", 0, PCheckForceVisibility) ;

	InstallCBuiltinPred("set_local_operators", 1, PSetLocalOperators) ;
	InstallCBuiltinPred("check_local_operators", 0, PCheckLocalOperators) ;
}
