/*
 *   This file is part of the CxProlog system

 *   Flags.c
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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
	debugging_flag = true,

	failOnError_flag = false,
	smartAtoms_flag = true,
	compatibleIfThen_flag = false,

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
  case 'c':	if( EqualStr(s, "compatible_if_then") )
  				{ CompatibleIfThen(compatibleIfThen_flag = b) ; return ; }
			break ;
  case 'd':	if( EqualStr(s, "debugging") ) { debugging_flag = b ; DebugUpdate() ; return ; }
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
  case 't':	if( EqualStr(s, "trace") ) { trace_flag = b ; DebugUpdate() ; return ; }
			break ;
  case 's':	if( EqualStr(s, "smart_atoms") ) { smartAtoms_flag = b ; return ; }
			if( EqualStr(s, "show_text") ) { showText_flag = b ; return ; }
			if( EqualStr(s, "super_indexes") ) { superIndexes_flag = b ; return ; }
			break ;
  case 'u':	if( EqualStr(s, "undef_warnings") ) { undefWarnings_flag = b ; return ; }
			break ;
  }
  Error("Unknown flag: '%s'", s) ;
}

static Bool GetFlag(CharPt s)
{
 switch( s[0] ) {
  case 'c':	if( EqualStr(s, "compatible_if_then") ) return compatibleIfThen_flag ;
			break ;
  case 'd':	if( EqualStr(s, "debugging") ) return debugging_flag ;
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
			break ;
  case 'u':	if( EqualStr(s, "undef_warnings") ) return undefWarnings_flag ;
			break ;
  }
  Error("Unknown flag: '%s'", s) ;
  return false ;
}

static void PrintFlag(CharPt s, Bool b)
{
	Write("%20s: %s\n", s, b ? "on" : "off") ;
}

static void ShowFlags()
{
	ShowVersion() ;
	Write("\tFlags:\n") ;
	PrintFlag("debugging", debugging_flag) ;
	PrintFlag("trace", trace_flag) ;
	PrintFlag("fail_on_error", failOnError_flag) ;
	PrintFlag("compatible_if_then", compatibleIfThen_flag) ;
	PrintFlag("keep_source", keepSource_flag) ;
	PrintFlag("smart_atoms", smartAtoms_flag) ;
	PrintFlag("undef_warnings", undefWarnings_flag) ;
	PrintFlag("memory_warnings", memoryWarnings_flag) ;
	PrintFlag("show_text", showText_flag) ;
	PrintFlag("super_indexes", superIndexes_flag) ;
/*
	PrintFlag("force_visibility", forceVisibility_flag) ;
	PrintFlag("local_operators", localOperators_flag) ;
*/
}


/* CXPROLOG C'BUILTINS */

static void PFlag2()
{
	CharPt fl = XTestAtomName(X0) ;
	if( UnifyWithAtomic(X1, GetFlag(XTestAtomName(X0)) ? tOnAtom : tOffAtom) )
		JumpNext()	
	DoFail()
}	

static void PFlag3()
{
	CharPt fl = XTestAtomName(X0) ;
	if( UnifyWithAtomic(X1, GetFlag(XTestAtomName(X0)) ? tOnAtom : tOffAtom) ) {
		SetFlag(fl, XTestFlag(X2) == tOnAtom) ;
		JumpNext()	
	}
	DoFail()
}	

static void PFlags()
{
	ShowFlags() ;
	JumpNext()
}

void InitFlags()
{
	InstallCBuiltinPred("flag", 2, PFlag2) ;
	InstallCBuiltinPred("flag", 3, PFlag3) ;
	InstallCBuiltinPred("flags", 0, PFlags) ;
}
