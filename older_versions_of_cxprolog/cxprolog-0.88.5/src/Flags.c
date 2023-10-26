/*
 *   This file is part of the CxProlog system

 *   Flags.c
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

int
	debug_flag = 0,
	interrupt_flag = 2,

	failOnError_flag = 0,
	compatibleIfThen_flag = true,
	compatibleStrings_flag = true,
	barIsSemicolon_flag = true,
	eofCode_flag = -1,

	keepSource_flag = true,
	lcoOptimization_flag = true,
	undefWarnings_flag = true,
	memoryWarnings_flag = true,
	indexParams_flag = 1,
	gCollection_flag = 1,
	
	floatDisplayPrecision_flag = 6,

	testRelocation_flag = 0,
	testGCollection_flag = 0,
	sysTrace_flag = 0,

	forceVisibility_flag = false,
	localOperators_flag = false,

/* secret flag */
	reconsulting_flag = 0 ;

typedef struct {
	CharPt name ;
	int *flag ;
	int type ;
	Proc1 updateProc ;
} FlagEntry, *FlagEntryPt ;

static FlagEntry flagTable[] =
{
	"debug",			&debug_flag,			'2', DebugUpdateFlags,
	"interrupt",		&interrupt_flag,		'4', nil,

	"fail_on_error",	&failOnError_flag,		'2', nil,
	"compatible_if_then",&compatibleIfThen_flag,'b', CompatibleIfThenUpdateFlags,
	"compatible_strings",&compatibleStrings_flag,'b', nil,
	"bar_is_semicolon",	&barIsSemicolon_flag,	'b', nil,
	"eof_code",			&eofCode_flag,			'i', nil,

	
	"keep_source",		&keepSource_flag,		'b', nil,
	"lco_optimization",	&lcoOptimization_flag,	'b', nil,
	"undef_warnings", 	&undefWarnings_flag, 	'b', nil,
	"memory_warnings",	&memoryWarnings_flag,	'b', nil,
	"garbage_collection",&gCollection_flag,		'b', nil,

	"index_params",		&indexParams_flag,		'3', nil,
	"float_display_precision",&floatDisplayPrecision_flag,'i', nil,
	"sys_trace",		&sysTrace_flag,			'3', SysStraceUpdateFlag,

	"test_rellocation",	&testRelocation_flag,	'b', TestRelocationUpdateFlags,
	"test_garbage_collection",
						&testGCollection_flag,	'b', TestGCUpdateFlags,

	"@force_visibility",&forceVisibility_flag,	'b', nil,
	"@local_operators",	&localOperators_flag,	'b', nil,
	nil
} ;

static FlagEntryPt FindFlag(CharPt name)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( EqualStr(f->name, name) )
			return f ;
	return DatabaseError("Unknown flag: '%s'", name) ;
}

static void SetFlag(CharPt s, Pt t)		/* t already derref */
{
	register FlagEntryPt f = FindFlag(s) ;
	int newValue ;
	switch( f->type ) {
		case 'b':
			newValue = XTestOnOff(t) ;
			break ;
		case 'i':
			newValue = XTestIntRange(t, -100, 100) ;
			break ;
		case 'n':
			newValue = XTestNat(t) ;
			break ;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			newValue = XTestIntRange(t, 0, f->type - '0') ;
			break ;
		default: InternalError("SetFlag") ;
	}
	if( f->updateProc == nil )
		*(f->flag) = newValue ;
	else (f->updateProc)(newValue) ;
}

static Pt GetFlag(CharPt s)
{
	register FlagEntryPt f = FindFlag(s) ;
	switch( f->type ) {
		case 'b':
			return *(f->flag) ? tOnAtom : tOffAtom ;
		case 'i':
		case 'n':
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			return MakeInt(*(f->flag)) ;
		default: return InternalError("GetFlag") ;
	}
}

static void FlagsShow()
{
	register FlagEntryPt f ;
	VersionShow() ;
	Write("Flags:\n") ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( f->name[0] != '@' ) {
			Write("%25s ", f->name) ;
			switch( f->type ) {
				case 'b':
					Write("[onf]:") ;
					Write("%s\n", *(f->flag) ? "on" : "off") ;
					break ;
				case 'i':
					Write("[int]:") ;
					Write("%d\n", *(f->flag)) ;
					break ;
				case 'n':
					Write("[nat]:") ;
					Write("%d\n", *(f->flag)) ;
					break ;
				case '0': case '1': case '2': case '3': case '4':
				case '5': case '6': case '7': case '8': case '9':
					Write("[0-%c]:", f->type) ;
					Write("%d\n", *(f->flag)) ;
					break ;
				default: InternalError("FlagsShow") ;
			}		
		}
}


/* CXPROLOG C'BUILTINS */

static void PFlag2()
{
	CharPt fl = XTestAtomName(X0) ;
	MustBe( UnifyWithAtomic(X1, GetFlag(fl)) )
}	

static void PFlag3()
{
	CharPt fl = XTestAtomName(X0) ;
	Ensure( UnifyWithAtomic(X1, GetFlag(fl)) )
	SetFlag(fl, Drf(X2)) ;
	JumpNext()	
}	

static void PFlags()
{
	FlagsShow() ;
	JumpNext()
}

void FlagsRestart()
{
	reconsulting_flag = 0 ;
}

void FlagsInit()
{
	InstallCBuiltinPred("flag", 2, PFlag2) ;
	InstallCBuiltinPred("flag", 3, PFlag3) ;
	InstallCBuiltinPred("flags", 0, PFlags) ;
}
