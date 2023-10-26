/*
 *   This file is part of the CxProlog system

 *   Flags.c
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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
	interrupt_flag = 4,

	failOnError_flag = 0,
	compatibleIfThen_flag = true,
	compatibleStrings_flag = true,
	barIsSemicolon_flag = true,
	extraSpacesInTerms_flag = false,
	eofCode_flag = -1,

	keepSource_flag = true,
	forceQuoted_flag = false,
	unknown_flag = 1,				/* 1 == fail */
	undefWarnings_flag = false,
	memoryWarnings_flag = false,	/* false */
	infoMessages_flag = true,
	indexParams_flag = 1,
	allDynamic_flag = 0,
	gCollection_flag = 1,			/* 1 */
	
	floatDisplayPrec_flag = 10,

	oldFashionedSeeing_flag = false,
#if COMPAT_0_90_3
	oldConsult_flag = true,
#else
	oldConsult_flag = false,
#endif

	testRelocation_flag = 0,
	testGCollection_flag = 0,
	sysTrace_flag = 0,

	forceVisibility_flag = false,
	localOperators_flag = false ;

/* Types:
		b - on, off
		c - chars, code, atom
		e - error, fail, warning
		i - -100..100
		2 - 0..2
		3 - 0..3
		4 - 0..4
*/

typedef struct {
	CharPt name ;
	int *flag ;
	int type ;
	FunI updateProc ;
} FlagEntry, *FlagEntryPt ;

static FlagEntry flagTable[] =
{
	{"debug",				&debug_flag,			'2', DebugUpdateFlags},
	{"interrupt",			&interrupt_flag,		'4', nil},
	{"fail_on_error",		&failOnError_flag,		'2', nil},
	{"compatible_if_then",	&compatibleIfThen_flag,	'b', CompatibleIfThenUpdateFlags},
	{"compatible_strings",	&compatibleStrings_flag,'b', nil},
	{"bar_is_semicolon",	&barIsSemicolon_flag,	'b', nil},
	{"extra_spaces_in_terms",
							&extraSpacesInTerms_flag,'b', nil},
	{"eof_code",			&eofCode_flag,			'i', nil},

	{"keep_source",			&keepSource_flag,		'b', nil},
	{"force_quoted",		&forceQuoted_flag,		'b', nil},
	{"unknown", 			&unknown_flag,			'e', nil},
	{"undef_warnings", 		&undefWarnings_flag, 	'b', nil},
	{"memory_warnings",		&memoryWarnings_flag,	'b', nil},
	{"info_messages",		&infoMessages_flag,		'b', nil},
	{"garbage_collection",	&gCollection_flag,		'b', nil},

	{"index_params",		&indexParams_flag,		'3', nil},
	{"all_dynamic",			&allDynamic_flag,		'2', nil},
	{"old_consult",			&oldConsult_flag,		'b', nil},
	{"float_display_precision",
							&floatDisplayPrec_flag,	'i', FloatDisplayPrecUpdateFlag},
	{"old_fashioned_seeing",&oldFashionedSeeing_flag,'b', nil},
	
	{"sys_trace",			&sysTrace_flag,			'3', SysTraceUpdateFlag},

	{"test_relocation",		&testRelocation_flag,	'b', TestRelocationUpdateFlags},
	{"test_garbage_collection",
							&testGCollection_flag,	'b', TestGCUpdateFlags},

	{"@force_visibility",	&forceVisibility_flag,	'b', nil},
	{"@local_operators",	&localOperators_flag,	'b', nil},
	{nil,					nil,					0,	nil}

} ;

static FlagEntryPt FindFlag(CharPt name)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( StrEqual(f->name, name) )
			return f ;
	return DatabaseError("Unknown flag: '%s'", name) ;
}

static void CallAllFlagUpdaters(void)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( f->updateProc != nil )
			(f->updateProc)(*f->flag) ;
}

static void SetFlag(CharPt s, Pt t)		/* t already deref */
{
	register FlagEntryPt f = FindFlag(s) ;
	int newValue ;
	switch( f->type ) {
		case 'b':
			newValue = XTestAtomAlt(t, "off", "on", nil) ;
			break ;
		case 'e':
			newValue = XTestAtomAlt(t, "error", "fail", "warning", nil) ;
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
		default:
			newValue = -1 ;
			InternalError("SetFlag") ;
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
			return MakeAtom(StrSeqGet(*(f->flag), "off", "on", nil)) ;
		case 'e':
			return MakeAtom(StrSeqGet(*(f->flag), "error", "fail", "warning", nil)) ;
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
	ShowVersion() ;
	Write("FLAGS:\n") ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( f->name[0] != '@' ) {
			Write("%25s: ", f->name) ;
			switch( f->type ) {
				case 'b':
					Write("%-10s", StrSeqGet(*(f->flag), "off", "on", nil)) ;
					Write(" | %s\n", StrSeqFormat("", ", ", "", "off", "on", nil)) ;
					break ;
				case 'e':
					Write("%-10s", StrSeqGet(*(f->flag), "error", "fail", "warning", nil)) ;
					Write(" | %s\n", StrSeqFormat("", ", ", "", "error", "fail", "warning", nil)) ;
					break ;
				case 'i':
					Write("%-10d", *(f->flag)) ;
					Write( " | INT\n") ;
					break ;
				case 'n':
					Write("%-10d", *(f->flag)) ;
					Write(" | NAT\n" ) ;
					break ;
				case '0': case '1': case '2': case '3': case '4':
				case '5': case '6': case '7': case '8': case '9':
					Write("%-10d", *(f->flag)) ;
					Write(" | 0..%c\n", f->type) ;
					break ;
				default: InternalError("FlagsShow") ;
			}		
		}
}


/* CXPROLOG C'BUILTINS */

static void PFlag2()
{
	CharPt fl = XTestAtomName(X0) ;
	MustBe( UnifyWithAtomic(X1, GetFlag(fl)) ) ;
}	

static void PFlag3()
{
	CharPt fl = XTestAtomName(X0) ;
	Ensure( UnifyWithAtomic(X1, GetFlag(fl)) ) ;
	SetFlag(fl, X2) ;
	JumpNext() ;	
}	

static void PFlags()
{
	FlagsShow() ;
	JumpNext() ;
}

static void PSetPrologFlag()
{
	CharPt fl = XTestAtomName(X0) ;
	SetFlag(fl, X1) ;
	JumpNext() ;
}

static void PCurrentPrologFlag()
{
	if( A(2) == tNilAtom ) {			/* init */
		X0 = Drf(X0) ;
		if( IsVar(X0) ) {				/* var init */
			A(2) = zeroIntPt ;
			A(3) = cPt(flagTable) ;
		}
		else {							/* atom handle */
			CharPt fl = XTestAtomName(X0) ;
			Discard() ;
			MustBe( UnifyWithAtomic(X1, GetFlag(fl)) ) ;
		}
	}

	if( A(2) == zeroIntPt ) {			/* var handle */
		FlagEntryPt f = ((FlagEntryPt)A(3)) ;
		for( ; f->name != nil ; f++ )
			if( f->name[0] != '@' ) {
				A(3) = cPt(f+1) ;
				MustBe( UnifyWithAtomic(X0, MakeAtom(f->name))
					 && UnifyWithAtomic(X1, GetFlag(f->name)) ) ;
			}
		Jump(DiscardAndFail) ;
	}
}

void FlagsRestart()
{
	/* nothing */ ;
}

void FlagsInit()
{
	CallAllFlagUpdaters() ;
	InstallCBuiltinPred("flag", 2, PFlag2) ;
	InstallCBuiltinPred("flag", 3, PFlag3) ;
	InstallCBuiltinPred("flags", 0, PFlags) ;

	InstallCBuiltinPred("set_prolog_flag", 2, PSetPrologFlag) ;
	InstallGNDeterCBuiltinPred("current_prolog_flag", 2, 2, PCurrentPrologFlag) ;
}