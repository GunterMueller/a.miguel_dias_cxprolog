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
	occursCheck_flag = 0,			/* 0 = false */
	keepSource_flag = true,
	floatDisplayPrec_flag = 10,
	eofCode_flag = -1,
	gCollection_flag = 1,			/* 1 */
	indexParams_flag = 1,

	compatibleIfThen_flag = true,
	compatibleStrings_flag = true,
	barIsSemicolon_flag = true,
	extraSpacesInTerms_flag = false,
	forceQuoted_flag = false,
	fileNameVariables_flag = 0,		/* 0 */

	unknown_flag = 1,				/* 1 == fail */
	undefWarnings_flag = false,
	memoryWarnings_flag = false,	/* false */
	infoMessages_flag = true,		/* true */
	
#if USE_JAVA
	javaAvailable_flag = true,
#else
	javaAvailable_flag = false,
#endif
#if USE_WXWIDGETS
	wxWidgetsAvailable_flag = true,
#else
	wxWidgetsAvailable_flag = false,
#endif

	sysTrace_flag = 0,				/* 0 */
	testRelocation_flag = 0,		/* 0 */
	testGCollection_flag = 0,		/* 0 */

	allDynamic_flag = 0,			/* 0 */
#if COMPAT_0_90_3
	oldConsult_flag = true,
#else
	oldConsult_flag = false,
#endif
	oldFashionedSeeing_flag = false,
	forceVisibility_flag = false,	/* false */
#if CONTEXTS == 3
	semanticVariant_flag = 3,		/* 3 */
#else
	semanticVariant_flag = 1,		/* 1 */
#endif
	localOperators_flag = false ;

static int ignore = 0 ;

/* Types:
		b - false, true
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
	Bool invisible ;
	Bool readOnly ;
} FlagEntry, *FlagEntryPt ;
	
static FlagEntry flagTable[] =
{
	{"debug",				&debug_flag,			'2', DebugUpdateFlags},
	{"interrupt",			&interrupt_flag,		'4', nil},
	{"occurs_check",		&occursCheck_flag,		'o', nil},
	{"keep_source",			&keepSource_flag,		'b', nil},
	{"float_display_precision",
							&floatDisplayPrec_flag,	'i', FloatDisplayPrecUpdateFlag},
	{"eof_code",			&eofCode_flag,			'i', nil},
	{"garbage_collection",	&gCollection_flag,		'b', nil},
	{"index_params",		&indexParams_flag,		'3', nil},
	{""},

	{"compatible_if_then",	&compatibleIfThen_flag,	'b', CompatibleIfThenUpdateFlags},
	{"compatible_strings",	&compatibleStrings_flag,'b', nil},
	{"bar_is_semicolon",	&barIsSemicolon_flag,	'b', nil},
	{"extra_spaces_in_terms",
							&extraSpacesInTerms_flag,'b', nil},
	{"force_quoted",		&forceQuoted_flag,		'b', nil},
	{"file_name_variables",	&fileNameVariables_flag,'b', nil},
	{""},

	{"fail_on_error",		&failOnError_flag,		'2', nil},
	{"unknown", 			&unknown_flag,			'e', nil},
	{"undef_warnings", 		&undefWarnings_flag, 	'b', nil},
	{"memory_warnings",		&memoryWarnings_flag,	'b', nil},
	{"info_messages",		&infoMessages_flag,		'b', nil},
	{""},

	{"java_available",		&javaAvailable_flag,	'b', nil, false, true},
	{"wxwidgets_available",	&wxWidgetsAvailable_flag,'b', nil, false, true},
	{""},
	
	{"sys_trace",			&sysTrace_flag,			'3', SysTraceUpdateFlag},

	{"test_relocation",		&testRelocation_flag,	'b', TestRelocationUpdateFlags},
	{"test_garbage_collection",
							&testGCollection_flag,	'b', TestGCUpdateFlags},
	{""},

	{"all_dynamic",			&allDynamic_flag,		'2', nil},
	{"old_consult",			&oldConsult_flag,		'b', nil},
	{"old_fashioned_seeing",&oldFashionedSeeing_flag,'b', nil},
	{"force_visibility",	&forceVisibility_flag,	'b', nil, true},
	{"semantic_variant",	&semanticVariant_flag,	'4', SemanticVarianteUpdateFlag, true},
	{"local_operators",		&localOperators_flag,	'b', nil, true},
	{"toplevel_print_options",	&ignore,			't', nil, true},
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

static void SetFlag(CharPt s, Pt t, Bool useBool)	/* t already deref */
{
	register FlagEntryPt f = FindFlag(s) ;
	int newValue ;
	if( f->readOnly )
		DatabaseError("Read-only flag '%s' cannot be changed", f->name) ;
	switch( f->type ) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			newValue = XTestIntRange(t, 0, f->type - '0') ;
			break ;
		case 'b':
			if( useBool )
				newValue = XTestAtomAlt(t, "false", "true", nil) ;
			else	/* for retro-compatibility */
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
		case 'o':
				newValue = XTestAtomAlt(t, "false", "true", "exception", nil) ;
			break ;
		case 't':
			newValue = 0 ;
			break ;
		default:
			newValue = -1 ;
			InternalError("SetFlag") ;
	}
	if( f->updateProc == nil )
		*(f->flag) = newValue ;
	else (f->updateProc)(newValue) ;
}

static Pt GetFlag(CharPt s, Bool useBool)
{
	register FlagEntryPt f = FindFlag(s) ;
	switch( f->type ) {
		case 'b':
			if( useBool )
				return MakeAtom(StrSeqGet(*(f->flag), "false", "true", nil)) ;
			else	/* for retro-compatibility */
				return MakeAtom(StrSeqGet(*(f->flag), "off", "on", nil)) ;
		case 'e':
			return MakeAtom(StrSeqGet(*(f->flag), "error", "fail", "warning", nil)) ;
		case 'i':
		case 'n':
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			return MakeInt(*(f->flag)) ;
		case 't':
			return tNilAtom ;
		default: return InternalError("GetFlag") ;
	}
}

static void FlagsShow()
{
	register FlagEntryPt f ;
	ShowVersion() ;
	Write("FLAGS:\n") ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( f->invisible )
			/* Nothing */ ;
		elif( *f->name == '\0' )
			Write("\n") ;
		else {
			Write("%25s: ", f->name) ;
			switch( f->type ) {
				case '0': case '1': case '2': case '3': case '4':
				case '5': case '6': case '7': case '8': case '9':
					Write("%-10d", *(f->flag)) ;
					Write(" | 0..%c", f->type) ;
					break ;
				case 'b':
					Write("%-10s", StrSeqGet(*(f->flag), "false", "true", nil)) ;
					Write(" | %s", StrSeqFormat("", ", ", "", "false", "true", nil)) ;
					break ;
				case 'e':
					Write("%-10s", StrSeqGet(*(f->flag), "error", "fail", "warning", nil)) ;
					Write(" | %s", StrSeqFormat("", ", ", "", "error", "fail", "warning", nil)) ;
					break ;
				case 'i':
					Write("%-10d", *(f->flag)) ;
					Write( " | INT") ;
					break ;
				case 'n':
					Write("%-10d", *(f->flag)) ;
					Write(" | NAT" ) ;
					break ;
				case 'o':
					Write("%-10s", StrSeqGet(*(f->flag), "false", "true", "exception", nil)) ;
					Write(" | %s", StrSeqFormat("", ", ", "", "false", "true", "exception", nil)) ;
					break ;
				case 't':
					Write("term") ;
					break ;
				default: InternalError("FlagsShow") ;
			}
			if( f->readOnly )		
				Write(" [read-only]") ;
			Write("\n") ;
		}
}


/* CXPROLOG C'BUILTINS */

static void PFlag2()
{
	CharPt fl = XTestAtomName(X0) ;
	MustBe( UnifyWithAtomic(X1, GetFlag(fl,false)) ) ;
}	

static void PFlag3()
{
	CharPt fl = XTestAtomName(X0) ;
	Ensure( UnifyWithAtomic(X1, GetFlag(fl, false)) ) ;
	SetFlag(fl, X2, false) ;
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
	SetFlag(fl, X1, true) ;
	JumpNext() ;
}

static void PCurrentPrologFlag()
{
	if( A(2) == tNilAtom ) {			/* init */
		X0 = Drf(X0) ;
		if( IsVar(X0) ) {				/* var init */
			A(2) = zeroIntPt ;
			A(3) = cPt(cC99Fix(flagTable)) ;
		}
		else {							/* atom handle */
			CharPt fl = XTestAtomName(X0) ;
			Discard() ;
			MustBe( UnifyWithAtomic(X1, GetFlag(fl, true)) ) ;
		}
	}

	if( A(2) == zeroIntPt ) {			/* var handle */
		FlagEntryPt f = ((FlagEntryPt)A(3)) ;
		for( ; f->name != nil ; f++ )
			if( f->name[0] != '@' ) {
				A(3) = cPt(f+1) ;
				MustBe( UnifyWithAtomic(X0, MakeAtom(f->name))
					 && UnifyWithAtomic(X1, GetFlag(f->name, true)) ) ;
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

	if( testRelocation_flag )
		WriteStd("*** STRESS-TESTING RELLOCATION ***\n") ;
	if( testGCollection_flag )
		WriteStd("*** STRESS-TESTING GARBAGE COLLECTION ***\n") ;
	if( sysTrace_flag > 0 )
		WriteStd("*** SYSTRACE LEVEL IS %d ***\n", sysTrace_flag) ;
}
