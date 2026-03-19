/*
 *   This file is part of the CxProlog system

 *   Flags.c
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2009 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define DEVELOPMENT		0

#if USE_JAVA
#define jDft	1
#else
#define jDft	0
#endif

#if USE_WXWIDGETS
#define wxDft	1
#else
#define wxDft	0
#endif

#if COMPASS
#define ouDft	1
#define ceDft	0
#else
#define ouDft	0
#define ceDft	1
#endif

#if CONTEXTS == 3
#define svDft	3
#else
#define svDft	1
#endif

int
	debug_flag = 0,
	onInterrupt_flag = 4,
	occursCheck_flag = 0,			/* 0 = off */
	keepSource_flag = true,
	floatDisplayPrec_flag = 10,
	integerRoundingFunction_flag = CheckRoundTowardZero(),
	bounded_flag = true,
	maxInteger_flag = maxInt,
	minInteger_flag = minInt,
	maxArity_flag = maxFunctorArity,
	eofCode_flag = -1,
	gCollection_flag = 1,			/* 1 */
	nIndexable_flag = 1,

	compatibleIfThen_flag = true,
#if COMPASS
	compatibleStrings_flag = true,
#endif
	barIsSemicolon_flag = false,
	extraSpacesInTerms_flag = false,
	forceQuoted_flag = false,
	characterEscapes_flag = ceDft,
	doubleQuotes_flag = 1,
	charConversion_flag = 0,
	fileNameVariables_flag = 0,		/* 0 */

	onError_flag = 0,
	unknown_flag = 1,				/* 1 == fail */
	infoMessages_flag = 1,			/* 1 */

	javaAvailable_flag = jDft,
	wxWidgetsAvailable_flag = wxDft,

	sysTrace_flag = 0,				/* 0 */
#if DEVELOPMENT
	testRelocation_flag = 1,		/* 0 */
	testGCollection_flag = 1,		/* 0 */
#else
	testRelocation_flag = 0,		/* 0 */
	testGCollection_flag = 0,		/* 0 */
#endif

	allDynamic_flag = 0,			/* 0 */
	oldUpdate_flag = ouDft,

/* invisible */
	memoryWarnings_flag = false,	/* false */
	oldFashionedSeeing_flag = false,
	forceVisibility_flag = false,	/* false */
	semanticVariant_flag = svDft,
	localOperators_flag = false,

/* invisible obsolete */
	undefWarnings_flag = false ;

static int ignore = 0 ;

int
	stacks_debugging = 0,
	trail_debugging = 0,
	scratch_debugging = 0,
	index_debugging = 0,
	gc_debugging = 0,
	sysTrace_debugging = 0,
	testRelocation_debugging = 0,
	mesg_debugging = 0 ;


/* FLAG TYPES */

#define maxFlagTypeValues	7

typedef struct {
	Char name ;
	CharPt values[maxFlagTypeValues] ;
} FlagType, *FlagTypePt ;

static FlagType flagTypeTable[] =
{
	{'a', {"off", "dynamic", "dynamic_iu", nil}},
#if COMPASS
	{'b', {"off", "on", nil}},
#else
	{'b', {"false", "true", nil}},
#endif
	{'c', {"off", "active", "exception", nil}},
	{'d', {"off", "debug", "trace", nil}},
	{'e', {"error", "fail", "warning", nil}},
	{'f', {"off", "on", nil}},
	{'i', {"off", "debug", "trace", "abort", "menu", nil}},
	{'m', {"silent", "normal", "memory", "most", "all", nil}},
	{'n', {"exception", "message", "fail", "hook", nil}},
	{'o', {"false", "true", "exception", nil}},
	{'q', {"chars", "codes", "atom", "struct", nil}},
	{'r', {"down", "toward_zero", nil}},
	{'s', {"off", "messages", "built_ins", "all_preds", "detailed", nil}},
	{'\0'}
} ;

static FlagTypePt FlagTypeGet(Char ftype)
{
	FlagTypePt ft ;
	for( ft = flagTypeTable ; ft->name != '\0' ; ft++ )
		if( ft->name == ftype )
			return ft ;
	return InternalError("FlagTypeGet") ;
}


/* FLAG VALUES */

typedef struct {
	CharPt name ;
	int *flag ;
	int type ;
	int dflt ;
	FunI updateProc ;
	Bool invisible ;
	Bool readOnly ;
} FlagEntry, *FlagEntryPt ;

static FlagEntry flagTable[] =
{
	{"debug",				&debug_flag,			'd', 0, DebugUpdateFlags},
	{"on_interrupt",		&onInterrupt_flag,		'i', 4, nil},
	{"occurs_check",		&occursCheck_flag,		'c', 0, nil},
	{"keep_source",			&keepSource_flag,		'b', 1, nil},
	{"float_display_precision",
							&floatDisplayPrec_flag,	'I', 10, FloatDisplayPrecUpdateFlag},
	{"integer_rounding_function",
					&integerRoundingFunction_flag,	'r', CheckRoundTowardZero(), nil, false, true},
	{"bounded",				&bounded_flag,			'b', 1, nil, false, true},
	{"max_integer",			&maxInteger_flag,		'I', maxInt, nil, false, true},
	{"min_integer",			&minInteger_flag,		'I', minInt, nil, false, true},
	{"max_arity",			&maxArity_flag,			'I', maxFunctorArity, nil, false, true},
	{"eof_code",			&eofCode_flag,			'I', -1, nil},
	{"garbage_collection",	&gCollection_flag,		'b', 1, nil},
	{"index_params",		&nIndexable_flag,		'3', 1, nil},
	{""},

	{"compatible_if_then",	&compatibleIfThen_flag,	'b', 1, CompatibleIfThenUpdateFlags},
#if COMPASS
	{"compatible_strings",  &compatibleStrings_flag,'b', 1, nil},
#endif
	{"bar_is_semicolon",	&barIsSemicolon_flag,	'b', 0, nil},
	{"extra_spaces_in_terms",
							&extraSpacesInTerms_flag,'b', 0, nil},
	{"force_quoted",		&forceQuoted_flag,		'b', 0, nil},
	{"character_escapes",	&characterEscapes_flag,	'b', ceDft, nil},
	{"double_quotes",		&doubleQuotes_flag,		'q', 1, nil},
	{"char_conversion",		&charConversion_flag,	'f', 0, nil},
	{"file_name_variables",	&fileNameVariables_flag,'b', 0, nil},
	{""},

	{"on_error",			&onError_flag,			'n', 0, nil},
	{"unknown",				&unknown_flag,			'e', 1, nil},
	{"info_messages",		&infoMessages_flag,		'm', 1, nil},
	{"encoding",			&ignore,				'X', 0, nil, false, false},
	{"encoding_os",			&ignore,				'X', 0, nil, false, true},
	{""},

	{"java_available",		&javaAvailable_flag,	'b', jDft, nil, false, true},
	{"wxwidgets_available",	&wxWidgetsAvailable_flag,'b',wxDft, nil, false, true},
	{""},

	{"sys_trace",			&sysTrace_flag,			's', 0, SysTraceUpdateFlag},
	{"test_relocation",		&testRelocation_flag,	'b', 0,TestRelocationUpdateFlag},
	{"test_garbage_collection",
							&testGCollection_flag,	'2', 0, TestGCUpdateFlag},
	{""},

	{"all_dynamic",			&allDynamic_flag,		'a', 0, nil},
	{"old_update",			&oldUpdate_flag,		'b', ouDft, nil},

/* invisible */
	{"memory_warnings",		&memoryWarnings_flag,	'b', 0, nil, true},
	{"old_fashioned_seeing",&oldFashionedSeeing_flag,'b', 0, nil, true},
	{"force_visibility",	&forceVisibility_flag,	'b', 0, nil, true},
	{"semantic_variant",	&semanticVariant_flag,	'4', svDft, SemanticVarianteUpdateFlag, true},
	{"local_operators",		&localOperators_flag,	'b', 0, nil, true},
	{"toplevel_print_options",	&ignore,			'T', 0, nil, true},

/* invisible obsolete */
	{"fail_on_error",		&onError_flag,			'3', 0, nil, true},
	{"undef_warnings",		&undefWarnings_flag,	'b', 0, nil, true},

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

static void SetFlag(CharPt s, Pt t)	/* t already deref */
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
		case 'I':
			newValue = XTestIntRange(t, -100, 100) ;
			break ;
		case 'N':
			newValue = XTestNat(t) ;
			break ;
		case 'T':
			newValue = 0 ;
			break ;
		case 'X':
			if( StrEqual(f->name, "encoding") )
				SetDefaultEncoding(EncodingMake(XTestAtomName(t))) ;
			else InternalError("SetFlag") ;
			return ;
		default: {
			FlagTypePt ft = FlagTypeGet(f->type) ;
			if( IsInt(Drf(t)) )
				newValue = XTestIntRange(t, 0, StrSeqLength(ft->values)-1) ;
			else
				newValue = StrSeqGetIdx(XTestAtomName(t), ft->values) ;
			if( newValue == -1 )
				TypeError(StrSeqFormat("\'", "/", "\'", ft->values), t) ;
		}
	}
	if( f->updateProc == nil )
		*(f->flag) = newValue ;
	else (f->updateProc)(newValue) ;
}

static Pt GetFlag(CharPt s, Bool asInt)
{
	register FlagEntryPt f = FindFlag(s) ;
	switch( f->type ) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case 'I':
		case 'N':
			return MakeInt(*(f->flag)) ;
		case 'T':
			return tNilAtom ;
		case 'X':
			if( StrEqual(f->name, "encoding") )
				return MakeAtom(EncodingName(DefaultEncoding())) ;
			elif( StrEqual(f->name, "encoding_os") )
				return MakeAtom(EncodingName(SystemEncoding())) ;
			else return InternalError("GetFlag") ;
			break ;
		default: {
			FlagTypePt ft = FlagTypeGet(f->type) ;
			if( asInt )
				return MakeInt(*(f->flag)) ;
			else
				return MakeAtom(StrSeqGetVal(*(f->flag), ft->values)) ;
		}
	}
}

static void FlagsShow(void)
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
					Write("%-12d", *(f->flag)) ;
					Write(" | 0..%c", f->type) ;
					break ;
				case 'I':
					Write("%-12d", *(f->flag)) ;
					Write( " | -integer-") ;
					break ;
				case 'N':
					Write("%-12d", *(f->flag)) ;
					Write(" | -natural-" ) ;
					break ;
				case 'T':
					Write("term") ;
					break ;
				case 'X': {
					if( StrEqual(f->name, "encoding") )
						Write("%-12s | -encoding-", EncodingName(DefaultEncoding())) ;
					elif( StrEqual(f->name, "encoding_os") )
						Write("%-12s | -encoding-", EncodingName(SystemEncoding())) ;
					else InternalError("FlagsShow") ;
					break ;
				}
				default: {
					FlagTypePt ft = FlagTypeGet(f->type) ;
					Write("%-12s", StrSeqGetVal(*(f->flag), ft->values)) ;
					Write(" | %s", StrSeqFormat("", ", ", "", ft->values)) ;
				}
			}
			if( f->readOnly )
				Write(" [read-only]") ;
			Write("\n") ;
		}
}

void FlagsCheckDefaults(void)
{
#if DEVELOPMENT
	register FlagEntryPt f ;
	WriteErr("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n") ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( *f->name != '\0' && *(f->flag) != f->dflt )
			WriteErr("%%%%%% FLAG %s - default = %d, current = %d\n",
							f->name, f->dflt, *(f->flag)) ;
	WriteErr("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n") ;
#endif
}

static void SetDebuggingFlag(CharPt f)
{
	if(   StrEqual(f, "stacks") )   stacks_debugging = 1 ;
	elif( StrEqual(f, "trail") )	trail_debugging = 1 ;
	elif( StrEqual(f, "scratch") )  scratch_debugging = 1 ;
	elif( StrEqual(f, "index") )	index_debugging = 1 ;
	elif( StrEqual(f, "gc") )		gc_debugging = 1 ;
	elif( StrEqual(f, "silent") )	infoMessages_flag = 0 ;

	elif( StrEqual(f, "mesg") )		mesg_debugging = 1 ;
	elif( StrEqual(f, "systrace1"))	sysTrace_debugging = 1 ;
	elif( StrEqual(f, "systrace2"))	sysTrace_debugging = 2 ;
	elif( StrEqual(f, "systrace3"))	sysTrace_debugging = 3 ;
	elif( StrEqual(f, "systrace4"))	sysTrace_debugging = 4 ;
	elif( StrEqual(f, "systrace"))	sysTrace_debugging = 4 ;
	elif( StrEqual(f, "reloc") )	testRelocation_debugging = 1 ;
	elif( StrEqual(f, "help") )
		WriteStd("DEBUG OPTIONS: stacks,trail,scratch,index,gc,,,mesg,systrace,reloc\n\n") ;
}



/* CXPROLOG C'BUILTINS */

static void PFlag2()
{
	CharPt fl = XTestAtomName(X0) ;
	MustBe( UnifyWithAtomic(X1, GetFlag(fl, IsInt(Drf(X1)))) ) ;
}

static void PFlag3()
{
	CharPt fl = XTestAtomName(X0) ;
	Ensure( UnifyWithAtomic(X1, GetFlag(fl, IsInt(Drf(X1)))) ) ;
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
			A(3) = cPt(cC99Fix(flagTable)) ;
		}
		else {							/* atom handle */
			CharPt fl = XTestAtomName(X0) ;
			Discard() ;
			MustBe( UnifyWithAtomic(X1, GetFlag(fl, IsInt(Drf(X1)))) ) ;
		}
	}

	if( A(2) == zeroIntPt ) {			/* var handle */
		FlagEntryPt f = ((FlagEntryPt)A(3)) ;
		for( ; f->name != nil ; f++ )
			if( !f->invisible && f->name[0] != '\0' ) {
				A(3) = cPt(f+1) ;
				MustBe( UnifyWithAtomic(X0, MakeAtom(f->name))
					 && UnifyWithAtomic(X1, GetFlag(f->name, IsInt(Drf(X1)))) ) ;
			}
		Jump(DiscardAndFail) ;
	}
}

void FlagsSetDebugging(CharPt str)
{
	CharPt fl ;
	Char save ;
	if( str == nil || *str == '\0' ) return ;
	for( fl = str ; ; fl = ++str ) {
		while( *str != '\0' && *str != ',' ) str++ ;
		save = *str ;
		*str = '\0' ;
		SetDebuggingFlag(fl) ;
		*str = save ;
		if( *str == '\0' ) break ;
	}
}

void FlagsRestart()
{
	/* nothing */ ;
}

void FlagsInit()
{
	CallAllFlagUpdaters() ;
	FlagsCheckDefaults() ;

	InstallCBuiltinPred("flag", 2, PFlag2) ;
	InstallCBuiltinPred("flag", 3, PFlag3) ;
	InstallCBuiltinPred("flags", 0, PFlags) ;

	InstallCBuiltinPred("set_prolog_flag", 2, PSetPrologFlag) ;
	InstallGNDeterCBuiltinPred("current_prolog_flag", 2, 2, PCurrentPrologFlag) ;
}
