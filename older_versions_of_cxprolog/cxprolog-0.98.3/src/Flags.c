/*
 *   This file is part of the CxProlog system

 *   Flags.c
 *   by A.Miguel Dias - 2000/08/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define DEVELOP		0

#if USE_JAVA
#define jDft		1
#else
#define jDft		0
#endif

#if USE_WXWIDGETS
#define wxDft		1
#else
#define wxDft		0
#endif

#if COMPASS
#define ouDft		1
#define ceDft		0
#define ieDft		0
#else
#define ouDft		0
#define ceDft		1
#define ieDft		1
#endif

#if CONTEXTS == 3
#define svDft		3
#else
#define svDft		1
#endif

#define CheckRoundTowardZero()	(true)

PInt
	debug_flag = 0,
	onInterrupt_flag = 4,
	occursCheck_flag = 0,			/* 0 = off */
	keepSource_flag = true,
	floatDisplayPrec_flag = 10,
	integerRoundingFunction_flag = CheckRoundTowardZero(),
	arithmetic_checking_flag = true,
	iso_flag = false,
	bounded_flag = true,
	maxInteger_flag = maxInt,
	minInteger_flag = minInt,
	maxArity_flag = maxFunctorArity,
	addressBits_flag = addr_bits,
	eofCode_flag = -1,
	gCollection_flag = 1,			/* 1 */
	nIndexable_flag = 1,

	compatibleIfThen_flag = true,
#if COMPASS
	compatibleStrings_flag = true,
#endif
	nilIsSpecial_flag = false,		/* false */
	barIsSemicolon_flag = true,
	cutIsSolo_flag = true,
	extraSpacesInTerms_flag = false,
	forceQuoted_flag = false,
	characterEscapes_flag = ceDft,
	doubleQuotes_flag = 1,
	charConversion_flag = 0,
	fileNameVariables_flag = 0,		/* 0 */

	onError_flag = 0,
	unknown_flag = 0,				/* 1 == fail */
	infoMessages_flag = DEVELOP ? 3 : 1, /* 1 */

	javaAvailable_flag = jDft,
	wxWidgetsAvailable_flag = wxDft,

	sysTrace_flag = 0,				/* 0 */
	testRelocation_flag = DEVELOP,	/* 0 */
	testGCollection_flag = DEVELOP,	/* 0 */
	testInvariants_flag = DEVELOP,	/* 0 */

	allDynamic_flag = 0,			/* 0 */
	oldUpdate_flag = ouDft,
	isoErrors_flag = ieDft,

/* invisible */
	memoryWarnings_flag = false,	/* false */
	oldFashionedSeeing_flag = false,
	forceVisibility_flag = false,	/* false */
	semanticVariant_flag = svDft,
	localOperators_flag = false,

/* invisible obsolete */
	undefWarnings_flag = false ;

static PInt ignore = 0 ;

PInt
	stacksDebugging_flag = 0,
	trailDebugging_flag = 0,
	scratchDebugging_flag = 0,
	indexDebugging_flag = 0,
	gcDebugging_flag = 0,
	sysTraceDebugging_flag = 0,
	testRelocationDebugging_flag = 0,
	testInvariantsDebugging_flag = 0,
	infoMessagesDebugging_flag = 0 ;


/* FLAG TYPES */

#define maxFlagTypeValues	7

typedef struct {
	Char name ;
	Str values[maxFlagTypeValues] ;
} FlagType, *FlagTypePt ;

static FlagType flagTypeTable[] =
{
	{'a', {"off", "dynamic", "dynamic_iu", nil}},
#if COMPASS
	{'b', {"off", "on", nil}},
#else
	{'b', {"false", "true", nil}},
#endif
	{'c', {"false", "true", "error", nil}},
	{'d', {"off", "debug", "trace", nil}},
	{'e', {"error", "fail", "warning", nil}},
	{'f', {"off", "on", nil}},
	{'i', {"off", "debug", "trace", "abort", "menu", nil}},
	{'m', {"silent", "normal", "memory", "most", "all", nil}},
	{'n', {"exception", "message", "fail", "hook", nil}},
	{'q', {"chars", "codes", "atom", "struct", nil}},
	{'r', {"down", "toward_zero", nil}},
	{'s', {"off", "messages", "built_ins", "all_preds", "detailed", nil}},
	{'\0', {}}
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
	Str name ;
	PInt *flag ;
	int type ;
	PInt dflt ;
	FunI updateProc ;
	Bool invisible ;
	Bool3 readOnly ;
	PInt store ;	
} FlagEntry, *FlagEntryPt ;

static FlagEntry flagTable[] =	 
{
/*       name                          flag                         type   dflt                 updateProc        invisible   readOnly  */
	{"debug",						&debug_flag,					'd',	0,				DebugUpdateFlag,		false,		false3,	0 },
	{"on_interrupt",				&onInterrupt_flag,				'i',	4,						nil,			false,		false3,	0 },
	{"occurs_check",				&occursCheck_flag,				'c',	0,						nil,			false,		false3,	0 },
	{"keep_source",					&keepSource_flag,				'b',	1,						nil,			false,		false3,	0 },
	{"float_display_precision",		&floatDisplayPrec_flag,			'I',	10,			FloatDisplayPrecUpdateFlag,	false,		false3,	0 },
	{"integer_rounding_function",	&integerRoundingFunction_flag,	'r',	CheckRoundTowardZero(),	nil,			false,		true3,	0 },
	{"arithmetic_checking",			&arithmetic_checking_flag,		'b',	1,						nil,			false,		false3,	0 },
	{"iso",							&iso_flag,						'b',	0,						nil,			false,		false3,	0 },
	{"bounded",						&bounded_flag,					'b',	1,						nil,			false,		true3,	0 },
	{"max_integer",					&maxInteger_flag,				'I',	maxInt,					nil,			false,		true3,	0 },
	{"min_integer",					&minInteger_flag,				'I',	minInt,					nil,			false,		true3,	0 },
	{"max_arity",					&maxArity_flag,					'I',	maxFunctorArity,		nil,			false,		true3,	0 },
	{"address_bits",				&addressBits_flag,				'I',	addr_bits,				nil,			false,		true3,	0 },
	{"eof_code",					&eofCode_flag,					'I',	-1,						nil,			false,		false3,	0 },
	{"garbage_collection",			&gCollection_flag,				'b',	1,				GCollectionUpdateFlag,	false,		false3,	0 },
	{"index_params",				&nIndexable_flag,				'3',	1,				NIndexableUpdateFlag,	false,		false3,	0 },
//	{"",&ignore,0,0,0,0,true3,0},

	{"compatible_if_then",			&compatibleIfThen_flag,			'b',	1,		CompatibleIfThenUpdateFlag,		false,		false3,	0 },
#if COMPASS
	{"compatible_strings",			&compatibleStrings_flag,		'b',	1,						nil,			false,		false3,	0 },
#endif
	{"nil_is_special",				&nilIsSpecial_flag,				'b',	0,						nil,			false,		undefined3,	0 },
	{"bar_is_semicolon",			&barIsSemicolon_flag,			'b',	1,						nil,			false,		false3,	0 },
	{"cut_is_solo",					&cutIsSolo_flag,				'b',	1,				CutIsSoloUpdateFlag,	false,		false3,	0 },
	{"extra_spaces_in_terms",
									&extraSpacesInTerms_flag,		'b',	0,						nil,			false,		false3,	0 },
	{"force_quoted",				&forceQuoted_flag,				'b',	0,						nil,			false,		false3,	0 },
	{"character_escapes",			&characterEscapes_flag,			'b',	ceDft,					nil,			false,		false3,	0 },
	{"double_quotes",				&doubleQuotes_flag,				'q',	1,						nil,			false,		false3,	0 },
	{"char_conversion",				&charConversion_flag,			'b',	0,						nil,			false,		false3,	0 },
	{"file_name_variables",			&fileNameVariables_flag,		'b',	0,						nil,			false,		false3,	0 },
//	{"",&ignore,0,0,0,0,true3,0},

	{"on_error",					&onError_flag,					'n',	0,						nil,			false,		false3,	0 },
	{"unknown",						&unknown_flag,					'e',	0,						nil,			false,		false3,	0 },  
		
	{"info_messages",				&infoMessages_flag,				'm',	1,			InfoMessagesUpdateFlag,		false,		false3,	0 },
	{"encoding",					&ignore,						'X',	0,						nil,			false,		false3,	0 },
	{"encoding_os",					&ignore,						'X',	0,						nil,			false,		true3,	0 },
//	{"",&ignore,0,0,0,0,true3,0},

	{"dialect",						&ignore,						'X',	0,						nil,			false,		true3,	0 },
	{"version",						&ignore,						'X',	0,						nil,			false,		true3,	0 },
	{"version_data",				&ignore,						'X',	0,						nil,			false,		true3,	0 },
	{"argv",						&ignore,						'X',	0,						nil,			false,		true3,	0 },
//	{"",&ignore,0,0,0,0,true3,0},

	{"java_available",				&javaAvailable_flag,			'b',	jDft,					nil,			false,		true3,	0 },
	{"wxwidgets_available",			&wxWidgetsAvailable_flag,		'b',	wxDft,					nil,			false,		true3,	0 },
//	{"",&ignore,0,0,0,0,true3,0},

	{"sys_trace",					&sysTrace_flag,					's',	0,				SysTraceUpdateFlag,		false,		false3,	0 },
	{"test_relocation",				&testRelocation_flag,			'b',	0,			TestRelocationUpdateFlag,	false,		false3,	0 },
	{"test_garbage_collection",		&testGCollection_flag,			'2',	0,				TestGCUpdateFlag,		false,		false3,	0 },
	{"test_invariants",				&testInvariants_flag,			'b',	0,				TestInvariantsFlag,		false,		false3,	0 },
//	{"",&ignore,0,0,0,0,true3,0},

	{"all_dynamic",					&allDynamic_flag,				'a',	0,						nil,			false,		false3,	0 },
	{"old_update",					&oldUpdate_flag,				'b',	ouDft,					nil,			false,		false3,	0 },

/* invisible */
	{"iso_errors",					&isoErrors_flag,				'b',	ieDft,					nil,			true,		false3,	0 },
	{"memory_warnings",				&memoryWarnings_flag,			'b',	0,						nil,			true,		false3,	0 },
	{"old_fashioned_seeing",		&oldFashionedSeeing_flag,		'b',	0,						nil,			true,		false3,	0 },
	{"force_visibility",			&forceVisibility_flag,			'b',	0,						nil,			true,		false3,	0 },
	{"semantic_variant",			&semanticVariant_flag,			'4',	svDft,		SemanticVarianteUpdateFlag,	true,		false3,	0 },
	{"local_operators",				&localOperators_flag,			'b',	0,						nil,			true,		false3,	0 },
	{"toplevel_print_options",		&ignore,						'T',	0,						nil,			true,		false3,	0 },

/* invisible obsolete */
	{"fail_on_error",				&onError_flag,					'3',	0,						nil,			true,		false3,	0 },
	{"undef_warnings",				&undefWarnings_flag,			'b',	0,						nil,			true,		false3,	0 },

	{nil,0,0,0,0,0,0,0}
} ;

static Bool userMode = false ;

static FlagEntryPt FindFlag(Str name)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		if( StrEqual(f->name, name) )
			return f ;
	return DatabaseError("Unknown flag: '%s'", name) ;
}

static void FlagsEnterUserMode(void)
{
	register FlagEntryPt f ;
	userMode = true ;
	for( f = flagTable ; f->name != nil ; f++ ) {
#if DEVELOP
		if( *f->flag != f->dflt )
			WriteErr("%%%%%% FLAG %s - default = %ld, current = %ld\n",
							f->name, f->dflt, *f->flag) ;
#endif
		if( f->updateProc != nil )
			(f->updateProc)(*f->flag) ;
	}
}

static void FlagUpdate(FlagEntryPt f, PInt value)
{
	if( userMode ) {
		if( f->readOnly == false3 && *f->flag != value ) {
			if( f->updateProc != nil )
				(f->updateProc)(value) ;
			else
				*f->flag = value ;
		}
	}
	else
		*f->flag = value ;
}

static void FlagsStoreDefaultValues(void)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		f->store = f->dflt ;
}

static void FlagsStoreCurrentValues(void)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		f->store = *f->flag ;
}
	
static void FlagsRestoreStoredValues(void)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		FlagUpdate(f, f->store) ;
}
	
static void FlagsForceDefaults(void)
{
	register FlagEntryPt f ;
	for( f = flagTable ; f->name != nil ; f++ )
		FlagUpdate(f, f->dflt);
}

static void PreSetFlagValue(FlagEntryPt f, Str arg)
{
	int newValue = InRange(arg[0], '0', '9') ? atoi(arg) : -1 ;
	if( f->readOnly == true3 )
		DatabaseError("The flag '%s' cannot be changed", f->name) ;
	switch( f->type ) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			if( !InRange(newValue, 0, f->type - '0') )
				TypeError(GStrFormat("(%s) RANGE(%d..%d)", f->name,
											0, f->type - '0'), nil) ;
			break ;
		case 'I':
			if( !InRange(newValue, -100, 100) )
				TypeError(GStrFormat("(%s) RANGE(%d..%d)", f->name, -100, 100), nil) ;
			break ;
		case 'N':
			if( !IsNat(newValue) )
				TypeError(GStrFormat("(%s) INT>=0", f->name), nil) ;
			break ;
		case 'T':
			newValue = 0 ;
			break ;
		case 'X':
			Error("The flag '%s' cannot be setup by this feature", f->name) ;
			return ;
		default: {
			FlagTypePt ft = FlagTypeGet(f->type) ;
			newValue = StrSeqGetIdx(arg, ft->values) ;
			if( newValue == -1 )
				TypeError(GStrFormat("(%s) %s", f->name,
							StrSeqFormat("\'", "/", "\'", ft->values)), nil) ;
		}
	}
	FlagUpdate(f, newValue) ;
}

static void SetFlagValue(FlagEntryPt f, Pt t)	/* t already deref */
{
	int newValue ;
	if( f->readOnly != false3 )
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
			else InternalError("SetFlagValue") ;
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
	FlagUpdate(f, newValue) ;
}

static Pt GetFlagValue(FlagEntryPt f, Bool asInt)
{
	switch( f->type ) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case 'I':
		case 'N':
			return MakeInt(*f->flag) ;
		case 'T':
			return tNilAtom ;
		case 'X':
			if( StrEqual(f->name, "encoding") )
				return MakeAtom(EncodingName(DefaultEncoding())) ;
			elif( StrEqual(f->name, "encoding_os") )
				return MakeAtom(EncodingName(SystemEncoding())) ;
			elif( StrEqual(f->name, "version_data") )
				return VersionTerm() ;
			elif( StrEqual(f->name, "version") )
				return MakeInt(VersionGet()) ;
			elif( StrEqual(f->name, "argv") )
				return ZOSGetArgs() ;
			elif( StrEqual(f->name, "dialect") )
				return MakeAtom("cx") ;
			else return InternalError("GetFlagValue") ;
			break ;
		default: {
			FlagTypePt ft = FlagTypeGet(f->type) ;
			if( asInt )
				return MakeInt(*f->flag) ;
			else
				return MakeAtom(StrSeqGetVal(*f->flag, ft->values)) ;
		}
	}
}

static void SetDebuggingFlag(Str f)
{
	if(   StrEqual(f, "stacks") )		stacksDebugging_flag = 1 ;
	elif( StrEqual(f, "trail") )		trailDebugging_flag = 1 ;
	elif( StrEqual(f, "scratch") )		scratchDebugging_flag = 1 ;
	elif( StrEqual(f, "index") )		indexDebugging_flag = 1 ; 
	elif( StrEqual(f, "gc") )			gcDebugging_flag = 1 ;

	elif( StrEqual(f, "mesg") )			infoMessagesDebugging_flag = 2 ;
	elif( StrEqual(f, "sys_trace1"))	sysTraceDebugging_flag = 1 ;
	elif( StrEqual(f, "sys_trace2"))	sysTraceDebugging_flag = 2 ;
	elif( StrEqual(f, "sys_trace3"))	sysTraceDebugging_flag = 3 ;
	elif( StrEqual(f, "sys_trace4"))	sysTraceDebugging_flag = 4 ;
	elif( StrEqual(f, "sys_trace"))		sysTraceDebugging_flag = 3 ;
	elif( StrEqual(f, "reloc") )		testRelocationDebugging_flag = 1 ;
	elif( StrEqual(f, "invar") )		testInvariantsDebugging_flag = 1 ;
	elif( StrEqual(f, "help") ) {
		WriteStd("%s%s\n", "DEBUG OPTIONS: ",
			"--debug stacks,trail,scratch,index,gc,,,mesg,sys_trace,reloc") ;
		EventHalt() ;
	}
	else
		Error("Invalid '--debug' option. Try 'cxprolog --debug help'") ;
}

static void HandleFlagsDebug(Str str)
{
	CharPt s = GStrMake(str) ;
	Str fl ;
	Char save ;
	if( s == nil || *s == '\0' ) return ;
	for( fl = s ; ; fl = ++s ) {
		while( *s != '\0' && *s != ',' ) s++ ;
		save = *s ;
		*s = '\0' ;
		if( *fl != '\0' )
			SetDebuggingFlag(fl) ;
		*s = save ;
		if( *s == '\0' ) break ;
	}
}

static void HandleFlagsPreSet(Str str)
{
	CharPt s = GStrMake(str) ;
	Str flag, arg ;
	Char stop ;	
	if( s == nil || *s == '\0' ) return ;
	do {
		flag = s ;	
		while( *s != '\0' && *s != ',' && *s != '=' ) s++ ;
		stop = *s ;
		*s++ = '\0' ;
		if( stop == '=' ) {
			arg = s ;
			while( *s != '\0' && *s != ',' ) s++ ;
			stop = *s ;
			*s++ = '\0' ;
			PreSetFlagValue(FindFlag(flag), arg) ;
		}
		
	} while( stop != '\0' ) ;
}

static void FlagShow(FlagEntryPt f)
{
	if( f->invisible )
		return ;

	if( *f->name == '\0' ) {
		Write("\n") ;
		return ;
	}

	Char sep = *f->flag == f->dflt ? '|' : '*' ;
	Write("%25s: ", f->name) ;
	switch( f->type ) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			Write("%-12ld", *f->flag) ;
			Write(" %c 0..%c", sep, f->type) ;
			break ;
		case 'I':
			Write("%-12ld", *f->flag) ;
			Write( " %c -integer-", sep) ;
			break ;
		case 'N':
			Write("%-12ld", *f->flag) ;
			Write(" %c -natural-", sep) ;
			break ;
		case 'T':
			Write("term") ;
			break ;
		case 'X': {
			if( StrEqual(f->name, "encoding") )
				Write("%-12s %c -encoding-",
								EncodingName(DefaultEncoding()), sep) ;
			elif( StrEqual(f->name, "encoding_os") )
				Write("%-12s %c -encoding-",
								EncodingName(SystemEncoding()), sep) ;
			elif( StrEqual(f->name, "version_data") )
				Write("%s %c -term-", TermAsStr(VersionTerm()), sep) ;
			elif( StrEqual(f->name, "version") )
				Write("%-12d %c -integer-", VersionGet(), sep) ;
			elif( StrEqual(f->name, "argv") )
				Write("%-12s %c -list-", "[...]", sep) ;
			elif( StrEqual(f->name, "dialect") )
				Write("%-12s %c -atom-", "cx", sep) ;
			else InternalError("FlagsShow") ;
			break ;
		}
		default: {
			FlagTypePt ft = FlagTypeGet(f->type) ;
			Write("%-12s", StrSeqGetVal(*f->flag, ft->values)) ;
			Write(" %c %s", sep, StrSeqFormat("", ", ", "", ft->values)) ;
		}
	}
	if( f->readOnly != false3 )
		Write(" [read-only]") ;
#if 0
	Write(" [flag = %d, dflt = %d, store = %d]", *f->flag, f->dflt, f->store) ;
#endif
	Write("\n") ;
}

void FlagsShow(void)
{
	register FlagEntryPt f ;
	ShowVersion() ;
	Write("FLAGS:\n") ;
	for( f = flagTable ; f->name != nil ; f++ )
		FlagShow(f) ;
}


/* CXPROLOG C'BUILTINS */

static void PFlag2(void)
{
	FlagEntryPt f = FindFlag(XTestAtomName(X0)) ;
	MustBe( UnifyWithAtomic(X1, GetFlagValue(f, IsInt(Drf(X1)))) ) ;
}

static void PFlag3(void)
{
	FlagEntryPt f = FindFlag(XTestAtomName(X0)) ;
	Ensure( UnifyWithAtomic(X1, GetFlagValue(f, IsInt(Drf(X1)))) ) ;
	SetFlagValue(f, X2) ;
	JumpNext() ;
}

static void PFlags0(void)
{
	FlagsShow() ;
	JumpNext() ;
}

static void PFlags1(void)
{
	Str command = XTestAtomName(X0) ;
	if( strcmp(command, "show") == 0 )
		FlagsShow() ;		
	elif( strcmp(command, "defaults") == 0 )
		FlagsForceDefaults() ;
	elif( strcmp(command, "store") == 0 )
		FlagsStoreCurrentValues() ;
	elif( strcmp(command, "restore") == 0 )
		FlagsRestoreStoredValues() ;
	else
		DatabaseError("Unknown flag command '%s'", command) ;
	JumpNext() ;
}

static void PSetPrologFlag(void)
{
	FlagEntryPt f = FindFlag(XTestAtomName(X0)) ;
	SetFlagValue(f, X1) ;
	JumpNext() ;
}

static void PCurrentPrologFlag(void)
{
	if( A(2) == tNilAtom ) {			/* init */
		X0 = Drf(X0) ;
		if( IsVar(X0) ) {				/* var init */
			A(2) = zeroIntPt ;
			A(3) = cPt(cC99Fix(flagTable)) ;
		}
		else {							/* atom handle */
			FlagEntryPt f = FindFlag(XTestAtomName(X0)) ;
			Discard() ;
			MustBe( Unify(X1, GetFlagValue(f, IsInt(Drf(X1)))) ) ;
		}
	}

	if( A(2) == zeroIntPt ) {			/* var handle */
		FlagEntryPt f = ((FlagEntryPt)A(3)) ;
		for( ; f->name != nil ; f++ )
			if( !f->invisible && f->name[0] != '\0' ) {
				A(3) = cPt(f+1) ;
				MustBe( UnifyWithAtomic(X0, MakeAtom(f->name))
					 && UnifyWithAtomic(X1, GetFlagValue(f, IsInt(Drf(X1)))) ) ;
			}
		Jump(DiscardAndFail) ;
	}
}

void FlagsRestart(void)
{
	/* nothing */ ;
}

void FlagsInit(void)
{
	FlagsStoreDefaultValues() ;
	HandleFlagsDebug(CmdLineArg("--debug")) ;
/* cxprolog --flags occurs_check=true,index_params=3,double_quotes=atom,nil_is_special=true */
	HandleFlagsPreSet(CmdLineArg("--flags")) ;
}

void FlagsInit2(void)
{
	InstallCBuiltinPred("flag", 2, PFlag2) ;
	InstallCBuiltinPred("flag", 3, PFlag3) ;
	InstallCBuiltinPred("flags", 0, PFlags0) ;
	InstallCBuiltinPred("flags", 1, PFlags1) ;

	InstallCBuiltinPred("set_prolog_flag", 2, PSetPrologFlag) ;
	InstallGNDeterCBuiltinPred("current_prolog_flag", 2, 2, PCurrentPrologFlag) ;
}

void FlagsInit3(void)
{
	FlagsEnterUserMode() ;
}
