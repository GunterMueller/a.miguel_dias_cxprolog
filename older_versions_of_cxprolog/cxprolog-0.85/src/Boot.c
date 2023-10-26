/*
 *   This file is part of the CxProlog system

 *   Boot.c
 *   by A.Miguel Dias - 2003/06/11
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

/* The following builtin predicates cannot be redefined, not even inside
	an alternative boot file */

static CharPt coreBuiltinsStr = "				\
												\
/* ACTIVATION */								\
												\
'$$_call_prolog'(X) :-							\
	X, !, '$$_set_success', fail.				\
'$$_call_prolog'(_) :- '$$_return'.				\
												\
'$$_run_thread'(X) :-							\
	X, !, '$$_active_thread_set_sucess', fail.	\
'$$_run_thread'(_) :- '$$_active_thread_return'.\
												\
'$$_lux0' :- 									\
	'$$_stress_testing',						\
	'$$_handle_command_line_args',				\
	create_unit(main),							\
	'$$_enter_user_mode',						\
	try main>>'$cxprolog_initialise',			\
	restart.									\
												\
'$$_lux1' :-									\
	try main>>'$cxprolog_top_level_goal',		\
	restart.									\
												\
'$$_stress_testing' :-							\
	flag(test_rellocation,_,off),				\
	flag(test_garbage_collection,_,off),		\
	(flag(test_rellocation,on)					\
	   -> writeln('*** STRESS-TESTING RELLOCATION ***')),			\
	(flag(test_garbage_collection,on)								\
	   -> writeln('*** STRESS-TESTING GARBAGE COLLECTION ***')).	\
																	\
'$$_handle_command_line_args' :-									\
	(os_arg('-check',_)	-> '$$_check_instalation' ; true),			\
	(os_arg('-boot',X) -> silent_consult(X) ; '$$_default_boot'),	\
	(os_arg('-script',X) -> '$$_consult_script'(X), exit ; true).	\
												\
/* SOME DEFINITIONS	*/							\
												\
call(X) :- X.									\
call(Pred,Args) :-	X =.. [Pred|Args], X.		\
												\
not X :- X, !, fail.							\
not _.											\
												\
\\+ X :- X, !, fail.							\
\\+ _.											\
												\
try G :- G, !.									\
try G.											\
												\
once G :- G, !.									\
												\
gen G :- G, fail.								\
gen G.											\
												\
possible X :- not X, !, fail.					\
possible _.										\
												\
If -> Then :- If, !, Then.						\
If -> Then.										\
If -> Then ; Else :- If, !, Then.				\
If -> Then ; Else :- !, Else.					\
A;_ :- A.										\
_;B :- B.										\
												\
A,B :- A, B.									\
												\
T =.. [F|As] :- '$$_univ'(As,T,F,0).			\
'$$_univ'([],T,F,N) :- functor(T,F,N), !.		\
'$$_univ'([A|As],T,F,N) :-						\
	'$$_sane_arity'(N),							\
	N1 is N+1, '$$_univ'(As,T,F,N1), !,			\
	arg(N1,T,A).								\
												\
'$$_user_assert'(C) :-							\
	assertz(C),									\
	numbervars(C,0,_),							\
	writeq(user,C),								\
	writeln(user,' asserted.'),					\
	restart.									\
(A :- B) :- B==true, !, '$$_user_assert'(A).	\
(A :- B) :- '$$_user_assert'((A :- B)).			\
												\
check_imports :-								\
	current_unit(U),							\
		U>>'$$_check_curr_unit_imports',		\
	fail.										\
check_imports.									\
												\
nodebug :- flag(debug,_,0).						\
notrace :- flag(debug,_,0).						\
debug :- flag(debug,_,1).						\
trace :- flag(debug,_,2).						\
												\
zpl :-											\
	fs_home,									\
	fs_exists(pl)								\
		-> fs_cd(X,[pl|X])						\
		; fs_cd([_|X],[pl|X]).					\
												\
/* CONTEXTS	*/									\
												\
context_top(U) :- context([U|_]).				\
unit_param(I,P) :-								\
	context([U|_]), arg(I,U,P).					\
unit_arity(A) :-								\
	context([U|_]), functor(U,_,A).				\
show_context :- context(X), writeqln(X).		\
show_context(S) :-								\
	context(X), writeqln(S,X).					\
												\
call_on_context([],X) :-						\
	call_on_empty_context(X).					\
call_on_context([U|C],X) :-						\
	call_on_context(C,U>>X).					\
												\
/* HISTORIC CONTEXT	*/							\
												\
show_hcontext :- hcontext(X), writeqln(X).		\
show_hcontext(S) :-								\
	hcontext(X), writeqln(S,X).					\
												\
/* OPERATORS */									\
												\
current_op(Pri, Type, Op) :-					\
	'$$_current_op_aux'(Op),					\
	'$$_op'(Type, Num),							\
	'$$_is_op'(Op, Num, Pri, Left, Right),		\
	'$$_op'(Type, Pri, Left, Right).			\
												\
'$$_op'( fx, 0).								\
'$$_op'( fy, 0).								\
'$$_op'( xf, 2).								\
'$$_op'( yf, 2).								\
'$$_op'(xfy, 1).								\
'$$_op'(xfx, 1).								\
'$$_op'(yfx, 1).								\
												\
'$$_op'( fx, Q, _, P) :- succ(P, Q).			\
'$$_op'( fy, Q, _, Q).							\
'$$_op'(xf,  Q, P, _) :- succ(P, Q).			\
'$$_op'(yf,  Q, Q, _).							\
'$$_op'(xfy, Q, P, Q) :- succ(P, Q).			\
'$$_op'(xfx, Q, P, P) :- succ(P, Q).			\
'$$_op'(yfx, Q, Q, P) :- succ(P, Q).			\
												\
/* CHECK INSTALATION */							\
												\
'$$_check_instalation' :-						\
	copy_term([2,1], X),						\
	sort(X,Y),									\
	Y = [2,1],									\
	writeln('*** FATAL ERROR: This instalation of CxProlog is FAULTY! ***'), \
	writeln('Bye.'),							\
	exit.										\
'$$_check_instalation' :-						\
	copy_term([10,9,8,7,6,5,4,3,2,1], X),		\
	sort(X,Y),									\
	Y = [1,2,3,4,5,6,7,8,9,10],					\
	writeln('This instalation of CxProlog seems to be OK!!!'), \
	writeln('Bye.'),							\
	exit.										\
";


/* The following builtin predicates can be redefined, but
	only inside an alternative boot file */

static CharPt predefinedBuiltinsStr = "			\
												\
/* UTILITIES */									\
												\
app([], L, L).									\
app([H|T], L, [H|R]) :- app(T, L, R).			\
												\
retractall(X) :- retract(X), fail.				\
retractall(X) :- retract((X:-_)), fail.			\
retractall(_).									\
												\
'$bind'([Name=Name|Rest]) :- '$bind'(Rest).		\
'$bind'([]).									\
												\
findall(T,G,L) :-								\
	queue_new(Q),								\
	'$$_findall'(T, G, Q),						\
	queue_as_list(Q, L),						\
	queue_delete(Q).							\
												\
'$$_findall'(T,G,Q) :-							\
	call(G), queue_put(Q,T), fail.				\
'$$_findall'(_,_,_).							\
												\
setof(T,G,S) :-									\
	bagof(T, G, B),								\
	sort(B, S).									\
												\
add_pl(user,user) :- !.							\
add_pl(user_input,user_input) :- !.				\
add_pl(A,A) :- slice(A,-3,-1,'.pl'), !.			\
add_pl(A,B) :- concat([A,'.pl'], B).			\
												\
/* CONSULT / RECONSULT */						\
												\
[].												\
[-File] :- !, reconsult(File).					\
[File] :- consult(File).						\
[File|Files] :- [File], Files.					\
												\
consult(File0) :-								\
	add_pl(File0, File),						\
	Heap0 is heapused,							\
	Time0 is cputime,							\
	silent_consult(File),						\
	Time is cputime - Time0,					\
	Heap is heapused - Heap0,					\
	write(user,'File '), writeq(user,File),		\
	('$$_is_reconsulting'						\
		-> write(user,' reconsulted ')			\
		 ; write(user,' consulted ')),			\
	write(user,Heap), write(user,' bytes '),	\
	write(user,Time), writeln(user,' sec.').	\
												\
reconsult(File0) :-								\
	'$$_enter_reconsulting',					\
	'$$_seq'(consult(File0),					\
	'$$_exit_reconsulting').					\
												\
silent_consult(File0) :-						\
	add_pl(File0, File),						\
	seeing(SaveIn), see(File),					\
	'$$_c_loop',								\
	'$$_check_curr_unit_imports',				\
	seen, see(SaveIn).							\
												\
'$$_consult_script'(File) :-					\
	seeing(SaveIn), see(File),					\
	get_line(_),	 /* skip #!cxprolog ... */	\
	'$$_c_loop',								\
	'$$_check_curr_unit_imports',				\
	seen, see(SaveIn).							\
												\
silent_reconsult(File0) :-						\
	'$$_enter_reconsulting',					\
	'$$_seq'(silent_consult(File0),				\
	'$$_exit_reconsulting').					\
												\
'$$_c_loop' :-									\
	repeat,										\
		read(Term),								\
	'$$_c_term'(Term), !.						\
												\
'$$_c_term'(X) :- var(X), !, assertz(X).		\
'$$_c_term'(end_of_file) :- !.					\
'$$_c_term'(eof) :- !.							\
'$$_c_term'(unit U) :- !,						\
	varnames(Vars), '$bind'(Vars),				\
	create_unit(U), U>>'$$_c_loop',				\
	U>>'$$_check_curr_unit_imports'.			\
'$$_c_term'(visible Spec) :- !,					\
	'$$_visible'(Spec), fail.					\
'$$_c_term'(import Spec from U) :- !,			\
	'$$_import'(Spec,U), fail.					\
'$$_c_term'((:-X)) :- !, (:-X), !, fail.		\
'$$_c_term'((?-X)) :- !,						\
	varnames(V), question(X,V), !, fail.		\
'$$_c_term'(X) :- assertz(X), fail.				\
												\
'$$_seq'(A,B) :- A, !, B, !.					\
'$$_seq'(A,B) :- B, !, fail.					\
												\
												\
/* LISTING */									\
												\
all :-											\
	current_unit(U),							\
		U>>listing,								\
	fail.										\
all.											\
												\
list :- listing.								\
												\
listing :-										\
	'$$_listing_header',						\
	'$$_listing_visibles',						\
	'$$_listing_imports',						\
	'$$_check_curr_unit_imports',				\
	'$$_listing_predicates', nl.				\
												\
listing(F/A) :-									\
	listing(F,A).								\
listing(F,A) :-									\
	functor(H,F,A),								\
	clause(H,B),								\
		portray_clause((H:-B)),					\
	fail.										\
listing(F) :-									\
	current_predicate(H),						\
		functor(H,F,_),							\
		clause(H,B),							\
		portray_clause((H:-B)),					\
	fail.										\
listing(_).										\
												\
'$$_listing_header' :-							\
	write('**** '),								\
	(unit_spec(USpec)							\
		-> write(unit USpec)					\
		; write('BUILTINS')),					\
	writeln(' ****************').				\
												\
'$$_listing_visibles' :-						\
	visible_predicate(H),						\
	functor(H,F,A),								\
	writeqln(visible F/A),						\
	fail.										\
'$$_listing_visibles'.							\
												\
'$$_listing_imports' :-							\
	imported_predicate(H,U),					\
	functor(H,F,A),								\
	writeqln(import F/A from U),				\
	fail.										\
'$$_listing_imports'.							\
												\
'$$_listing_predicates' :-						\
	current_predicate(H),						\
		clause(H,B),							\
			portray_clause((H:-B)),				\
	fail.										\
'$$_listing_predicates'.						\
												\
portray_clause((H:-true)) :- !,					\
	numbervars(H, 0, _),						\
	writeq(H),writeln('.').						\
portray_clause((H:-B)) :-						\
	numbervars((H:-B), 0, _),					\
	writeq(H), writeln(' :-'),					\
	'$$_portray_body'(B),						\
	writeln('.').								\
												\
'$$_portray_body'(X) :- var(X), !,				\
	tab(8), writeq(X).							\
'$$_portray_body'((X,Xs)) :-	!,				\
	tab(8), writeq(X), writeln(','),			\
	'$$_portray_body'(Xs).						\
'$$_portray_body'(X) :-							\
	tab(8), writeq(X).							\
												\
/* QUESTION INTERACTION */						\
												\
(:-G) :- G, !.									\
(:-G) :-										\
	write(user_error,'? '),						\
	writeln(user_error,G).						\
												\
(?-G) :- G.										\
												\
question((:-G),_) :- !, (:-G).					\
question(G,[]) :-								\
	G, !,										\
	writeln(user,yes).							\
question(G,[H|T]) :-							\
	G,											\
	'$$_show_vars'([H|T]),						\
	'$$_more', !.								\
question(_,_) :- writeln(user,no).				\
												\
'$$_more' :- '$$_getnb'(X), '$$_more'(X).		\
'$$_more'(10) :- !, writeln(user,yes).			\
'$$_more'(0';) :- !, skip(user,10), fail.		\
'$$_more'(_) :-									\
	skip(user,10), writeln(user,yes).			\
'$$_getnb'(X) :-								\
	repeat, get0(user,X), X	\\= 32, !.			\
												\
'$$_show_vars'([]).								\
'$$_show_vars'([S]) :- !,						\
	'$$_show_one_var'(S).						\
'$$_show_vars'([H|T]) :-						\
	'$$_show_one_var'(H), nl(user),				\
	'$$_show_vars'(T).							\
'$$_show_one_var'(Var=Val) :-					\
	write(user,Var),							\
	write(user,'='),							\
	writeq(user,Val).							\
												\
/* BAGOF										\
	Adapted from C-Prolog: David Warren,		\
	Fernando Pereira, R.A.O'Keefe.	*/			\
												\
Variable^Goal :- Goal.							\
												\
bagof(Template, Generator, Bag) :-				\
	'$$_excess_vars'(Generator, Template, [], FreeVars), \
	FreeVars \\== [], !,						\
	Key =.. ['$$_'|FreeVars],					\
	findall(Key-Template, Generator, Bags0),	\
	keysort(Bags0, Bags),						\
	'$$_pick'(Bags, Key, Bag).					\
bagof(Template, Generator, Bag) :-				\
	findall(Template, Generator, Bag0),			\
	Bag0 \\== [],								\
	Bag = Bag0.									\
												\
'$$_pick'(Bags, Key, Bag) :-					\
	Bags \\== [],								\
	'$$_parade'(Bags, Key1, Bag1, Bags1),		\
	'$$_decide'(Key1, Bag1, Bags1, Key, Bag).	\
												\
'$$_parade'([K-X|L1], K, [X|B], L) :- !,		\
	'$$_parade'(L1, K, B, L).					\
'$$_parade'(L, K, [], L).						\
												\
'$$_decide'(Key, Bag, [], Key, Bag) :- !.		\
'$$_decide'(Key, Bag, Bags, Key, Bag).			\
'$$_decide'(_, _, Bags, Key, Bag) :-			\
	'$$_pick'(Bags, Key, Bag).					\
												\
'$$_excess_vars'(T, X, L0, L) :-				\
	var(T), !,									\
	'$$_excess_vars2'(T, X, L0, L).				\
'$$_excess_vars2'(T, X, L0, L) :-				\
	not subterm(T, X), !,						\
	'$$_introduce'(T, L0, L).					\
'$$_excess_vars2'(T, X, L0, L0).				\
'$$_excess_vars'(X^P, Y, L0, L) :- !,			\
	'$$_excess_vars'(P, (X,Y), L0, L).			\
'$$_excess_vars'(setof(X,P,S), Y, L0, L) :- !,	\
	'$$_excess_vars'((P,S), (X,Y), L0, L).		\
'$$_excess_vars'(bagof(X,P,S), Y, L0, L) :- !,	\
	'$$_excess_vars'((P,S), (X,Y), L0, L).		\
'$$_excess_vars'(T, X, L0, L) :-				\
	functor(T, _, N),							\
	'$$_rem_excess_vars'(N, T, X, L0, L).		\
												\
'$$_rem_excess_vars'(0, _, _, L, L) :- !.		\
'$$_rem_excess_vars'(N, T, X, L0, L) :-			\
	succ(M, N),									\
	arg(N, T, T1),								\
	'$$_excess_vars'(T1, X, L0, L1),			\
	'$$_rem_excess_vars'(M, T, X, L1, L).		\
												\
'$$_introduce'(X, L, L) :-						\
	subterm(X, L), !.							\
'$$_introduce'(X, L, [X|L]).					\
";


/* The following builtin predicates are defaults that will be
   used only if no alternative boot file is provided */

static CharPt defaultBuiltinsStr = "			\
												\
'$cxprolog_initialise':-						\
	version,									\
	'$env_context' := [main].					\
												\
'$cxprolog_top_level_goal' :-					\
	'$env_context' =: C,						\
	call_on_context(C,'$top_level2').			\
												\
/* TOP LEVEL */									\
												\
'$top_level1' :-								\
  ( flag(debug,1) -> write(user,'(debug) ')		\
  ; flag(debug,2) -> write(user,'(trace) ')		\
  ; true ),										\
	context(C), write(user,C),					\
	write(user,' ?- '),							\
	read(user,G), varnames(V),					\
	question(G,V).								\
												\
'$top_level2' :-								\
  ( flag(debug,1) -> write(user,'(debug) ')		\
  ; flag(debug,2) -> write(user,'(trace) ')		\
  ; true ),										\
	context(C), write(user,C),					\
	write(user,' ?- '),							\
	read(user,G), varnames(V), 					\
	abolish('$$_top_call',1),					\
	assert(('$$_top_call'(V):-G)),				\
	question('$$_top_call'(V),V).				\
												\
/* HANDLING CONTEXT */							\
												\
push U :-										\
	U>>true,	/* validate unit U */			\
	'$env_context' =: C,						\
	'$env_context' := [U|C],					\
	restart.									\
pop :-											\
	'$env_context' =: [_|C],					\
	'$env_context' := C,						\
	restart.									\
												\
";

static void ZBasicLoadFile(CharPt fileName)
{
    register Pt t ;
    See(fileName) ;
    HSave() ;
    while( (t = ZReadTerm()) != eofPt ) {
        if( t == nil )
            FatalError("Installing special builtin predicate") ;
        else CompileClause(t, true) ;
        HRestore() ;
    }
    Seen() ;
}

static Bool ZBasicLoadStrMac(CharPt str)
{
    if( strlen(coreBuiltinsStr) < 2 K ) {
        if( str == coreBuiltinsStr )
            ZBasicLoadFile(":pl:CxCore.pl") ;
        elif( str == predefinedBuiltinsStr )
            ZBasicLoadFile(":pl:CxDefs.pl") ;
        elif( str == defaultBuiltinsStr )
            ZBasicLoadFile(":pl:CxBoot.pl") ;
        else Default("ZBasicLoadStrMac") ;
        return true ;
    }
    else return false ;
}

static void ZBasicLoadStr(CharPt str)
{
    register Pt t ;
    if( ZBasicLoadStrMac(str) ) return ;
    SeeStr(str) ;
    HSave() ;
    while( (t = ZReadTerm()) != eofPt ) {
        if( t == nil )
            FatalError("Installing special builtin predicate") ;
        else CompileClause(t, true) ;
        HRestore() ;
    }
    SeenStr() ;
}

void Boot(int argc, CharPt argv[])
{
	StreamsInit() ;
	CmdLineInit(argc, argv) ;
	YourPrologue() ;
	InterruptOff() ;
	MemoryInit() ;
	BufferInit() ;
	CheckHost() ;  /* Must be after MemoryInit() */
	NumbersInit() ;
	ClockInit() ;
	IndexInit() ;
	VarDictionaryInit() ;
	AtomsInit() ;
	FunctorsInit() ;
	InstructionsInit() ;
	MachineInit() ;
	CompilerInit() ;
	UnitsInit() ;	
/* C predicates may be installed from here onwards */

	AtomsInit2() ;
	InstructionsInit2() ;
	GCollectionInit() ;
	NumbersInit2() ;
	FlagsInit() ;
	DebugInit() ;
	DisassemblerInit() ;
	TermsInit() ;
	ArithInit() ;
	UnifyInit() ;
	OpsInit() ;
	PredicatesInit() ;
	ThreadsInit() ;
	TermReadInit() ;
	TermWriteInit() ;
	
	IVarsInit() ;
	Streams2Init() ;
	QueuesInit() ;
	StacksInit() ;
	DictsInit() ;
	ArraysInit() ;
	FileSysInit() ;
	NetInit() ;
	ProcessesInit() ;
	BootInit() ;
	CmdLineInit2() ;
	CallPrologInit() ;

/* Create root threads and initialize abstract machine */
	ThreadRootCreate(MakeAtom("$$_lux0"), MakeAtom("$$_lux1")) ;

	ZCheckHostSpeed() ;
	ZBasicLoadStr(coreBuiltinsStr) ;
	ZBasicLoadStr(predefinedBuiltinsStr) ;
	MarkBuiltinsPermanent() ;
	YourExtensions() ;
	InterruptOn() ;
}


/* CXPROLOG C'BUILTINS */

static void PEnterUserMode()
{
	if( !Booting() )
		FatalError("Cannot be activated twice") ;
	CompatibleIfThenUpdateFlags(compatibleIfThen_flag) ;
	MarkBuiltinsPermanent() ;
	UserModeInstructions() ;
	Booting() = false ;
	JumpNext()
}

static void PDefaultBoot()
{
	ZBasicLoadStr(defaultBuiltinsStr) ;
	JumpNext()
}

static void PEndOfFile()
{
	WriteStd("^D\n") ;
	EventHalt() ;
	JumpNext()
}

static void PVersion()
{
	VersionShow() ;
	JumpNext()
}


static void PHostSpeed()
{
	Size speed = hostSpeed ;
	if( testRelocation_flag || testGCollection_flag ) speed /= 200 ;
	if( Unify(X0, MakeInt(speed)) )
		JumpNext()
	DoFail()
}

static void POSName()
{
	if( Unify(X0, MakeAtom(OSName())) )
		JumpNext()
	DoFail()
}

static void PShow()
{
	Write("Show what?\n") ;
	Write("    arrays.  atoms.       builtins.  dicts.    flags.    floats.  ivars.  ops. \n") ;
	Write("    queues.  statistics.  stacks.    streams.  threads.  units.   version.\n") ;
	JumpNext()
}

void BootInit()
{
	InstallCBuiltinPred("$$_enter_user_mode", 0, PEnterUserMode) ;
	InstallCBuiltinPred("$$_default_boot", 0, PDefaultBoot) ;
	InstallCBuiltinPred("end_of_file", 0, PEndOfFile) ;

	InstallCBuiltinPred("version", 0, PVersion) ;
	InstallCBuiltinPred("host_speed", 1, PHostSpeed) ;
	InstallCBuiltinPred("os_name", 1, POSName) ;
	InstallCBuiltinPred("show", 0, PShow) ;
}
