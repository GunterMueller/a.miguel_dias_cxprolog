/*
 *   This file is part of the CxProlog system

 *   CxProlog.c
 *   by A.Miguel Dias - 2000/04/02
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

/* The following builtin predicates cannot be redefined, not even inside
	an alternative boot file */

static CharPt coreBuiltinsStr = "			\
											\
/* ACTIVATION */							\
											\
'$$_called_from_c'(X) :-					\
	X, active_thread_completed.				\
'$$_called_from_c'(_) :-					\
	active_thread_failed.					\
											\
'$$_lux0' :-								\
	flag(compatible_if_then,_,on),			\
	(unix_get_arg('-boot',X)				\
			-> silent_consult(X)			\
			; '$$_default_boot'),			\
	'$$_enter_user_mode',					\
	create_unit(main),						\
	main>>'$cxprolog_startup',				\
	restart.								\
											\
'$$_lux1' :-								\
	dict_clear('$dict'),					\
	queue_clear('$queue'),					\
	stack_clear('$stack'),					\
	main>>'$cxprolog_restart',				\
	restart.								\
											\
/* SOME DEFINITIONS	*/						\
											\
call(X) :- X.								\
call(Pred,Args) :-	X =.. [Pred|Args], X.	\
											\
'$$_debugging_call'(G) :-					\
	'$$_init_debugging',					\
	G,										\
	'$$_end_debugging',						\
	(true ; '$$_init_debugging', fail).		\
'$$_debugging_call'(_) :-					\
	'$$_end_debugging',						\
	fail.									\
											\
not X :- X, !, fail.						\
not _.										\
											\
If -> Then :- If, !, Then.					\
If -> Then.									\
If -> Then ; Else :- If, !, Then.			\
If -> Then ; Else :- !, Else.				\
A;_ :- A.									\
_;B :- B.									\
											\
A,B :- A, B.								\
											\
(:-G) :- G, !.								\
(:-G) :- write(user,'? '), writeln(user,G).	\
(?-G) :- G.									\
											\
T =.. [F|As] :- '$$_univ'(As,T,F,0).		\
'$$_univ'([],T,F,N) :- functor(T,F,N), !.	\
'$$_univ'([A|As],T,F,N) :-					\
	N1 is N+1, '$$_univ'(As,T,F,N1), !,		\
	arg(N1,T,A).							\
											\
(A :- B) :-									\
    assertz((A :- B)),						\
	numbervars((A :- B), 0, _),				\
    writeq(user,(A :- B)),					\
    writeln(user,' asserted.'),				\
    restart.								\
											\
check_imports :-							\
	current_unit(U),						\
		U>>'$$_check_curr_unit_imports',	\
	fail.									\
check_imports.								\
											\
/* CONTEXTS	*/								\
											\
U>>X :- '$$_primitive'(1).					\
U<>X :- '$$_primitive'(2).					\
call_on_empty_context(X) :-					\
	'$$_primitive'(3).						\
down(X) :- 'builtin'<>X.					\
context_top(U) :- context([U|_]).			\
unit_param(I,P) :-							\
	context([U|_]), arg(I,U,P).				\
unit_arity(A) :-							\
	context([U|_]), functor(U,_,A).			\
show_context :- context(X), writeqln(X).	\
show_context(S) :-							\
	context(X), writeqln(S,X).				\
											\
call_on_context([],X) :-					\
	call_on_empty_context(X).				\
call_on_context([U|C],X) :-					\
	call_on_context(C,U>>X).				\
											\
/* HISTORIC CONTEXT	*/						\
											\
>X :- '$$_primitive'(4).					\
<X :- '$$_primitive'(5).					\
show_hcontext :- hcontext(X), writeqln(X).	\
show_hcontext(S) :-							\
	hcontext(X), writeqln(S,X).				\
											\
/* OPERATORS */								\
											\
current_op(Pri, Type, Op) :-				\
	'$$_current_op_aux'(Op),				\
	'$$_op'(Type, Num),						\
	'$$_is_op'(Op, Num, Pri, Left, Right),	\
	'$$_op'(Type, Pri, Left, Right).		\
											\
'$$_op'( fx, 0).							\
'$$_op'( fy, 0).							\
'$$_op'( xf, 2).							\
'$$_op'( yf, 2).							\
'$$_op'(xfy, 1).							\
'$$_op'(xfx, 1).							\
'$$_op'(yfx, 1).							\
											\
'$$_op'( fx, Q, _, P) :- succ(P, Q).		\
'$$_op'( fy, Q, _, Q).						\
'$$_op'(xf,  Q, P, _) :- succ(P, Q).		\
'$$_op'(yf,  Q, Q, _).						\
'$$_op'(xfy, Q, P, Q) :- succ(P, Q).		\
'$$_op'(xfx, Q, P, P) :- succ(P, Q).		\
'$$_op'(yfx, Q, Q, P) :- succ(P, Q).		\
											\
/* QUESTION INTERACTION */					\
											\
question((:-G),_) :- !, (:-G).				\
question(G,[]) :-							\
	'$$_debugging_call'(G), !,				\
	writeln(user,yes).						\
question(G,[H|T]) :-						\
	'$$_debugging_call'(G),					\
	'$$_show_vars'([H|T]),					\
	'$$_more', !.							\
question(_,_) :- writeln(user,no).			\
											\
'$$_more' :- '$$_getnb'(X), '$$_more'(X).	\
'$$_more'(10) :- !, writeln(user,yes).		\
'$$_more'(0';) :- !, skip(user,10), fail.	\
'$$_more'(_) :-								\
	skip(user,10), writeln(user,yes).		\
'$$_getnb'(X) :-							\
	repeat, get0(user,X), X\\= 32, !.		\
											\
'$$_show_vars'([]).							\
'$$_show_vars'([S]) :- !,					\
	'$$_show_one_var'(S).					\
'$$_show_vars'([H|T]) :-					\
	'$$_show_one_var'(H), nl(user),			\
	'$$_show_vars'(T).						\
'$$_show_one_var'(Var=Val) :-				\
	write(user,Var),						\
	write(user,'='),						\
	writeq(user,Val).						\
" ;


/* The following builtin predicates can be redefined, but
    only inside an alternative boot file */

static CharPt predefinedBuiltinsStr = "		\
											\
/* UTILITIES */								\
											\
app([], L, L).								\
app([H|T], L, [H|R]) :-  app(T, L, R).		\
											\
retractall(X) :- retract(X), fail.			\
retractall(X) :- retract((X:-_)), fail.		\
retractall(_).								\
											\
findall(X,G,_) :-							\
	queue_clear('$queue'),					\
	call(G),								\
		queue_put('$queue',X),				\
	fail.									\
findall(_,_,L) :-							\
	queue_as_list('$queue', L),				\
	queue_clear('$queue').					\
											\
add_pl(user,user) :- !.						\
add_pl(A,A) :- slice(A,-3,-1,'.pl'), !.		\
add_pl(A,B) :- concat([A,'.pl'], B).		\
											\
/* CONSULT / RECONSULT */					\
											\
[].											\
[-File] :- !, reconsult(File).				\
[File] :- consult(File).					\
[File|Files] :- [File], Files.				\
											\
consult(File0) :-							\
	add_pl(File0, File),					\
	Heap0 is heapused,						\
	Time0 is cputime,						\
	silent_consult(File),					\
	Time is cputime - Time0,				\
	Heap is heapused - Heap0,				\
	write(user,'File '), writeq(File),		\
	write(user,' consulted '),				\
	write(user,Heap), write(user,' bytes '),\
	write(user,Time), writeln(user,' sec.').\
											\
reconsult(File0) :-							\
	add_pl(File0, File),					\
	Heap0 is heapused,						\
	Time0 is cputime,						\
	silent_reconsult(File),					\
	Time is cputime - Time0,				\
	Heap is heapused - Heap0,				\
	write(user,'File '), writeq(File),		\
	write(user,' reconsulted '),			\
	write(user,Heap), write(user,' bytes '),\
	write(user,Time), writeln(user,' sec.').\
											\
silent_consult(File0) :-					\
	add_pl(File0, File),					\
	seeing(SaveIn), see(File),				\
	'$$_c_loop',							\
	seen, see(SaveIn).						\
											\
'$$_c_loop' :-								\
	repeat,									\
		'$$_read_for_assert'(Term),			\
	'$$_c_term'(Term), !.					\
											\
'$$_c_term'(X) :- var(X), !, assertz(X).	\
'$$_c_term'(end_of_file) :- !.				\
'$$_c_term'(eof) :- !.						\
'$$_c_term'(unit U) :- !,					\
	create_unit(U), U>>'$$_c_loop'.			\
'$$_c_term'(visible Spec) :- !,				\
	'$$_visible'(Spec), fail.				\
'$$_c_term'(import Spec from U) :- !,		\
	'$$_import'(Spec,U), fail.				\
'$$_c_term'((:-X)) :- !, (:-X), !, fail.	\
'$$_c_term'((?-X)) :- !,					\
	varnames(V), question(X,V), !, fail.	\
'$$_c_term'(X) :- assertz(X), fail.			\
											\
silent_reconsult(File0) :-					\
	add_pl(File0, File),					\
	seeing(SaveIn), see(File),				\
	'$$_r_loop',							\
	seen, see(SaveIn).						\
											\
'$$_r_loop' :-								\
	dict_clear('$dict'),					\
	repeat,									\
		'$$_read_for_assert'(Term),			\
	'$$_r_term'(Term), !.					\
											\
'$$_r_term'(X) :- var(X), !, assertz(X).	\
'$$_r_term'(end_of_file) :- !.				\
'$$_r_term'(eof) :- !.						\
'$$_r_term'(unit U) :- !,					\
	dict_clear('$dict'),					\
	create_unit(U),							\
	U>>'$$_r_loop'.							\
'$$_r_term'(visible Spec) :- !,				\
	'$$_retract'(Spec),						\
	'$$_visible'(Spec),						\
	fail.									\
'$$_r_term'(import Spec from U) :- !,		\
	'$$_retract'(Spec),						\
	'$$_import'(Spec,U),					\
	fail.									\
'$$_r_term'((:-X)) :- !, (:-X), !, fail.	\
'$$_r_term'((?-X)) :- !,					\
	varnames(V), question(X,V), !, fail.	\
'$$_r_term'((H:-B)) :- !,					\
	functor(H,N,A),							\
	'$$_cond_retract'(N,A),					\
	assertz((H:-B)),						\
	fail.									\
'$$_r_term'(H) :-							\
	functor(H,N,A),							\
	'$$_cond_retract'(N,A),					\
	assertz(H),								\
	fail.									\
											\
'$$_visible'([]) :- !.						\
'$$_visible'([H|T]) :- !,					\
	'$$_visible'(H), '$$_visible'(T).		\
'$$_visible'(N/A) :- !,						\
	visible(N,A).							\
'$$_visible'(_) :-							\
	writeln(user_error,'Invalid specification for visible/1'),	\
	restart.								\
											\
'$$_import'([],U) :- !.						\
'$$_import'([H|T],U) :- !,					\
	'$$_import'(H,U), '$$_import'(T,U).		\
'$$_import'(N/A,U) :- !,					\
	import(N,A,U).							\
'$$_import'(_,_) :-							\
	writeln(user_error,'Invalid specification for import/2'),	\
	restart.								\
											\
'$$_retract'([]) :- !.						\
'$$_retract'([H|T]) :- !,					\
	'$$_retract'(H), '$$_retract'(T).		\
'$$_retract'(N/A) :- !,						\
	'$$_cond_retract'(N,A).					\
'$$_retract'(_) :-							\
	writeln(user_error,'Invalid specification for visible/1 or import/2'),	\
	restart.								\
											\
'$$_cond_retract'(N,A) :-					\
	dict_get('$dict',p(N,A),true), !.		\
'$$_cond_retract'(N,A) :-					\
	dict_set('$dict',p(N,A),true),			\
	abolish(N,A).							\
											\
/* LISTING */								\
											\
all :-										\
	current_unit(U),						\
		U>>listing,							\
		fail.								\
all.										\
											\
list :-	 listing.							\
											\
listing :-									\
	'$$_listing_header',					\
	'$$_listing_visibles',					\
	'$$_listing_imports',					\
	'$$_check_curr_unit_imports',			\
	'$$_listing_predicates', nl.			\
											\
listing(F/A) :-								\
	listing(F,A).							\
listing(F,A) :-								\
	functor(H,F,A),							\
	clause(H,B),							\
		portray_clause((H:-B)),				\
	fail.									\
listing(F) :-								\
	current_predicate(H),					\
		functor(H,F,_),						\
		clause(H,B),						\
		portray_clause((H:-B)),				\
	fail.									\
listing(_).									\
											\
'$$_listing_header' :-						\
	context_top(U),							\
	functor(U,F,A),							\
	write('**** '),							\
	writeq(unit F/A),						\
	writeln(' ****************').			\
											\
'$$_listing_visibles' :-					\
	visible_predicate(H),					\
	functor(H,F,A),							\
	writeqln(visible F/A),					\
	fail.									\
'$$_listing_visibles'.						\
											\
'$$_listing_imports' :-						\
	imported_predicate(H,U),				\
	functor(H,F,A),							\
	writeqln(import F/A from U),			\
	fail.									\
'$$_listing_imports'.						\
											\
'$$_listing_predicates' :-					\
	current_predicate(H),					\
		clause(H,B),						\
			portray_clause((H:-B)),			\
	fail.									\
'$$_listing_predicates'.					\
											\
portray_clause((H:-true)) :- !,				\
	numbervars(H, 0, _),					\
	writeq(H),writeln('.').					\
portray_clause((H:-B)) :-					\
	numbervars((H:-B), 0, _),				\
	writeq(H), writeln(' :-'),				\
	'$$_portray_body'(B),					\
	writeln('.').							\
											\
'$$_portray_body'(X) :- var(X), !,			\
	tab(8), writeq(X).						\
'$$_portray_body'((X,Xs)) :-	!,			\
	tab(8), writeq(X), writeln(','),		\
	'$$_portray_body'(Xs).					\
'$$_portray_body'(X) :-						\
	tab(8), writeq(X).						\
" ;


/* The following builtin predicates are defaults that will be
   used only if no alternative boot file is provided */

static CharPt defaultBuiltinsStr = "		\
											\
'$cxprolog_startup':-						\
	version,								\
	flag(debugging,_,on),					\
	flag(trace,_,off),						\
	flag(fail_on_error,_,off),				\
	flag(compatible_if_then,_,on),			\
	flag(keep_source,_,on),					\
	flag(smart_atoms,_,on),					\
	flag(undef_warnings,_,on),				\
	flag(memory_warnings,_,on),				\
	flag(show_text,_,off),					\
	flag(super_indexes,_,off),				\
	'$env_context' := [main],				\
	restart.								\
											\
'$cxprolog_restart' :-						\
	'$env_context' =: C,					\
	call_on_context(C,'$top_level').		\
											\
/* HANDLING CONTEXT */						\
											\
push U :-									\
	U>>true,	/* validate unit U */		\
	'$env_context' =: C,					\
	'$env_context' := [U|C],				\
	restart.								\
pop :-										\
	'$env_context' =: [_|C],				\
	'$env_context' := C,					\
	restart.								\
											\
/* TOP LEVEL */								\
											\
'$top_level' :-								\
	repeat,									\
		(flag(trace,on) ->					\
			write(user,'(trace) ') ; true),	\
		context(C), write(user,C),			\
		write(user,' ?- '),					\
		read(user,G), varnames(V),			\
		question(G,V),						\
	fail.									\
											\
trace :- flag(trace,_,on).					\
notrace :- flag(trace,_,off).				\
											\
" ;

static long stacksSize = stacksSizeDefault ;/* default stacks size	*/

static void BasicLoadStr(CharPt str)
{
	register Pt t ;
	Hdl saveH = H ;

	SeeStr(str) ;
	while( (t = ReadTerm()) != tEofAtom ) {
		if( t == nil )
			FatalError("Installing special builtin predicate") ;
		else CompileClause(t, true) ;
		H = saveH ;
	}
	SeenStr() ;
}

static void CxPrologue(void)
{
	CharPt s ;
	int i ;

	if( (s = GetEnv("CXPROLOG_SIZE")) != nil ||
		(s = GetArg("-size")) != nil )
	{
		if( sscanf(s, "%d", &i) != 1 )
			Error("Invalid size") ;
		if( not InRange(i, 0, 100000) )
			Error("size out of range (0..100000)") ;
		stacksSize = i K ;		
	}
}

static jmp_buf eventHandler ;

void EventContinue()	{ longjmp(eventHandler, 1) ; }
void EventForceFail()	{ longjmp(eventHandler, 2) ; }
void EventRestart()		{ longjmp(eventHandler, 3) ; }
void EventHalt()		{ longjmp(eventHandler, 4) ; }
void EventExit()		{ longjmp(eventHandler, 5) ; }

void ShowVersion(void)
{
	Write("CxProlog version 0.76\n") ;
}


void InitCxProlog(void) ;

int main(int argc, CharPt argv[])
{
	ThreadPt th ;
	
	switch( setjmp(eventHandler) ) {
		case 0: {	/* startup */
			InitStreams() ;
			InitArgs(argc, argv) ;
			CxPrologue() ;
			YourPrologue() ;
			InitMemory() ;
			CheckHost() ;  /* Must be after InitMemory() */
			InitReals() ;
			InitTime() ;
			InitIndex() ;
			InitVarDictionary() ;
			InitAtoms() ;
			InitFunctors() ;
			InitMachine() ;
			InitCompiler() ;			

			InitUnits() ;
			InitFlags() ;
			InitDebug() ;
			InitListCode() ;
			InitTerms() ;
			InitArith() ;
			InitUnify() ;
			InitOps() ;
			InitPredicates() ;
			InitThreads() ;
			InitTermRead() ;
			InitTermWrite() ;
			InitCxProlog() ;

			InitUnixServices() ;
			InitFileSys() ;
			InitIVars() ;
			InitQueues() ;
			InitStacks() ;
			InitDicts() ;

			th = ThreadNew(stacksSize, MakeAtom("$$_lux0"), MakeAtom("$$_lux1")) ;
			CheckHostSpeed() ;
			BasicLoadStr(coreBuiltinsStr) ;
			MarkCoreBuiltins() ;
			BasicLoadStr(predefinedBuiltinsStr) ;
			MarkBuiltins() ;
			YourExtensions() ;
			ThreadTransfer(th, tGoingAtom) ;
			FatalError("main") ;
			break ;
		}
		case 1: {	/* continue */
			RunMachine() ;
			break ;
		}
		case 2: {	/* force fail */
			FreeBuffer() ;
			P = Bf(P) ;
			RunMachine() ;
			break ;
		}
		case 3: {	/* restart current thread */
			FreeBuffer() ;
			if( not UnitIsReserved(builtinUnit) )
				EventHalt() ; /* an error has ocurred at boot time */
			ActiveThreadRestart() ;
			break ;
		}
		case 4: {	/* halt */
			FreeBuffer() ;
			SaneStreams() ;
			WriteStd("CxProlog halted.\n") ;
			break ;
		}
		case 5: {	/* exit */
			FreeBuffer() ;
			SaneStreams() ;
			break ;
		}
		default: {
			Default("main") ;
			break ;
		}
	}
	return 0 ;
}

/* CXPROLOG C'BUILTINS */

static void PVersion()
{
	ShowVersion() ;
	JumpNext()
}

static void PRestart()
{
	EventRestart() ;
	JumpNext()
}

static void PHalt()
{
	EventHalt() ;
	JumpNext()
}

static void PExit()
{
	EventExit() ;
	JumpNext()
}

static void PEndOfFile()
{
	WriteStd("^D\n") ;
	EventHalt() ;
	JumpNext()
}

static void PGetLevel()
{
	if( UnifyWithAtomic(X0, TagAtom(Ef(B0))) ) JumpNext()
	DoFail()
}

static void PCutTo()
{
	Pt t0 = Drf(X0) ;
	if( cChoicePointPt(XPt(t0)) > B ) {
		B = cChoicePointPt(XPt(t0)) ;
		HB = Bf(H) ;
	
	/* TidyTrail
		t0 = cPt(Bf(TR)) ;
		while( cHdl(t0) < TR )
			if( IsToTrailVar(*cHdl(t0)) ) t0++ ;
			else *t0 = cWord(Pop(TR)) ; */
	}
	JumpNext()
}

static void PCut()
{
	Error("Dynamic '!/0' is not supported") ;
	JumpNext()
}

static void PHostSpeed()
{
	if( Unify(X0, MakeInt(hostSpeed)) ) JumpNext()
	DoFail()
}

static void PDefaultBoot()
{
	BasicLoadStr(defaultBuiltinsStr) ;
	JumpNext()
}

static void PShow()
{
	Write("Show what?\n") ;
	Write("    builtins.  flags.  ops.     stacks.      streams.  units.\n") ;
	Write("    dicts.     ivars.  queues.  statistics.  threads.  version.\n") ;
	JumpNext()
}

static void InitCxProlog()
{
	InstallCBuiltinPred("version", 0, PVersion) ;
	InstallCBuiltinPred("restart", 0, PRestart) ;
	InstallCBuiltinPred("halt", 0, PHalt) ;
	InstallCBuiltinPred("exit", 0, PExit) ;
	InstallCBuiltinPred("end_of_file", 0, PEndOfFile) ;
	InstallCBuiltinPred("get_level", 1, PGetLevel) ;
	InstallCBuiltinPred("cut", 1, PCutTo) ;
	InstallCBuiltinPred("!", 0, PCut) ;
	InstallCBuiltinPred("host_speed", 1, PHostSpeed) ;
	InstallCBuiltinPred("show", 0, PShow) ;
	InstallCBuiltinPred("$$_default_boot", 0, PDefaultBoot) ;
}

