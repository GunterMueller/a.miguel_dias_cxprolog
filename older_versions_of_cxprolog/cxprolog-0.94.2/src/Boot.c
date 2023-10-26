/*
 *   This file is part of the CxProlog system

 *   Boot.c
 *   by A.Miguel Dias - 2003/06/11
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

/* The following built-in predicates should not be redefined, not even inside
	an alternative boot file */

static CharPt coreBuiltinsStr = "				\
												\
/* ACTIVATION */							  ""\
												\
'$$_call_prolog'(G) :- 							\
	once G, '$$_set_success', fail.				\
'$$_call_prolog'(_) :- '$$_return'.				\
												\
'$$_run_thread'(G) :-							\
	once G,										\
	'$$_active_thread_set_success', fail.		\
'$$_run_thread'(_) :-							\
	'$$_active_thread_return'.					\
                    							\
'$$_lux0' :-									\
	'$$_stress_testing',						\
	try (os_arg('-quiet','')					\
	 ->	set_prolog_flag(memory_warnings,off),	\
		set_prolog_flag(info_messages,off)),	\
	(os_arg('-boot',X)							\
			-> silent_consult(X)				\
			; '$$_default_boot'),				\
	dynamic_iu('$$_top_call'/1), /* dynamic built-in */	\
	'$$_enter_user_mode',						\
	dict_new('$$_gensym_dict'),					\
	create_unit(main),							\
	try main>>'$cxprolog_initialise',			\
	try (os_arg('-check',_)						\
			-> main>>'$$_check_instalation'),	\
	try (os_arg('-script',X)					\
			-> main>>silent_consult(X)),		\
	restart.									\
												\
'$$_lux1' :-									\
	try main>>'$cxprolog_top_level_goal',		\
	restart.									\
												\
'$$_stress_testing' :-							\
	set_prolog_flag(test_relocation,off),							\
    set_prolog_flag(test_garbage_collection,off),					\
 	try (current_prolog_flag(test_relocation,on)					\
	   -> writeln('*** STRESS-TESTING RELLOCATION ***')),			\
	try (current_prolog_flag(test_garbage_collection,on)			\
	   -> writeln('*** STRESS-TESTING GARBAGE COLLECTION ***')).	\
												\
/* CORE PREDICATES	*/						  ""\
												\
\\+ G :- G, !, fail.							\
\\+ _.											\
												\
not G :- \\+ G.									\
												\
try G :- G, !.									\
try G.											\
												\
once G :- G, !.									\
												\
gen G :- G, fail.								\
gen G.											\
												\
possible G :- \\+ G, !, fail.					\
possible _.										\
											  	\
with_ivar(I,V,G) :-								\
	'$$_ivar_get'(I,X), I &:= V, G, I &:= X.	\
												\
If -> Then :- If, !, Then.						\
If -> Then.	/* PATCHED */						\
If -> Then ; Else :- If, !, Then.				\
If -> Then ; Else :- !, Else.					\
												\
A;_ :- A.										\
_;B :- B.										\
												\
A,B :- A, B.								  ""\
												\
T =.. [F|As] :- '$$_univ'(As,T,F,0).			\
'$$_univ'([],T,F,N) :- functor(T,F,N), !.		\
'$$_univ'([A|As],T,F,N) :-						\
	'$$_sane_arity'(N),							\
	N1 is N+1, '$$_univ'(As,T,F,N1), !,			\
	arg(N1,T,A).								\
												\
(A :- true) :- !, '$$_user_assert'(A).			\
(A :- B) :- '$$_user_assert'((A :- B)).			\
'$$_user_assert'(C) :-							\
	assertz(C),									\
	numbervars(C,0,_),							\
	writeq(user,C),								\
	writeln(user,' asserted.'),					\
	restart.									\
												\
'$$_check_curr_unit_missing' :-					\
	predicate_property(X, undefined),			\
	predicate_property(X, full_name(Pred)),		\
	warning(['Predicate ''', Pred, ''' is used',\
		' but undefined in its unit']),			\
	fail.										\
'$$_check_curr_unit_missing'.					\
												\
check_missing :-								\
	current_unit(U),							\
		U>>'$$_check_curr_unit_missing',		\
	fail.										\
check_missing.									\
											  ""\
'$$_check_curr_unit_imports'(H) :-				\
	predicate_property(H, imported_from(U)),	\
	predicate_property(H, full_name(Pred)),		\
	functor(U,NU,AU), functor(UU,NU,AU),		\
	(current_unit(UU)							\
	->  (U>>predicate_property(H,visible)		\
		-> true									\
		; warning(['Imported predicate ''',		\
				Pred, ''' is not visible in ',	\
				'unit ''', NU/AU, ''''])		\
		)										\
	; warning(['Predicate ''', Pred, ''' is ',	\
		'imported from nonexistent unit ''',	\
		NU/AU, ''''])							\
	),											\
	fail.										\
'$$_check_curr_unit_imports'(_).				\
												\
check_imports :-								\
	current_unit(U),							\
		U>>'$$_check_curr_unit_imports'(_),		\
	fail.										\
check_imports.									\
												\
check_globals.	/* TODO */						\
												\
visible_predicate(P) :-							\
	predicate_property(P, visible).				\
imported_predicate(P,UD) :-						\
	predicate_property(P, imported_from(UD)).	\
builtin_predicate(P) :-							\
	predicate_property(P, built_in).			\
system_predicate(P) :- 							\
	predicate_property(P, built_in).			\
												\
mutable_builtin(N,A) :-							\
	dynamic(N/A).								\
												\
nodebug :- set_prolog_flag(debug,0).			\
notrace :- set_prolog_flag(debug,0).			\
debug :- set_prolog_flag(debug,1).				\
trace :- set_prolog_flag(debug,2).				\
											  ""\
zpl :-											\
	fs_home,									\
	fs_exists(pl)								\
		-> fs_cd(X,[pl|X])						\
		; fs_cd([_|X],[pl|X]).					\
												\
/* CONTEXTS	*/								  ""\
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
call_on_context([],G) :-						\
	call_on_empty_context(G).					\
call_on_context([U|C],G) :-						\
	call_on_context(C,U>>G).					\
												\
import P from U	:- import(P,U).					\
												\
/* HISTORIC CONTEXT	*/						  ""\
												\
show_hcontext :- hcontext(X), writeqln(X).		\
show_hcontext(S) :-								\
	hcontext(X), writeqln(S,X).					\
												\
/* OPERATORS */								  ""\
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
/* CHECK INSTALATION */						  ""\
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
" ;


/* The following built-in predicates may be redefined inside
   an alternative boot file */

static CharPt predefinedBuiltinsStr = "			\
												\
/* UTILITIES */								  ""\
												\
app([], L, L).									\
app([H|T], L, [H|R]) :- app(T, L, R). 			\
												\
forall(G1, G2) :- \\+ (G1, \\+ G2).				\
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
	forall(G, queue_put(Q,T)),					\
	queue_as_list(Q, L),						\
	queue_delete(Q).							\
												\
setof(T,G,S) :-									\
	bagof(T, G, B),								\
	sort(B, S).									\
												\
gensym(Prefix, Atom) :-							\
    (dict_get('$$_gensym_dict',Prefix,N)		\
		-> true ; N = 0),						\
    succ(N,M),									\
    dict_set('$$_gensym_dict',Prefix,M),		\
    concat([Prefix,M], Atom).					\
												\
gensym(Atom) :- gensym('%', Atom).				\
												\
add_pl(user,user) :- !.							\
add_pl(user_input,user) :- !.					\
add_pl(A,A) :- slice(A,-3,-1,'.pl'), !.			\
add_pl(A,B) :- concat([A,'.pl'], B).			\
												\
/* CONSULT */								  ""\
												\
[].												\
[-File|Files] :- !, reconsult(File), Files.		\
[File|Files] :- consult(File), Files.			\
												\
compile(Files) :- consult(Files).				\
												\
consult(Files) :-								\
	'$$_map_consult'(Files,						\
			'$$_consult_one',false).			\
												\
reconsult(Files) :-								\
	'$$_map_consult'(Files,						\
			'$$_consult_one',true).				\
												\
silent_consult(Files) :-						\
	'$$_map_consult'(Files,						\
			'$$_silent_consult_one',false).		\
												\
silent_reconsult(Files) :-						\
	'$$_map_consult'(Files,						\
			'$$_silent_consult_one',true).		\
												\
'$enter_consult'(S) :-							\
	'$$_enter_consult'(S,false).				\
												\
'$$_map_consult'([],_,_) :- !.					\
'$$_map_consult'([File|Files],G,B) :- !,		\
	'$$_map_consult'(File,G,B),					\
	'$$_map_consult'([File|Files],G,B).			\
'$$_map_consult'(File,G,B) :-					\
	call(G,File,B).								\
												\
'$$_consult_one'(File,B) :-						\
	current_prolog_flag(info_messages,off)		\
		-> '$$_silent_consult_one'(File,B)		\
		; '$$_loud_consult_one'(File,B).		\
												\
'$$_silent_consult_one'(File,B) :-				\
	'$$_check_consult'(B),						\
	add_pl(File, F),							\
	( F == user -> S = user ; open(F,read,S) ),	\
 /* now skip #!cxprolog ... first line */		\
	'$$_enter_consult'(S,B),					\
	( peek(S,35) -> get_line(S,_) ; true ),		\
	'$$_c_loop',								\
	'$exit_consult',							\
	close(S).									\
												\
'$$_loud_consult_one'(File0,B) :-				\
	add_pl(File0, File), 						\
	Heap0 is heapused,							\
	Time0 is cputime,							\
	'$$_silent_consult_one'(File,B),			\
	Time is cputime - Time0,					\
	Heap is heapused - Heap0,					\
	write(user,'File '), writeq(user,File),		\
	(B==true									\
        -> write(user,' reconsulted ')			\
         ; write(user,' consulted ')),			\
	write(user,Heap), write(user,' bytes '),	\
	write(user,Time), writeln(user,' sec.').	\
											  ""\
'$$_c_loop' :-									\
	consulting(S),								\
	repeat,										\
		read(S, Term),							\
	'$$_c_term'(Term), !.						\
												\
'$$_c_term'(X) :- var(X), !,					\
	'$consult_clause'(X).						\
'$$_c_term'(end_of_file) :- !.					\
'$$_c_term'(eof) :- !.							\
'$$_c_term'((unit U)) :- !,						\
	varnames(Vars), '$bind'(Vars),				\
	create_unit(U), U>>'$$_c_loop'.				\
'$$_c_term'((multifile Spec)) :- !,				\
	multifile(Spec), fail.						\
'$$_c_term'((visible Spec)) :- !,				\
	visible(Spec), fail.						\
'$$_c_term'((gopen Spec)) :- !,					\
	gopen(Spec), fail.							\
'$$_c_term'((gclose Spec)) :- !,				\
	gclose(Spec), fail.							\
'$$_c_term'((import Spec from U)) :- !,			\
	import(Spec,U), fail.						\
'$$_c_term'((dynamic Spec)) :- !,				\
	dynamic(Spec), fail.						\
'$$_c_term'((dynamic_iu Spec)) :- !,			\
	dynamic_iu(Spec), fail.						\
'$$_c_term'((:-X)) :- !, (:-X), !, fail.		\
'$$_c_term'((?-X)) :- !,						\
	varnames(V), question(X,V), !, fail.		\
'$$_c_term'(X) :- '$consult_clause'(X), fail.	\
												\
'$$_seq'(A,B) :- A, !, B, !.					\
'$$_seq'(A,B) :- B, !, fail.					\
												\
												\
/* LISTING */								  ""\
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
	'$$_listing_header','$$_listing'(_).		\
												\
listing(V) :- var(V),!,'$$_listing_clauses'(V).	\
listing(A) :- atom(A), !, '$$_listing_atom'(A).	\
listing(F/N) :- !, listing(F,N).				\
listing(H) :- '$$_listing_clauses'(H).			\
listing(F,N) :-									\
	functor(H,F,N), '$$_listing'(H).			\
												\
'$$_listing_header' :-							\
	unit_spec(USpec)							\
		->	write('**** '),						\
			write(unit USpec),					\
			writeln(' ****************')		\
		;	true.								\
											  ""\
'$$_listing_atom'(A) :-							\
	forall(current_predicate(A,H), '$$_listing_multifiles'(H)),		\
	forall(current_predicate(A,H), '$$_listing_visibles'(H)),		\
	forall(current_predicate(A,H), '$$_listing_gopen'(H)),			\
	forall(current_predicate(A,H), '$$_listing_gclose'(H)),			\
	forall(current_predicate(A,H), '$$_listing_imports'(H)),		\
	forall(current_predicate(A,H), '$$_listing_dynamics'(H)),		\
	forall(current_predicate(A,H), '$$_listing_dynamics_iu'(H)),	\
	forall(current_predicate(A,H), '$$_listing_clauses'(H)), nl.	\
												\
'$$_listing'(H) :-								\
	'$$_listing_multifiles'(H),					\
	'$$_listing_visibles'(H),					\
	'$$_listing_gopen'(H),						\
	'$$_listing_gclose'(H),						\
	'$$_listing_imports'(H),					\
	'$$_listing_dynamics'(H),					\
	'$$_listing_dynamics_iu'(H),				\
	'$$_listing_clauses'(H), nl.				\
												\
'$$_listing_kind'(H,K) :-						\
	queue_new(Q),								\
	forall(predicate_property(H, K),			\
		(functor(H,F,A), queue_put(Q,F/A))),	\
	(queue_as_seq(Q,S)							\
	 -> write(':-'), write(K), write(' '),	  ""\
		write(S), writeln('.')					\
	 ;  true),									\
	 queue_delete(Q).							\
												\
'$$_listing_multifiles'(H) :-					\
	'$$_listing_kind'(H, multifile).			\
												\
'$$_listing_visibles'(H) :-						\
	'$$_listing_kind'(H, visible).				\
												\
'$$_listing_imports'(H) :-						\
	predicate_property(H, imported_from(U)),	\
	functor(H,F,A),								\
	writeq((:-import F/A from U)), writeln('.'),\
	'$$_check_curr_unit_imports'(H),			\
	fail.										\
'$$_listing_imports'(_).						\
												\
'$$_listing_gopen'(H) :-						\
	'$$_listing_kind'(H, gopen).				\
												\
'$$_listing_gclose'(H) :-						\
	'$$_listing_kind'(H, gclose).				\
												\
'$$_listing_dynamics'(H) :-						\
	'$$_listing_kind'(H, dynamic).				\
												\
'$$_listing_dynamics_iu'(H) :-					\
	'$$_listing_kind'(H, dynamic_iu).			\
												\
'$$_listing_clauses'(H) :-						\
	current_predicate(_, H),				  ""\
		clause(H,B),							\
			portray_clause((H:-B)),				\
	fail.										\
'$$_listing_clauses'(_).						\
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
/* QUESTION INTERACTION */					  ""\
												\
(:-G) :- G, !.									\
(:-G) :-										\
	write(user_error,'? '),						\
	writeqln(user_error,G).						\
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
	write(user,' = '),							\
	writeq(user,Val),							\
	write(user,' ').							\
												\
/* BAGOF									  ""\
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
											  ""\
'$$_excess_vars'(T, X, L0, L) :-				\
	var(T), !,									\
	'$$_excess_vars2'(T, X, L0, L).				\
'$$_excess_vars2'(T, X, L0, L) :-				\
	\\+ subterm(T, X), !,						\
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
" ;


/* The following built-in predicates are defaults that will be
   used only if no alternative boot file is provided */

static CharPt defaultBuiltinsStr = "			\
												\
'$cxprolog_initialise':-						\
	try (current_prolog_flag(info_messages,on)	\
								-> version),	\
	'$env_context' := [main].					\
												\
'$cxprolog_top_level_goal' :-					\
	'$env_context' =: C,						\
	call_on_context(C,'$top_level2').			\
												\
/* TOP LEVEL */								  ""\
												\
'$top_level1' :-								\
  ( current_prolog_flag(debug,1)				\
  				-> write(user,'(debug) ')		\
  ; current_prolog_flag(debug,2)				\
  				-> write(user,'(trace) ')		\
  ; true ),										\
	context(C), writeq(user,C),					\
	write(user,' ?- '),							\
	read(user,G), varnames(V),					\
	question(G,V).								\
												\
'$top_level2' :-							  ""\
  ( current_prolog_flag(debug,1)				\
  				-> write(user,'(debug) ')		\
  ; current_prolog_flag(debug,2)				\
  				-> write(user,'(trace) ')		\
  ; true ),										\
	context(C), writeq(user,C),					\
	write(user,' ?- '),							\
	read(user,G), varnames(V), 					\
	abolish('$$_top_call'/1),					\
	assert(('$$_top_call'(V):-G)),				\
	>question('$$_top_call'(V),V).				\
												\
/* HANDLING CONTEXT */						  ""\
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
pwd :- fs_cd(X), writeqln(X).					\
" ;

void Boot(int argc, CharPt argv[])
{
	CmdLineInit(argc, argv) ;
	StringInit() ;
	ExtraInit() ;
	AtomsInit() ;	/* Must be just after ExtraInit() so that ExtraAtomTag == 0 */
	NumbersInit() ;	/* Must be just after AtomsInit() so that ExtraAtomTag == 1 */
	LocaleInit() ;
	FilesInit() ;
	StreamsInit() ;
	YourPrologue() ;
	InterruptOff() ;
	ScratchInit() ;
	CheckHost() ;  /* Must be after ScratchInit() */
	ClockInit() ;
	IndexInit() ;
	VarDictInit() ;
	FunctorsInit() ;
	InstructionsInit() ;
	MachineInit() ;
	ContextsInit() ;
	UnitsInit() ;
	CompilerInit() ;
	MesgInit() ;
	SysTraceInit() ;
/* C predicates may be installed from here onwards */

	MachineInit2() ;
	ExtraInit2() ;
	AtomsInit2() ;
	LocaleInit2() ;
	ScratchInit2() ;
	InstructionsInit2() ;
	ContextsInit2() ;
	UnitsInit2() ;
	ExceptionsInit() ;
	GCollectionInit() ;
	NumbersInit2() ;
	FlagsInit() ;
	DebugInit() ;
	DisassemblerInit() ;
	TermsInit() ;
	ArithInit() ;
	UnifyInit() ;
	OperatorsInit() ;
	PredicatesInit() ;
	PredicatePropertyInit() ;
	ClausesInit() ;
	ThreadsInit() ;
	TermReadInit() ;
	TermWriteInit() ;

	IVarsInit() ;
	CharactersInit() ;
	FileSysInit() ;
	StreamsInit2() ;
	StreamPropertyInit() ;
	QueuesInit() ;
	StacksInit() ;
	DictsInit() ;;
	ArraysInit() ;
	BuffersInit() ;
	NetInit() ;
	ProcessesInit() ;
	BootInit() ;
	ConsultInit() ;
	CmdLineInit2() ;
	VersionInit() ;
	CallPrologInit() ;

/* Create root threads and initialize abstract machine */
	ThreadRootCreate(MakeAtom("$$_lux0"), MakeAtom("$$_lux1")) ;
	ZBasicLoadStr(coreBuiltinsStr) ;
	CompatibleIfThenUpdateFlags(compatibleIfThen_flag) ;
	ZBasicLoadStr(predefinedBuiltinsStr) ;

	JavaInit() ;
	JavaAddOnsInit() ;

	WxWidgetsInit() ;
	WxAddOnsInit() ;
	WxWidgetsDemoInit() ;

	GuiEventInit() ;
	GuiInit() ;

	MarkStaticBuiltinsAsPermanent() ;	
	ZCheckHostSpeed() ;	
	YourExtensions() ;
	InterruptOn() ;
}


/* CXPROLOG C'BUILTINS */

static void PEnterUserMode()
{
	if( !Booting() )
		FatalError("Cannot be activated twice") ;
	MarkStaticBuiltinsAsPermanent() ;
	UnitIsPermanent(systemUnit) = true ;
	bottomUnit = emptyUnit ;
	Booting() = false ;
	JumpNext() ;
}

static void PDefaultBoot()
{
	ZBasicLoadStr(defaultBuiltinsStr) ;
	JumpNext() ;
}

static void PEndOfFile()
{
	WriteStd("^D\n") ;
	EventHalt() ;
	JumpNext() ;
}

static void POSName()
{
	MustBe( Unify(X0, MakeAtom(OSName())) ) ;
}

static void PShow()
{
	ShowVersion() ;
	Write("Show what?\n") ;
	Write(" arrays.  atoms.    builtins.  char_reorder.  dicts.    flags.\n") ;
    Write(" floats.  ivars.    locale.    ops.           queues.   statistics.\n") ;
	Write(" stacks.  streams.  threads.   units.         version.\n") ;
	JumpNext() ;
}

static void POSDependencies()
{
	int i ;
	ShowVersion() ;
	Write("OS/Machine dependencies:\n") ;

	
#if ULONG_MAX == 0xFFFFFFFFL
	Write("   32 BITS\n") ;
#else
	Write("   64 BITS\n") ;
#endif	
#if USE_THREADED_CODE
	Write("   USE_THREADED_CODE\n") ;
#endif
#if UNDERSTAND_EXTERNAL_ENCODINGS
	Write("   UNDERSTAND_EXTERNAL_ENCODINGS\n") ;
#endif
#if USE_JAVA
	Write("   USE_JAVA\n") ;
#endif
#if USE_WXWIDGETS
	Write("   USE_WXWIDGETS\n") ;
#endif

	Write("   Memory allocation policy = %s\n",
				UsingMMap() ? "MMAP" : "MALLOC") ;

	i = CalculateFloatSize() ;
	Write("   Float size = %d", i) ;
	if( i <= 32 ) Write(" (BAD: No double precision available!)") ;
	elif( i <= 64 ) Write(" (BAD: No extended precision available!)") ;
	Write("\n") ;
	JumpNext() ;
}

static void PLoadLib()
{
#ifdef PREFIX
	CharPt libName = PREFIX "/lib/cxprolog/lib.pl" ;	
	if( !OSExists(libName) )
		Error("Cannot access '%s'", libName) ;
	ZBasicLoadFile(libName) ;
	if( infoMessages_flag )
		Write("'%s' loaded.\n", libName) ;
#else
	Error("Library unavailable") ;
#endif
	JumpNext() ;
}

static void PCxProlog()
{
	JumpNext() ;
}

void BootInit()
{
	InstallCBuiltinPred("$$_enter_user_mode", 0, PEnterUserMode) ;
	InstallCBuiltinPred("$$_default_boot", 0, PDefaultBoot) ;
	InstallCBuiltinPred("end_of_file", 0, PEndOfFile) ;

	InstallCBuiltinPred("os_name", 1, POSName) ;
	InstallCBuiltinPred("show", 0, PShow) ;
	InstallCBuiltinPred("os_dep", 0, POSDependencies) ;
	InstallCBuiltinPred("load_lib", 0, PLoadLib) ;
	InstallCBuiltinPred("cxprolog", 0, PCxProlog) ;
}
