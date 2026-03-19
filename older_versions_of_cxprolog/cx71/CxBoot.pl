/*
 *   This file is part of the CxProlog system

 *   CxBoot.pl
 *   by A.Miguel Dias - 2000/04/02
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

% ACTIVATION

'@@_called_from_c'(X) :- X, '@@_event'(2).
'@@_called_from_c'(_) :- '@@_event'(3).
'@@_called_from_c'(_).

% Normal startup
'@@_lux0' :-
	/* [you may consult extra system files here], */
	create_unit(main),
	create_ivar('@@_env_context'),
	'@@_user_mode',
	push main.

% Restart after error or restart/0 call
'@@_lux1' :-
	'@@_env_context' =: C,
	call_on_context(C,'$$_top_level').

% TOP LEVEL

'$$_top_level' :-
	repeat,
		'@@_prompt',	% the prompt depends on the term in '@@_env_context'
		read(G,V), 
		'$$_try'(G,V),
	fail.

'$$_try'(end_of_file,_) :- !, writeln('^D'), halt.
'$$_try'(G,[]) :- '$$_top_goal'(G), !, '$$_writeln_silent'(yes).
'$$_try'(G,[H|T]) :- '$$_top_goal'(G), '$$_showVars'([H|T]), '$$_more', !.
'$$_try'(_,_) :- '$$_writeln_silent'(no).

'$$_top_goal'(G) :-
	'@@_set_in_top_goal'(true),
	G,
	('@@_set_in_top_goal'(false);
		('@@_set_in_top_goal'(true), fail)).
'$$_top_goal'(_) :-
	'@@_set_in_top_goal'(false),
	fail.

'$$_more' :- get(X), '$$_more'(X).
'$$_more'(10) :- !, '$$_writeln_silent'(yes).
'$$_more'(0';) :- !, skip(10), fail.
'$$_more'(_) :- skip(10), '$$_writeln_silent'(yes).

'$$_showVars'([]).
'$$_showVars'([S]) :- !, '$$_showOneVar'(S).
'$$_showVars'([H|T]) :- '$$_showOneVar'(H), nl, '$$_showVars'(T).
'$$_showOneVar'(Var=Val) :- write(Var), write('='), writeq(Val).

'$$_writeln_silent'(_) :- check_silent, !.
'$$_writeln_silent'(X) :- writeln(X).

push U :-
	U>>true,	% validate U
	'@@_env_context' =: C,
	'@@_env_context' := [U|C],
	restart.
pop :-
	'@@_env_context' =: [_|C],
	'@@_env_context' := C,
	restart.



% OPERATORS

current_op(Priority, Type, Operator) :-
	'@@_current_op_aux'(Operator),
	'$$_op'(Type, Num),
	'@@_is_op'(Operator, Num, Priority, Left, Right),
	'$$_op'(Type, Priority, Left, Right).

'$$_op'( fx, 0).
'$$_op'( fy, 0).
'$$_op'(xf, 2).
'$$_op'(yf, 2).
'$$_op'(xfy, 1).
'$$_op'(xfx, 1).
'$$_op'(yfx, 1).

'$$_op'( fx, Q, _, P) :- succ(P, Q).
'$$_op'( fy, Q, _, Q).
'$$_op'(xf,  Q, P, _) :- succ(P, Q).
'$$_op'(yf,  Q, Q, _).
'$$_op'(xfy, Q, P, Q) :- succ(P, Q).
'$$_op'(xfx, Q, P, P) :- succ(P, Q).
'$$_op'(yfx, Q, Q, P) :- succ(P, Q).



% SOME DEFINITIONS

writeln(X) :- write(X), nl.
twriteln(X) :- tab(4), write(X), nl.
ttwriteln(X) :- tab(8), write(X), nl.
displayln(X) :- display(X), nl.
writeqln(X) :- writeq(X), nl.
code(F,A) :- '@@_code'(F,A).
code(F/A) :- '@@_code'(F,A).
code(F) :- '@@_code'(F).

halt :- '@@_event'(4).
restart :- '@@_event'(1).

call(X) :- X.
call(Pred,Args) :- X =.. [Pred|Args], X.

not X :- X, !, fail.
not _.

If -> Then :- If, !, Then.
If -> Then.

If -> Then ; Else :- If, !, Then.
If -> Then ; Else :- Else.

A;_ :- A.
_;B :- B.

A,B :- A, B.

(:- X) :- X.
(?- X) :- X.

X === L :- concatt(L,X).

T =.. [F|As] :- '$$_=..'(As,T,F,0).
'$$_=..'([],T,F,N) :- functor(T,F,N), !.
'$$_=..'([A|As],T,F,N) :- N1 is N+1, '$$_=..'(As,T,F,N1), !, arg(N1,T,A).

[].
[File|Files] :- consult([File|Files]).

(A :- B) :-
    assertz((A :- B)),
    write((A :- B)), writeln(' asserted.'),
    restart.

app([], L, L).
app([H|T], L, [H|R]) :-
    app(T, L, R).

system_predicate(X) :-
	builtin_predicate(X).


% CONTEXTS

% context extension
U>>X :- '@@_primitive'(1).

% switching of top of context
U<>X :- '@@_primitive'(2).

% context freeing
call_on_empty_context(X) :-	 '@@_primitive'(3).

% context setting
call_on_context([],X) :- call_on_empty_context(X).
call_on_context([U|C],X) :- call_on_context(C,U>>X).

down(X) :- builtin<>X.

show_context :- context(X), writeln(X).

unit(U) :- context([U|_]).

unit_param(I,P) :- context([U|_]), arg(I,U,P).
	
unit_arity(A) :- context([U|_]), functor(U,_,A).

units :-
	current_unit(U),
	twriteln(U),
	fail.
units.



% HISTORIC CONTEXT

% historic context push
>X :- '@@_primitive'(4).

% historic context enter
<X :- '@@_primitive'(5).

show_hcontext :- hcontext(X), writeln(X).



% CONSULT / RECONSULT

consult(X) :- '$$_do_consult'(X, false).
reconsult(X) :- '$$_do_consult'(X, true).

'$$_do_consult'([],_) :- !.
'$$_do_consult'(-File,_) :- !,
	'$$_do_consult'(File,true).
'$$_do_consult'([File|Files],Re) :- !,
	'$$_do_consult'(File,Re),
	'$$_do_consult'(Files,Re).
'$$_do_consult'(File0,Re) :-
	'$$_add_pl'(File0, File),
	Heap0 is heapused,
	Time0 is cputime,
	seeing(SaveIn), see(File),
	'$$_consult_loop'(Re),
	abolish('$$_retract_done', 2),
	seen, see(SaveIn),
	check_imports,
	Time is cputime - Time0,
	Heap is heapused - Heap0,
	telling(SaveOut), tell(user),
	write('File '), write(File),
	(Re==true -> write(' reconsulted ') ; write(' consulted ')),
	write(Heap), write(' bytes '),
	write(Time), writeln(' sec.'),
	told, tell(SaveOut).

'$$_consult_loop'(false) :- repeat, read(Term,Vars), '$$_consult_term'(Term,Vars), !.
'$$_consult_loop'(true) :- repeat, read(Term,Vars), '$$_reconsult_term'(Term,Vars), !.

'$$_consult_term'(end_of_file,_) :- !.
'$$_consult_term'(eof,_) :- !.
'$$_consult_term'(unit U,Vars) :- !,
	'$$_bind_names'(Vars), writeln(unit U), create_unit(U), U>>'$$_consult_loop'(false).
'$$_consult_term'(visible Spec,_) :- !, visible Spec, fail.
'$$_consult_term'(import Spec from U,_) :- !, import Spec from U, fail.
'$$_consult_term'(ivar Spec,_) :- !, ivar Spec, fail.
'$$_consult_term'(op(A,B,C),_) :- !, op(A,B,C), fail.
'$$_consult_term'((?-X,_)) :- !, X, !, fail.
'$$_consult_term'((:-X),_) :- !, X, !, fail.
'$$_consult_term'(X,_) :- assertz(X), fail.

'$$_reconsult_term'(end_of_file,_) :- !.
'$$_reconsult_term'(eof,_) :- !.
'$$_reconsult_term'(unit U,Vars) :- !, abolish('$$_retract_done', 2),
	'$$_bind_names'(Vars), writeln(unit U), create_unit(U), U>>'$$_consult_loop'(true).
'$$_reconsult_term'(visible Spec,_) :- !, '$$_retract_spec'(Spec), visible Spec, fail.
'$$_reconsult_term'(import Spec from U,_) :- !, '$$_retract_spec'(Spec), import Spec from U, fail.
'$$_reconsult_term'(ivar Spec,_) :- !, ivar Spec, fail.
'$$_reconsult_term'(op(A,B,C),_) :- !, op(A,B,C), fail.
'$$_reconsult_term'((?-X),_) :- !, X, !, fail.
'$$_reconsult_term'((:-X),_) :- !, X, !, fail.
'$$_reconsult_term'((H:-B),_) :- !, functor(H,N,A), '$$_cond_retract'(N,A), assertz((H:-B)), fail.
'$$_reconsult_term'(H,_) :- functor(H,N,A), '$$_cond_retract'(N,A), assertz(H), fail.

'$$_bind_names'([Name=Name|Rest]) :- '$$_bind_names'(Rest).
'$$_bind_names'([]).

visible [] :- !.
visible [H|T] :- !, visible H, visible T.
visible N/A :- !, visible(N,A).
visible _ :- write('Invalid specification for visible/1'), restart.

import [] from U :- !.
import [H|T] from U :- !, import H from U, import T from U.
import N/A from U :- !, import(N,A,U).
import _ from _ :- write('Invalid specification for import/2'), restart.

ivar [] :- !.
ivar [H|T] :- !, ivar H, ivar T.
ivar N :- !, create_ivar(N).
ivar _ :- write('Invalid specification for ivar/1'), restart.

'$$_retract_spec'([]) :- !.
'$$_retract_spec'([H|T]) :- !, '$$_retract_spec'(H), '$$_retract_spec'(T).
'$$_retract_spec'(N/A) :- !, '$$_cond_retract'(N,A).
'$$_retract_spec'(_) :- write('Invalid specification for $retract_spec/1'), restart.

'$$_cond_retract'(N,A) :- '$$_retract_done'(N,A), !.
'$$_cond_retract'(N,A) :- asserta('$$_retract_done'(N,A)), abolish(N,A).

% add ".pl" to the name if necessary
'$$_add_pl'(user,user) :-	!.
'$$_add_pl'(A,B) :-
	name(A,A1), ( app(_,".pl",A1) -> A = B ; (app(A1, ".pl", B1), name(B,B1)) ).



% RETRACTALL

retractall(X) :- retract(X), fail.
retractall(X) :- retract((X:-_)), fail.
retractall(_).



% FINDALL

findall(X,G,_) :-
	asserta('$$_findall_found'('$$_findall_mark')),
	call(G),
	asserta('$$_findall_found'(X)),
	fail.
findall(_,_,L) :-
	'$$_findall_collect'([],M),
	!,
	L=M.

'$$_findall_collect'(S,L) :-
	'$$_findall_getnext'(X), !,
	'$$_findall_collect'([X|S],L).
'$$_findall_collect'(L,L).

'$$_findall_getnext'(X) :-
	retract('$$_findall_found'(X)),
	!,
	X \== '$$_findall_mark'.



% LISTING

all :-
	current_unit(U),
	U>>'$$_do_listing',
	fail.
all :-
	check_imports.

list :-
	listing.
listing :-
	'$$_do_listing',
	check_imports.

builtins :-
	builtin>>locals.

locals :-
	'$$_listing_header',
	'$$_listing_visibles',
	'$$_listing_imports',
	'$$_listing_ivars',
	'$$_listing_ops'.

'$$_do_listing' :-
	locals,
	nl,
	'$$_listing_predicates',
	nl.

listing(F/A) :-
	listing(F,A).
listing(F,A) :-
	functor(H,F,A),
	clause(H,B),
	'$$_listing_clause'(H,B),
	fail.
listing(F) :-
	current_predicate(H),
	functor(H,F,_),
	clause(H,B),
	'$$_listing_clause'(H,B),
	fail.
listing(_).
	
'$$_listing_header' :-
	unit(U),
	write('**** '),
	write(unit U),
	writeln(' ****************').

'$$_listing_visibles' :-
	visible_predicate(H),
	functor(H,F,A),
	writeln(visible F/A),
	fail.
'$$_listing_visibles'.

'$$_listing_imports' :-
	imported_predicate(H,U),
	functor(H,F,A),
	writeln(import F/A from U),
	fail.
'$$_listing_imports'.

'$$_listing_ivars' :-
	current_ivar(V),
	writeln(ivar V),
	fail.
'$$_listing_ivars'.

'$$_listing_ops' :-
	current_op(A,B,C),
	writeqln(op(A,B,C)),
	fail.
'$$_listing_ops'.

'$$_listing_predicates' :-
	current_predicate(H),
	clause(H,B),
	'$$_listing_clause'(H,B),
	fail.
'$$_listing_predicates'.

'$$_listing_clause'(H,true) :-
	!,
	write(H),
	writeln('.').
'$$_listing_clause'(H,B) :- write(H),
	writeln(' :-'),
	'$$_listing_body'(B),
	writeln('.').

'$$_listing_body'(X) :-
	var(X),
	!,
	ttwrite(X).
'$$_listing_body'((X,Xs)) :-
	!,
	ttwrite(X),
	writeln(','),
	'$$_listing_body'(Xs).
'$$_listing_body'(X) :-
	ttwrite(X).
