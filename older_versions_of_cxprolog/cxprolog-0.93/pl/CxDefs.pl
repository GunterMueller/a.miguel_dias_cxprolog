/*
 *   This file is part of the CxProlog system

 *   CxDefs.pl
 *   by A.Miguel Dias - 2001/01/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                                               %
  % The definitions in this file match the internal definitions   %
  % of some CxProlog predefined builtin predicates. This file was %
  % created so that you could analyse those definitions. Deleting %
  % this file causes no harm to CxProlog.                         %
  %                                                               %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* UTILITIES */

app([], L, L).
app([H|T], L, [H|R]) :-  app(T, L, R).

retractall(X) :- retract(X), fail.
retractall(X) :- retract((X:-_)), fail.
retractall(_).

findall(T,G,L) :-
	queue_new(Q),
	'$$_findall'(T, G, Q),
    queue_as_list(Q, L),
	queue_delete(Q).

'$$_findall'(T,G,Q) :-
    call(G), queue_put(Q,T), fail.
'$$_findall'(_,_,_).

setof(T,G,S) :-
	bagof(T, G, B),
	sort(B, S).

add_pl(user,user) :- !.
add_pl(user_input,user_input) :- !.
add_pl(A,A) :- slice(A,-3,-1,'.pl'), !.
add_pl(A,B) :- concat([A,'.pl'], B).


/* CONSULT / RECONSULT */

[].
[-File] :- !, reconsult(File).
[File] :- consult(File).
[File|Files] :- [File], Files.

consult(File0) :-
    add_pl(File0, File),
    Heap0 is heapused,
    Time0 is cputime,
    silent_consult(File),
    Time is cputime - Time0,
    Heap is heapused - Heap0,
    write(user,'File '), writeq(user, File),
    ('$$_is_reconsulting'
        -> write(user,' reconsulted ')
         ; write(user,' consulted ')),
    write(user,Heap), write(user,' bytes '),
    write(user,Time), writeln(user,' sec.').

reconsult(File0) :-
	'$$_enter_reconsulting',
	'$$_seq'(consult(File0), '$$_exit_reconsulting').

silent_consult(File0) :-
    add_pl(File0, File),
    seeing(SaveIn), see(File),
    '$$_c_loop',
    '$$_check_curr_unit_imports',
    seen, see(SaveIn).

silent_reconsult(File0) :-
	'$$_enter_reconsulting',
	'$$_seq'(silent_consult(File0), '$$_exit_reconsulting').

'$$_c_loop' :-
    repeat,
        read(Term),
    '$$_c_term'(Term), !.

'$$_c_term'(X) :- var(X), !, assertz(X).
'$$_c_term'(end_of_file) :- !.
'$$_c_term'(eof) :- !.
'$$_c_term'(unit U) :- !,
    create_unit(U), U>>'$$_c_loop',
    U>>'$$_check_curr_unit_imports'.
'$$_c_term'(visible Spec) :- !,
    '$$_visible'(Spec), fail.
'$$_c_term'(import Spec from U) :- !,
    '$$_import'(Spec,U), fail.
'$$_c_term'((:-X)) :- !, (:-X), !, fail.
'$$_c_term'((?-X)) :- !,
    varnames(V), question(X,V), !, fail.
'$$_c_term'(X) :- assertz(X), fail.

'$$_seq'(A,B) :- A, !, B, !.
'$$_seq'(A,B) :- B, !, fail.	


/* LISTING */

all :-
    current_unit(U),
        U>>listing,
    fail.
all.

list :- listing.

listing :-
    '$$_listing_header',
    '$$_listing_visibles',
    '$$_listing_imports',
    '$$_check_curr_unit_imports',
    '$$_listing_predicates', nl.

listing(F/A) :-
    listing(F,A).
listing(F,A) :-
    functor(H,F,A),
    clause(H,B),
        portray_clause((H:-B)),
    fail.
listing(F) :-
    current_predicate(H),
        functor(H,F,_),
        clause(H,B),
        portray_clause((H:-B)),
    fail.
listing(_).

'$$_listing_header' :-
    context_top(U),
    functor(U,F,A),
    write('**** '),
    writeq(unit F/A),
    writeln(' ****************').

'$$_listing_visibles' :-
    visible_predicate(H),
    functor(H,F,A),
    writeqln(visible F/A),
    fail.
'$$_listing_visibles'.

'$$_listing_imports' :-
    imported_predicate(H,U),
    functor(H,F,A),
    writeqln(import F/A from U),
    fail.
'$$_listing_imports'.

'$$_listing_predicates' :-
    current_predicate(H),
        clause(H,B),
            portray_clause((H:-B)),
    fail.
'$$_listing_predicates'.

portray_clause((H:-true)) :- !,
    numbervars(H, 0, _),
    writeq(H),writeln('.').
portray_clause((H:-B)) :-
    numbervars((H:-B), 0, _),
    writeq(H), writeln(' :-'),
    '$$_portray_body'(B),
    writeln('.').

'$$_portray_body'(X) :- var(X), !,
    tab(8), writeq(X).
'$$_portray_body'((X,Xs)) :-    !,
    tab(8), writeq(X), writeln(','),
    '$$_portray_body'(Xs).
'$$_portray_body'(X) :-
    tab(8), writeq(X).


/* QUESTION INTERACTION */

(:-G) :- G, !.
(:-G) :-
	write(user_error,'? '),
	writeln(user_error,G).

(?-G) :- G.

question((:-G),_) :- !, (:-G).
question(G,[]) :-
        G, !,
    writeln(user,yes).
question(G,[H|T]) :-
    G,
    '$$_show_vars'([H|T]),
    '$$_more', !.
question(_,_) :- writeln(user,no).

'$$_more' :- '$$_getnb'(X), '$$_more'(X).
'$$_more'(10) :- !, writeln(user,yes).
'$$_more'(0';) :- !, skip(user,10), fail.
'$$_more'(_) :-
    skip(user,10), writeln(user,yes).
'$$_getnb'(X) :-
    repeat, get0(user,X), X\= 32, !.

'$$_show_vars'([]).
'$$_show_vars'([S]) :- !,
    '$$_show_one_var'(S).
'$$_show_vars'([H|T]) :-
    '$$_show_one_var'(H), nl(user),
    '$$_show_vars'(T).
'$$_show_one_var'(Var=Val) :-
    write(user,Var),
    write(user,'='),
    writeq(user,Val).


/* BAGOF */

%	Adapted from C-Prolog: David Warren, Fernando Pereira, R.A.O'Keefe.

Variable^Goal :- Goal.

bagof(Template, Generator, Bag) :-
	'$$_excess_vars'(Generator, Template, [], FreeVars),
	FreeVars \== [], !,
	Key =.. ['$$_'|FreeVars],
	findall(Key-Template, Generator, Bags0),
	keysort(Bags0, Bags),
	'$$_pick'(Bags, Key, Bag).
bagof(Template, Generator, Bag) :-
	findall(Template, Generator, Bag0),
	Bag0 \== [],
	Bag = Bag0.

'$$_pick'(Bags, Key, Bag) :-
	Bags \== [],
	'$$_parade'(Bags, Key1, Bag1, Bags1),
	'$$_decide'(Key1, Bag1, Bags1, Key, Bag).

'$$_parade'([K-X|L1], K, [X|B], L) :- !,
	'$$_parade'(L1, K, B, L).
'$$_parade'(L, K, [], L).

'$$_decide'(Key, Bag, [], Key, Bag) :- !.
'$$_decide'(Key, Bag, Bags, Key, Bag).
'$$_decide'(_, _, Bags, Key, Bag) :-
	'$$_pick'(Bags, Key, Bag).

'$$_excess_vars'(T, X, L0, L) :-
	var(T), !,
	'$$_excess_vars2'(T, X, L0, L).
'$$_excess_vars2'(T, X, L0, L) :-
	not subterm(T, X), !,
	'$$_introduce'(T, L0, L).
'$$_excess_vars2'(T, X, L0, L0).
'$$_excess_vars'(X^P, Y, L0, L) :- !,
	'$$_excess_vars'(P, (X,Y), L0, L).
'$$_excess_vars'(setof(X,P,S), Y, L0, L) :- !,
	'$$_excess_vars'((P,S), (X,Y), L0, L).
'$$_excess_vars'(bagof(X,P,S), Y, L0, L) :- !,
	'$$_excess_vars'((P,S), (X,Y), L0, L).
'$$_excess_vars'(T, X, L0, L) :-
	functor(T, _, N),
	'$$_rem_excess_vars'(N, T, X, L0, L).

'$$_rem_excess_vars'(0, _, _, L, L) :- !.
'$$_rem_excess_vars'(N, T, X, L0, L) :-
	succ(M, N),
	arg(N, T, T1),
	'$$_excess_vars'(T1, X, L0, L1),
	'$$_rem_excess_vars'(M, T, X, L1, L).

'$$_introduce'(X, L, L) :-
	subterm(X, L), !.
'$$_introduce'(X, L, [X|L]).
