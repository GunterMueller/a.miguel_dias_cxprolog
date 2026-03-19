/*
 *   This file is part of the CxProlog system

 *   CxDefs.pl
 *   by A.Miguel Dias - 2001/01/02
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

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                                               %
  % The definitions in this file match the internal definitions   %
  % of some CxProlog predefined builtin predicates. This file was %
  % created so that you could analyse those definitions. Deleting %
  % this file causes no harm to CxProlog.                         %
  %                                                               %                                                               %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* UTILITIES */

app([], L, L).
app([H|T], L, [H|R]) :-  app(T, L, R).

retractall(X) :- retract(X), fail.
retractall(X) :- retract((X:-_)), fail.
retractall(_).

findall(X,G,_) :-
	queue_clear('$queue'),
	call(G),
		queue_put('$queue',X),
	fail.
findall(_,_,L) :-
	queue_as_list('$queue', L),
	queue_clear('$queue').

add_pl(user,user) :- !.
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
	write(user,'File '), writeq(File),
	write(user,' consulted '),
	write(user,Heap), write(user,' bytes '),
	write(user,Time), writeln(user,' sec.').

reconsult(File0) :-
	add_pl(File0, File),
	Heap0 is heapused,
	Time0 is cputime,
	silent_reconsult(File),
	Time is cputime - Time0,
	Heap is heapused - Heap0,
	write(user,'File '), writeq(File),
	write(user,' reconsulted '),
	write(user,Heap), write(user,' bytes '),
	write(user,Time), writeln(user,' sec.').

silent_consult(File0) :-
	add_pl(File0, File),
	seeing(SaveIn), see(File),
	'$$_c_loop',
	seen, see(SaveIn).

'$$_c_loop' :-
	repeat,
		read(Term),
	'$$_c_term'(Term), !.

'$$_c_term'(X) :- var(X), !, assertz(X).
'$$_c_term'(end_of_file) :- !.
'$$_c_term'(eof) :- !.
'$$_c_term'(unit U) :- !,
	create_unit(U), U>>'$$_c_loop'.
'$$_c_term'(visible Spec) :- !,
	'$$_visible'(Spec), fail.
'$$_c_term'(import Spec from U) :- !,
	'$$_import'(Spec,U), fail.
'$$_c_term'((:-X)) :- !, (:-X), !, fail.
'$$_c_term'((?-X)) :- !,
	varnames(V), question(X,V), !, fail.
'$$_c_term'(X) :- assertz(X), fail.

silent_reconsult(File0) :-
	add_pl(File0, File),
	seeing(SaveIn), see(File),
	'$$_r_loop',
	seen, see(SaveIn).

'$$_r_loop' :-
	dict_clear('$dict'),
	repeat,
		read(Term),
	'$$_r_term'(Term), !.

'$$_r_term'(X) :- var(X), !, assertz(X).
'$$_r_term'(end_of_file) :- !.
'$$_r_term'(eof) :- !.
'$$_r_term'(unit U) :- !,
	dict_clear('$dict'),
	create_unit(U),
	U>>'$$_r_loop'.
'$$_r_term'(visible Spec) :- !,
	'$$_retract'(Spec),
	'$$_visible'(Spec),
	fail.
'$$_r_term'(import Spec from U) :- !,
	'$$_retract'(Spec),
	'$$_import'(Spec,U),
	fail.
'$$_r_term'((:-X)) :- !, (:-X), !, fail.
'$$_r_term'((?-X)) :- !,
	varnames(V), question(X,V), !, fail.
'$$_r_term'((H:-B)) :- !,
	functor(H,N,A),
	'$$_cond_retract'(N,A),
	assertz((H:-B)),
	fail.
'$$_r_term'(H) :-
	functor(H,N,A),
	'$$_cond_retract'(N,A),
	assertz(H),
	fail.

'$$_visible'([]) :- !.
'$$_visible'([H|T]) :- !,
	'$$_visible'(H), '$$_visible'(T).
'$$_visible'(N/A) :- !,
	visible(N,A).
'$$_visible'(_) :-
	writeln(user_error,'Invalid specification for visible/1'),
	restart.

'$$_import'([],U) :- !.
'$$_import'([H|T],U) :- !,
	'$$_import'(H,U), '$$_import'(T,U).
'$$_import'(N/A,U) :- !,
	import(N,A,U).
'$$_import'(_,_) :-
	writeln(user_error,'Invalid specification for import/2'),
	restart.

'$$_retract'([]) :- !.
'$$_retract'([H|T]) :- !,
	'$$_retract'(H), '$$_retract'(T).
'$$_retract'(N/A) :- !,
	'$$_cond_retract'(N,A).
'$$_retract'(_) :-
	writeln(user_error,'Invalid specification for visible/1 or import/2'),
	restart.

'$$_cond_retract'(N,A) :-
	dict_get('$dict',p(N,A),true), !.
'$$_cond_retract'(N,A) :-
	dict_set('$dict',p(N,A),true),
	abolish(N,A).

/* LISTING */

all :-
	current_unit(U),
		U>>listing,
		fail.
all.

list :-	 listing.

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
'$$_portray_body'((X,Xs)) :-	!,
	tab(8), writeq(X), writeln(','),
	'$$_portray_body'(Xs).
'$$_portray_body'(X) :-
	tab(8), writeq(X).
