#!/usr/local/bin/cxprolog --script

:- writeln('Testing reentrancy\n').

/* Unit tests for Prolog/C and Prolog/Java reentrancy */

test_c_reentrancy :-
	Levels = 4,
	build_term(Levels, '$$_call_prolog_through_c', writeln('It Works (C)!!!'), Res),
	call(Res).

test_java_reentrancy :-
	Levels = 4,
	build_term(Levels, '$$_call_prolog_through_java', writeln('It Works (Java)!!!'), Res),
	call(Res).

build_term(0, _, Res, Res) :- !.
build_term(N, F, G, Res) :-
	M is N - 1,
	T =.. [F,G],
	build_term(M, F, T, Res).

 :- test_c_reentrancy
::: writeln('It Works (C)!!!').

 :- test_java_reentrancy
::: writeln('It Works (Java)!!!')
<<: current_prolog_flag(java_available,true).

:- writeln(done), exit_script.
