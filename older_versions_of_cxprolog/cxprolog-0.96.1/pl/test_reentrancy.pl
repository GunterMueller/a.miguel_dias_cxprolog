#!/usr/local/bin/cxprolog --script

run :-
	test_c_reentrancy,
	(current_prolog_flag(java_available,true) -> test_java_reentrancy).

test_c_reentrancy :-
	Levels = 4,
	build_term(Levels, '$$_call_prolog_through_c', writeln('It Works!!!'), Res),
	call(Res).

test_java_reentrancy :-
	Levels = 4,
	build_term(Levels, '$$_call_prolog_through_java', writeln('It Works!!!'), Res),
	call(Res).

build_term(0, _, Res, Res) :- !.
build_term(N, F, G, Res) :-
	M is N - 1,
	T =.. [F,G],
	build_term(M, F, T, Res).

:- run, halt.
