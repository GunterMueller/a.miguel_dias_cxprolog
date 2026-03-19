#!/usr/local/bin/cxprolog --script

:- write('Testing ins predicates... ').

/* Unit tests for ins predicates */


test_ins :-
	ins(1, t, X, R),
	validate_global_stack(c(X,R)),
	a=a.			% needed to catch bugs
test_ins_start :-
	ins_start(t, X, R),
	validate_global_stack(c(X,R)),
	a=a.			% needed to catch bugs
test_ins_end :-
	ins_end(t, X, R),
	validate_global_stack(c(X,R)),
	a=a.			% needed to catch bugs

validate_global_stack(T) :-
	T = c(X,t(Y)),	% needed to catch bugs
	X == Y.			% two goals needed

:-	test_ins, test_ins_start, test_ins_end
::: true.

:- writeln('done'), exit_script.
