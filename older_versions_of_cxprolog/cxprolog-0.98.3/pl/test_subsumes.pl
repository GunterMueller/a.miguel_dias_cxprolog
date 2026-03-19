#!/usr/local/bin/cxprolog --flags nil_is_special=true --script

:- write('Testing subsumes... ').

/* Unit tests for subsume */

:-	subsumes_term(a, a)
::: true.

:-	subsumes_term(f(X,Y), f(Z,Z))
::: true.

:-	subsumes_term(f(X,Y), f(Z,Z)), X == Y
::: false.

:-	subsumes_term(f(Z,Z), f(X,Y))
::: false.

:-	subsumes_term(g(X), g(f(X)))
::: false.

:-	subsumes_term(X, f(X))
::: false.

:-	subsumes_term(X, Y), subsumes_term(Y, f(X))
::: true.



:-	subsumes(a, a)
::: true.

:-	subsumes(f(X,Y), f(Z,Z)), X == Y
::: true.

:-	subsumes(f(Z,Z), f(X,Y))
::: false.

:-	subsumes(g(X), g(f(X)))
::: false.

:-	subsumes(X, f(X))
::: false.

:-	subsumes(X, Y), subsumes(Y, f(X))
::: false.

:- writeln('done'), exit_script.
