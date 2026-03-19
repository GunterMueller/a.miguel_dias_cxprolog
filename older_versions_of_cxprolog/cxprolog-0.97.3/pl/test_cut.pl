#!/usr/local/bin/cxprolog --script

:- writeln('Testing cut').

/* Unit tests for cut */

a :- writeln(a1).
a :- writeln(a2).

b :- writeln(b1).
b :- writeln(b2).

c :- writeln(c1).
c :- writeln(c2).

d :- writeln(d1).
d :- writeln(d2).

x :- writeln(x1).
x :- writeln(x2).

z1(X) :- X; x.
z2(X) :- call(X); x.


 :- x, a, !, b, fail
::: writeln('x1\na1\nb1\nb2'), fail.

 :- x, call((a, !, b)), fail
::: writeln('x1\na1\nb1\nb2\nx2\na1\nb1\nb2'), fail.

 :- C = (a, !, b), x, call(C), fail
::: writeln('x1\na1\nb1\nb2\nx2\na1\nb1\nb2'), fail.

 :- x, call( (,), a, (!, b)), fail
::: writeln('x1\na1\nb1\nb2\nx2\na1\nb1\nb2'), fail.


 :- ((a, !, b); x), fail
::: writeln('a1\nb1\nb2'), fail.

  :- (call((a, !, b)); x), fail
::: writeln('a1\nb1\nb2\nx1\nx2'), fail.

 :- C = (a, !, b), (call(C); x), fail
::: writeln('a1\nb1\nb2\nx1\nx2'), fail.


 :- x, ((a,!) -> b ; c), fail
::: writeln('x1\na1\nb1\nb2\nx2\na1\nb1\nb2'), fail.

 :- x, (a -> (!,b) ; c), fail
::: writeln('x1\na1\nb1\nb2'), fail.


 :- z1(call((a, !, b))), fail
::: writeln('a1\nb1\nb2\nx1\nx2'), fail.

 :- z2((a, !, b)), fail
::: writeln('a1\nb1\nb2\nx1\nx2'), fail.


 :- x, call_cleanup((a, !, b), writeln(cleanup)), fail
::: writeln('x1\na1\nb1\nb2\ncleanup\nx2\na1\nb1\nb2\ncleanup'), fail.


:- writeln('done\n'), exit_script.
