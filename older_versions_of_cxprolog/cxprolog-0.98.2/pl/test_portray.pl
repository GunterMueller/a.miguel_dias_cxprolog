#!/usr/local/bin/cxprolog --script

:- write('Testing portray... ').

/* Unit tests for portray */

portray({X}) :- write('<'), write(X), write('>').
portray([X]) :- write('<'), write(X), write('>'), throw(a).

 :- print(f({aaa},[aaa]))
::: write('f(<aaa>,<aaa>[aaa])').

 :- print(q([a({1}),q(b([1]),f({aaa},b(2))),a(2)]))
::: write('q([a(<1>),q(b(<1>[1]),f(<aaa>,b(2))),a(2)])').

:- writeln('done'), exit_script.
