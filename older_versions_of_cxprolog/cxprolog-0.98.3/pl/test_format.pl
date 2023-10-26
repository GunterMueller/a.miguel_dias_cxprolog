#!/usr/local/bin/cxprolog --script

:- write('Testing format... ').

/* Unit tests for format */

portray({X}) :- write('<'), write(X), write('>').

ppp :- write(ole), throw(a).

:- format('~2a - ~@ - ~a - ~p', [ola, ppp, oli, f({aaa})])
::: write('olaola - ole - oli - f(<aaa>)').

:- writeln('done'), exit_script.


