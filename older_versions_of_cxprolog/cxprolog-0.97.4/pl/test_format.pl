#!/usr/local/bin/cxprolog --script

:- writeln('Testing format').

/* Unit tests for format */

portray({X}) :- write('<'), write(X), write('>').

ppp :- write(ole), throw(a).

:- format('~2a - ~@ - ~a - ~p', [ola, ppp, oli, f({aaa})])
::: write('olaola - ole - oli - f(<aaa>)').

:- writeln('done\n'), exit_script.


