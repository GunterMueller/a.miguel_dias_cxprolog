#!/usr/local/bin/cxprolog --script

:- write('Testing $PAR... ').

 :- X='$PAR'(b), hide_par([X,X],Y), writeln(Y), hide_par(Z,Y), writeln(Z)
::: writeln('[b,b]'), writeln('[$PAR(b),$PAR(b)]').

 :- X='$PAR'('$PAR'(b)), hide_par([X,X],Y), writeln(Y), hide_par(Z,Y), writeln(Z)
::: writeln('[b,b]'), writeln('[$PAR($PAR(b)),$PAR($PAR(b))]').

 :- X='$PAR'('$PAR'('$PAR'(b))), hide_par([X,X],Y), writeln(Y), hide_par(Z,Y), writeln(Z)
::: writeln('[b,b]'), writeln('[$PAR($PAR($PAR(b))),$PAR($PAR($PAR(b)))]').

 :- X='$PAR'(b), hide_par(X,Y), writeln(Y), hide_par(Z,Y), writeln(Z)
::: writeln('b'), writeln('b').

 :- Z = f(Z,'$PAR'(1)), hide_par(Z,Y), arg(2,Y,X), writeln(X)
::: writeln(1).

:- writeln('done'), exit_script.
