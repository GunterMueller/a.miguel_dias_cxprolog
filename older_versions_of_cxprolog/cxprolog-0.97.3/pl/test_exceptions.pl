#!/usr/local/bin/cxprolog --script

:- writeln('Testing exceptions').

/* Unit tests for exceptions */

ex5:- call_cleanup(throw(t), writeln(done)).
ex4:- call_cleanup(true, ex5).
ex3:- call_cleanup(true, ex4).
ex2:- call_cleanup(true, ex3).
ex1:- catch(ex2,X,writeln(X)), catch(ex2,X,writeln(X)).

:- catch(ex2,X,writeln(X)), catch(ex2,X,writeln(X))
::: writeln('done\nt\ndone\nt').

:- writeln('done\n'), exit_script.
