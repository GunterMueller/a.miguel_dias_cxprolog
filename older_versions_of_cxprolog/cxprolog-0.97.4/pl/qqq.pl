#!/usr/local/bin/cxprolog --script

:- writeln('Testing exceptions').

/* Unit tests for exceptions */





ex6_5 :- call_cleanup(throw(t), writeln(done)).
ex6_4 :- call_cleanup(writeln(4), ex6_5).
ex6_3 :- call_cleanup(ex6_4, writeln(3)).
ex6_2 :- call_cleanup(writeln(2), ex6_3).
ex6_1 :- call_cleanup(ex6_2, writeln(1)).
ex6 :- catch(ex6_1,X,writeln(X)).

:- ex6.


:- writeln('done\n'), exit_script.
