#!/usr/local/bin/cxprolog --script

:- write('Testing exceptions... ').

/* Unit tests for exceptions */

ex1_1 :- throw(a).
ex1 :- catch(ex1_1, a, write(a)), writeln(a).

:- ex1
::: writeln(aa).




ex2_3 :- throw(b).
ex2_2 :- catch(ex2_3, c, write(c)), writeln(c).
ex2_1 :- catch(ex2_2, b, write(b)), writeln(b).
ex2 :- catch(ex2_1, a, write(a)), writeln(a).

:- ex2
::: writeln('bb\na').




ex3_6 :- throw(b).
ex3_5 :- catch(ex3_6, f, write(f)), writeln(f).
ex3_4 :- '$$_call_prolog_through_c'(ex3_5), writeln(e).
ex3_3 :- catch(ex3_4, d, write(d)), writeln(d).
ex3_2 :- '$$_call_prolog_through_c'(ex3_3), writeln(c).
ex3_1 :- catch(ex3_2, b, write(b)), writeln(b).
ex3 :- '$$_call_prolog_through_c'(ex3_1), writeln(a).

:- ex3
::: writeln('bb\na').



:- catch((writeln(a); writeln(b); throw(c); writeln(d)), X, writeln(X)), fail
::: writeln('a\nb\nc'), fail.



:- '$$_call_prolog_through_c'('$$_call_prolog_through_c'(true))
::: true.


:- '$$_call_prolog_through_c'('$$_call_prolog_through_c'(fail))
::: fail.


:- '$$_call_prolog_through_c'('$$_call_prolog_through_c'(throw(a)))
::: throw(a).


:- call_cleanup(throw(t), writeln(done))
::: writeln('done'), throw(t).

:- catch(call_cleanup(throw(t), writeln(done)), X, writeln(X))
::: writeln('done\nt').


ex4_5 :- call_cleanup(throw(t), writeln(done)).
ex4_4 :- call_cleanup(true, ex4_5).
ex4_3 :- call_cleanup(true, ex4_4).
ex4_2 :- call_cleanup(true, ex4_3).
ex4_1 :- call_cleanup(true, ex4_2).
ex4 :- catch(ex4_1,X,writeln(X)), catch(ex4_1,X,writeln(X)).

:- ex4
::: writeln('done\nt\ndone\nt').


ex5_5 :- call_cleanup(throw(t), writeln(done)).
ex5_4 :- call_cleanup(ex5_5, writeln(4)).
ex5_3 :- call_cleanup(ex5_4, writeln(3)).
ex5_2 :- call_cleanup(ex5_3, writeln(2)).
ex5_1 :- call_cleanup(ex5_2, writeln(1)).
ex5 :- catch(ex5_1,X,writeln(X)), catch(ex5_1,X,writeln(X)).

:- ex5
::: writeln('done\n4\n3\n2\n1\nt\ndone\n4\n3\n2\n1\nt').


ex6_5 :- call_cleanup(throw(t), writeln(done)).
ex6_4 :- call_cleanup(writeln(4), ex6_5).
ex6_3 :- call_cleanup(ex6_4, writeln(3)).
ex6_2 :- call_cleanup(writeln(2), ex6_3).
ex6_1 :- call_cleanup(ex6_2, writeln(1)).
ex6 :- catch(ex6_1,X,writeln(X)), catch(ex6_1,X,writeln(X)).

:- ex6
::: writeln('2\n4\ndone\n3\n1\nt\n2\n4\ndone\n3\n1\nt').


:- writeln('done'), exit_script.
