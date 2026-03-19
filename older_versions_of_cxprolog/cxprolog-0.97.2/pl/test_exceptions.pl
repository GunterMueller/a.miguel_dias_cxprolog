#!/usr/local/bin/cxprolog --script

:- writeln('Testing exceptions\n').

/* Unit tests for exceptions */

e:- call_cleanup(throw(t), writeln(done)).
d:- call_cleanup(true, e).
c:- call_cleanup(true, d).
b:- call_cleanup(true, c).
a:- catch(b,X,writeln(X)), catch(b,X,writeln(X)).

:- catch(b,X,writeln(X)), catch(b,X,writeln(X))
::: writeln('done\nt\ndone\nt').

:- writeln(done), exit_script.
