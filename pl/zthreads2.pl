/* writes groups of 10 a's and b's until a stack overflow occurs */

start:-
	thread_new(a,main>>a,main>>aa),
	thread_new(b,main>>b,main>>bb),
	sync,
	thread_transfer(a,_),
	writeln(end).

visible [a/0, aa/0].
a:- inc, write('a  '), thread_transfer(b,X), writeln(a-X), a.
a:- writeln('a END'), thread_transfer(b,X), writeln(a-X).
aa :- writeln('a aborted'), statistics, thread_transfer(b,_).


visible [b/0, bb/0].
b:- inc, write('b  '), thread_transfer(a,X), writeln(b-X), b.
b:- writeln('b END'), thread_transfer(a,X), writeln(b-X).
bb :- writeln('b aborted'), statistics, thread_transfer(a,_).


sync :- retractall(n(_)), assert(n(0)).
inc :- retract(n(N)), inc(N), write(N), write('   ').

inc(100) :- !, fail.
inc(N) :- M is N+1, assert(n(M)).
