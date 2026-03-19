/* writes groups of 10 a's and b's until a stack overflow occurs */

start:-
	thread_new(a,main>>a,main>>aa),
	thread_new(b,main>>b,main>>bb),
	sync,
	thread_transfer(a,_),
	writeln(end).

visible [a/0, aa/0].
a:- inc, wth, a.		%generates a stack overflow producing 'abort' */
a:- thread_transfer(b,X), writeln(X), a.
aa :- writeln(aborted), wth, statistics, thread_transfer(b,_).

visible [b/0, bb/0].
b:- inc, wth, b.		%generates a stack overflow producing 'abort' */
b:- thread_transfer(a,_), b.
bb :- writeln(aborted), wth, statistics, thread_transfer(a,_).


wth :- active_thread(X), writeln(X).

sync :- retractall(n(_)), assert(n(0)).
inc :- retract(n(N)), inc(N).

inc(10) :- !, assert(n(0)), fail.
inc(N) :- M is N+1, assert(n(M)).
