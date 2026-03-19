/* writes groups of 10 a's and b's until a stack overflow occurs */

start:-
	new_thread(a,12,a,aa),
	new_thread(b,12,b,bb),
	sinc,
	transfer_to_thread(a,_),
	writeln(end).

a:- inc, wth, a.		%generates a stack overflow producing 'abort' */
a:- transfer_to_thread(b, X), writeln(X), a.
aa :- writeln(aborted), wth, statistics, transfer_to_thread(b,_).

b:- inc, wth, b.		%generates a stack overflow producing 'abort' */
b:- transfer_to_thread(a, _), b.
bb :- writeln(aborted), wth, statistics, transfer_to_thread(a,_).


wth :- actual_thread(X), write(X).

sinc :- retractall(n(_)), assert(n(0)).
inc :- retract(n(N)), inc(N).

inc(10) :- !, assert(n(0)), fail.
inc(N) :- M is N+1, assert(n(M)).
