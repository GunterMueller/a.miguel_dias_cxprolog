l(0,[]) :- !.
l(N,[a|L]) :- M is N-1, l(M,L).

t(N) :- l(N,A), l(N,B), A=B.

