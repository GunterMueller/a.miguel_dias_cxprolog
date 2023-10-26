ml(0,[]).
ml(N,[a|L]) :- M is N-1, ml(M,L).

mx(0,[]).
mx(N,[X|L]) :- M is N-1, mx(M,L).

a(N) :- ml(N,L), mx(N,R), statistics, unif(L,R).

unif([],[]).
unif([X|L],[X|R]) :- unif(L,R).

:- write_depth(10,10).

z :- ml(9999,X).






x(0,X,Y) :- !, app(X,Y,[1,2,3,4,5]).
x(N,X,Y) :- M is N-1, x(M,X,Y).
