a(0) :- !.
a(X) :- put(65), Y is X -1, a(Y).

b :- tell(aaaaa), a(10000), told.


c(X,Y) :- concat([X,Y],S), statistics, c(S,S).
c :- c('1', 'a').

% Test super-indexes
q(1,2,3,4).
q(2,3,4,5).
q(X,a,b,c).
q(3,4,5,6).
q(4,5,6,7).
q(5,6,7,8).
q(X,a,b,c).
q(6,7,8,9).


w(X,Y).
w(X,1).
w(X,2).
