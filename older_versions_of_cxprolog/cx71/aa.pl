a(0) :- !.
a(X) :- put(65), Y is X -1, a(Y).

b :- tell(aaaaa), a(10000), told.
