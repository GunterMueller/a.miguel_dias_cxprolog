
a(_).

b(X) :- X = 5, mshow(trail).
b(_).

trail :- q &:= X, q &:= 99, a(X), b(X).
trail .


:- trail.


