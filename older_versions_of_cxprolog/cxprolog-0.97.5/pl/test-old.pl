

id(_).

xbig_term(0,[]) :- !.
xbig_term(N,x(99,R)) :- M is N-1, xbig_term(M,R).

t :- xbig_term(5,X), gc, id(X).

