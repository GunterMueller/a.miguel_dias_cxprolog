% Check BIG term

xbig_term(0,[]) :- !.
xbig_term(N,[N|R]) :- M is N-1, xbig_term(M,R).

big_term(N) :-
	write_depth(0,0),
	writeln(begin),
	xbig_term(N,R),
	writeln(created),
	tell('big_term.pl'),
	writeln(R),
	told.

big_term :- big_term(65000).


% Check BIG floats.

xbig_floats(X):-
	writeln(X) ,
	Y is X*X,
	xbig_floats(Y).

big_floats :-
	flag(float_display_precision,_,40),
	xbig_floats(1.00000001001).


% Check TRAIL expansion

a_list(0,[]).
a_list(N,[a|L]) :- M is N-1, a_list(M,L).

var_list(0,[]).
var_list(N,[X|L]) :- M is N-1, var_list(M,L).

check_trail(N) :-
	statistics,
	a_list(N,L),
	var_list(N,R),
	statistics,
	L=R,
	statistics.

check_trail :- check_trail(10000).

unif([],[]).
unif([X|L],[X|R]) :- unif(L,R).


% Generate LONG sequence of clauses
gen_clausesx(0) :- !.
gen_clausesx(N) :-
	M is N-1,
	write('dc(a'), write(M), writeln(').'),
	gen_clausesx(M).

gen_clauses(F,N) :-
	tell(F),
	gen_clausesx(N),
	told.


% Check BIG is/2

big_is(0,1) :- !.
big_is(N,1+R) :- M is N-1, big_is(M,R).

big_is(N) :-
	A is cputime,
	big_is(N,T),
	X is T,
	Z is cputime - A,
	writeln(result = X),
	writeln(time = Z).

big_is :- big_is(10000).




/*
zz([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,
 A0,B0,C0,D0,E0,F0,G0,H0,I0,J0,K0,L0,M0,N0,O0,P0,Q0,R0,S0,T0,
 A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,
 A2,B2,C2,D2,E2]).
*/
