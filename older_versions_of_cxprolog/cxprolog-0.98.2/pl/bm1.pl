run :- bm.

bm :-
	T0 is cputime,
    nrev_data(Count, List),
    (	
        repeat(Count),
        nrev_dummy(List),
        fail
	;	true
	),
    T1 is cputime,
	(
		repeat(Count),
		rev(List),	
		fail
	;	true
	),
    T2 is cputime,
    report(Count, T0, T1, T2).

rev(L) :- !, revln(L, _). 

revln([], []).
revln([H|T], R) :-
    revln(T, L),
    appln(L, [H], R).

appln([], L, L).
appln([H|T], L, [H|R]) :-
    appln(T, L, R).

nrev_data(10000, [ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
     16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]).

nrev_dummy(_).

report(Count, T0, T1, T2) :-
    Time1 is T1-T0,
    Time2 is T2-T1,
    Time  is Time2-Time1,
    Lips is 496*Count/Time,
    write(Lips), write(' lips for '), write(Count),
    write(' iterations taking '), write(Time),
    write(' '), write('secs'), write(' ('),
    write(Time2-Time1), write(')'),
    nl.


repeat(1) :- !.
repeat(N).
repeat(N) :- M is N - 1, repeat(M).
