/* A CxProlog library of usefull predicates - This is a preliminary version */
/* This code was written with some inspiration from
 * "The Art of Prolog" book and other sources...
 * */

/* list_append(?L1,?L2,?L3) - List L3 is the concatenation of lists L1 and L2 */
list_append([],L,L).
list_append([X|Xs],L,[X|Ys]) :- list_append(Xs,L,Ys).

/* list_length(?L,?N) - N is the length of list L */
list_length([],0).
list_length([X|Xs],N) :- list_length(Xs,M), succ(M,N).

/* list_member(?T,?L) - Term T is a member of list L */
list_member(T,[T|Xs]) :- (Xs == [] -> ! ; true).
list_member(T,[_|Xs]) :- list_member(T,Xs).

/* list_nextto(?T1,?T2,?L) - Term T2 follows term T1 in list L */
list_nextto(T1,T2,[T1,T2|_]).
list_nextto(T1,T2,[_|Xs]) :- list_nextto(T1,T2,Xs).

/* list_delete(?L1,?T,?L2) - Del all elems of L1 that unify with T giving L2 */
list_delete([],_,[]) :- !.
list_delete([T|Xs],T,L2) :- !, list_delete(Xs,T,L2).
list_delete([X|Xs],T,[X|Ys]) :- list_delete(Xs,T,Ys).

/* list_reverse(+L1,-L2) - L2 is the reverse of the list L1 */
list_reverse(L1,L2) :- '$$_rev'(L1,[],L2).
'$$_rev'([],L,L).
'$$_rev'([X|Xs],L,R) :- '$$_rev'(Xs,[X|L],R).

/* list_nth0(?N,?L,?T) - The Nth element of list L is T */
list_nth0(N,L,T) :- integer(N), !, N >= 0, '$$_nth_i'(L,T,N).
list_nth0(N,L,T) :- var(N), !, '$$_nth_v'(L,T,0,N).
'$$_nth_i'([T|Xs],T,0) :- !.
'$$_nth_i'([_|Xs],T,N) :- succ(M,N), '$$_nth_i'(Xs,T,M).
'$$_nth_v'([T|_],T,R,R).
'$$_nth_v'([_|Xs],T,N,R) :- succ(N,M), '$$_nth_v'(Xs,T,M,R).

/* list_nth1(?N,?L,?T) - The Nth element of list L is T */
list_nth1(N,L,T) :- integer(N), !, succ(M,N), '$$_nth_i'(L,T,M).
list_nth1(N,L,T) :- var(N), !, '$$_nth_v'(L,T,1,N).

/* list_last(?L,?T) - T is the last element of list L */
list_last([X],X).
list_last([_|Xs],R) :- list_last(Xs,R).

/* list_sum(+L,-N) - N is the sum of all numbers in the list L */
list_sum(L,N) :- '$$_sum'(L,0,N).                                                                                
'$$_sum'([],R,R).
'$$_sum'([X|Xs],N,R) :- M is N+X, '$$_sum'(Xs,M,R).

/* list_num(+N1,+N2,-L) - The list N consists in the int sequence N1 .. N2 */
list_num(N1,N2,L) :- integer(N1), integer(N2), N1=<N2, '$$_num'(N1,N2,L).                                   
'$$_num'(N,N,[N]) :- !.
'$$_num'(N,M,[N|Xs]) :- succ(N,N1), '$$_num'(N1,M,Xs).

/* list_select(?T,?L,?R) - Select term T from list L leaving the rest R */
list_select(T,[T|Xs],Xs).
list_select(T,[X|Xs],[X|Ys]) :- list_select(T,Xs,Ys).

/* list_permutation(?L1,?L2) - L1 and L2 are mutual permutations */
list_permutation(L1,L2) :- '$$_perm'(L1,L2,L2).
'$$_perm'([],[],[]).
'$$_perm'([X|Xs],L,[_|Ys]) :- '$$_perm'(Xs,R,Ys), list_select(X,L,R).

/* list_map(+G,?L1,?L2) - Apply G to all elements of L1 giving L2 */
list_map(G,L1,L2) :- '$$_map'(L1,L2,G).
'$$_map'([],[],G).
'$$_map'([X|Xs],[Y|Ys],G) :- call(G,X,Y), '$$_map'(Xs,Ys,G).

/* list_forall(+G,+L1) - Apply G to all elements of L1  */
list_forall(G,L1) :- '$$_forall'(L1,G).
'$$_forall'([],G).
'$$_forall'([X|Xs],G) :- call(G,X), '$$_forall'(Xs,G).

/* int_between(+N1, +N2, ?N3) -  N1 =< N3 =< N2 */
int_between(N1, N2, N3) :-
	integer(N1), integer(N2), N1 =< N2,
	(  integer(N3)
	-> N1 =< N3, N3 =< N2
	;  (N3 = N1 ; N is N1 + 1, int_between(N, N2, N3))
	).
	
/* Compatibility predicates for some Prologs */
append(L1,L2,L3) :- list_append(L1,L2,L3) .
length(L,N) :- list_length(L,N).
member(X,L) :- list_member(X,L).
nextto(T1,T2,L) :- list_nextto(T1,T2,L).
delete(L1,T,L2) :- list_delete(L1,T,L2).
reverse(L1,L2) :- list_reverse(L1,L2).
nth0(N,L,T) :- list_nth0(N,L,T).
nth1(N,L,T) :- list_nth1(N,L,T).
last(L,T):- list_last(L,T). 
sumlist(L,N):- list_sum(L,N).
numlist(N1,N2,L) :- list_num(N1,N2,L).
select(X,L1,L2) :- list_select(X,L1,L2).
permutation(L1,L2) :- list_permutation(L1,L2).

/* gensym(+Prefix, -Unique) - Unique atom with the given Prefix  */
/* Already in the kernel with a better implementation.
gensym(Prefix, Unique) :-
    '$gensym_next'(Prefix, Num),
    atom_codes(Prefix, Name1),
    number_codes(Num, Name2),
    append(Name1, Name2, Name),
    atom_codes(Unique, Name).

'$gensym_next'(Prefix, Num) :-
    retract('$gensym'(Prefix, Num1)), !,
    Num is Num1 + 1,
    asserta('$gensym'(Prefix, Num)).
'$gensym_next'(Prefix, 1) :-
    asserta('$gensym'(Prefix, 1)).

gensym(Unique) :- gensym('%', Unique).
*/
