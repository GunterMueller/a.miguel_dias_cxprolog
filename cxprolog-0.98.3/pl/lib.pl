/* CxProlog library */
/* This code inspired in several sources... */

/* append(?L1,?L2,?L3) - L3 is the concatenation of L1 and L2 */
append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

/* length(?L,?N) - N is the length of list L */
length([],0).
length([H|T],N) :- length(T,M), succ(M,N).

/* member(?T,?L) - The term T is a member of list L */
member(X,[X|T]) :- (T == [] -> ! ; true).
member(X,[_|T]) :- member(X,T).

/* reverse(+L1,-L2) - L2 is the reverse of the list L1 */
reverse(L1,L2) :- '$$_rev'(L1,[],L2).
'$$_rev'([],A,A).
'$$_rev'([H|L],A,R) :- '$$_rev'(L,[H|A],R).

/* select(?X,?L,?R) - Select term X from list L leaving the rest R */
select(X,[X|T],T).
select(X,[H|T1],[H|T2]) :- select(X,T1,T2).

/* permutation(?L1,?L2) - L1 and L2 are mutual permutations */
permutation(L1,L2) :- '$$_perm'(L1,L2,L2).
'$$_perm'([],[],[]).
'$$_perm'([H1|T1],L2,[_|T3]) :- '$$_perm'(T1,T2,T3), select(H1,L2,T2).
