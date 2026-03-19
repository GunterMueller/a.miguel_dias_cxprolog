

id(_).

xbig_term(0,[]) :- !.
%xbig_term(N,x(99,R)) :- M is N-1, xbig_term(M,R).

%xbig_term(N,[99|R]) :- M is N-1, xbig_term(M,R).
xbig_term(N,[N|R]) :- M is N-1, xbig_term(M,R).

heap :- xbig_term(5,X), gc, id(X).


b(X,Z) :- X = 5, Z = f(9), mshow(trail), gc, mshow(trail).
b(_,_).

trail :- q &:= 99, id(X), b(X,f(Y)).
trail .


a_list(0,[]).
a_list(N,[a|L]) :- M is N-1, a_list(M,L).

var_list(0,[]).
var_list(N,[X|L]) :- M is N-1, var_list(M,L).

check_trail(N) :-
    a_list(N,L),
    var_list(N,R),
    L=R.

trail2 :- check_trail(3), mshow(trail), gc, mshow(trail).




make_cyclic(X,a(a(Y)),Z) :-
    X = a(a(X)),
    Y = a(a(a(Y))),
    Z = a(a(b(Z))).
make_cyclic(_,_,_).

cyclic :-
    make_cyclic(A,B,C),
	mshow(local), mshow(global), gc, mshow(global), mshow(local).

/*
zpl, [test].
gcr(1), make_cyclic(A,B,C), gcr(0).

*/


ppp(a(5),b(4)).
ppp(_,_).

p :-
    ppp(A,B),
	mshow(local), mshow(global), gc, mshow(global).




a(L) :- a([1|L]).
a :- gcr(10000), a([]), gcr(0).




% This file contains example Prolog programs. 
% The programs in this file are all tested. You can load 
% the file and run the test cases.

% append(L1,L2,L3): append L1 and L2 to get L3.

append([],L,L).
append([A|L],L1,[A|L2]) :- append(L,L1,L2).




% member(A,L): A is in list L

member(A,[A|L]).
member(A,[B|L]) :- A \== B, member(A,L).


%*******************************************************************
% you can use any constant symbol to represent 0 and 
% any function symbol to represent the successor function

xplus(0, X, X).
xplus(s(X), Y, s(Z)) :- xplus(X,Y,Z).

mult(0, X, 0).
mult(s(0), X, X).
mult(s(X), Y, N) :- mult(X, Y, N1), xplus(Y, N1, N).

fact(0, s(0)).
fact(s(0), s(0)).
fact(s(X), N) :- fact(X, N1), mult(N1, s(X), N).

%*******************************************************************
% In pracitical programming, you may want to use 
% builtin predicates for arithmetics.

fact1(0, 1).
fact1(1, 1).
fact1(X, N) :- Z is X - 1, fact1(Z, N1), 
               N is N1 * X.

% test cases

t1(W) :- xplus(s(s(0)), s(s(s(0))), W).
t2(X,Y) :- xplus(X, Y, s(s(s(0)))).
t3(W) :- mult(s(s(0)), s(s(s(0))), W).
t4(W) :- fact(s(s(s(s(0)))), W).
t5(W) :- fact1(4, W).

%*******************************************************************
% In Prolog, we can only define predicates (relations). If you
% want to define a function, you have to define a relation by 
% using an extra parameter to hold the "returned" result.
% The proper use of the paremeters in invocations will give 
% you the desired function. 

% Recall we earlier wrote the Lisp function "cartesian". 
% We now define a relation, i.e., cartesian(X,Y,Z) where 
% X and Y are lists and  Z is the list of all the paris corresponding
% to cartesian product of X and Y, e.g.,
% cartesian([a, b], [d, e], [[a, d], [a, e], [b, d], [b, e]]) 
% is true.


cartesian([], L, []).
cartesian([A|N], L, M) :- 
             pair(A,L,M1), 
             cartesian(N, L, M2),
             append(M1, M2, M).
pair(A, [], []).
pair(A, [B|L], [[A,B]|N] ) :- pair(A, L, N).


% tests

t6(W) :- cartesian([a, b], [d, e], W).
t7 :- cartesian([a, b], [d, e], [[a, d], [a, e], [b, d], [b, e]]). 


%*******************************************************************
% Define a relation reverse(X,Y) such that Y is the reversed list of X

reverse([], []).
reverse([A|L1], L2) :- reverse(L1, N), append(N, [A], L2).


%*******************************************************************

% Given a (possibly nested) list of numbers L, 
% sum(L,N) will have N bound to the sum of the numbers in L.

sum([],0).
sum(N,N) :- number(N).
sum([A|L],S) :- sum(A,S1), sum(L,S2), S is S1 + S2.


% test case:
t(S) :- sum([2,3,[4,5,[6],7],9], S).

%*******************************************************************
% flatten(L,L1): flatten a list of atoms (atoms and numbers) L to 
% a flat list L1. 

flatten([],[]).
flatten([A|L],[A|L1]) :- 
     xatom(A), flatten(L,L1).
flatten([A|L],R) :- 
     flatten(A,A1), flatten(L,L1), append(A1,L1,R).

xatom(A) :- atom(A).
xatom(A) :- number(A).

% tests

f(I) :- flatten([2,[a]], I).
f1(I) :- flatten([2,[a],[b,5,[c],d],9], I).


%*******************************************************************
% You can express patterns of nested lists, e.g.
%
%  1. []         an empty list
%  2. [[]|L]     a list whose car part is an empty list
%  3. [[A|W]|L]  a list whose car part is a non-empty list
%  4. [A|L]      if none of the above applies, A is an atom or number.


flat([],[]).
flat([[]|L],L1) :- flat(L,L1).
flat([[A|L]|W],R) :- flat([A|L],U), flat(W,W1), append(U,W1,R).
flat([A|L], [A|L1]) :- flat(L,L1).

% The last clause above works only for getting the first answer.
% Backtracking could invoke it when A is not an atom. Better to replace 
% it by 
%
% flat([A|L], [A|L1]) :- xatom(A), flat(L,L1).


% tests
f2(I) :- flat([2,[a]], I).
f3(I) :- flat([2,[a],[b,5,[c],d],9], I).




%*******************************************************************
%                 The Eight-Queens Problem
%
% A board of eight queens with each of them placed in a raw  
% such that no two of them are in the same collumn, or in the same
% diagonal (if they are, they will attack each other)

% We use a list to represent the board positions and how queens are 
% placed on the board. The ith queen (in the ith raw) in the jth 
% collumn will be represented  by placing integer i as the jth element
% in the list. For example, [1,4,2,5,3,8,6,7] represents the locations
% of the queens as:
%    the first queen is at the board position (1.1); 
%    the second queen is at the board position (2.4); 
%    the third queen is at the board position (3.2), etc. 


solution(Board) :-
    permutation([1,2,3,4,5,6,7,8], Board), safe(Board).

permutation([], []).
permutation([A|M], N) :- 
          permutation(M, N1),
          insert(A, N1, N).

insert(A, L, [A|L]).
insert(A, [B|L], [B|L1]) :- insert(A, L, L1).

safe([Q]).
safe([Q|List]) :- nodiag(Q, List, 1), safe(List).

nodiag(Q, [], Dist).
nodiag(Q1, [Q2|List], Dist) :- 
       noattack(Q1, Q2, Dist),
       NewDist is Dist + 1,
       nodiag(Q1, List, NewDist).

noattack(Q1, Q2, Dist) :- 
       Q2 - Q1 =\= Dist, Q1 - Q2 =\= Dist.

% could use the builtin function abs(X) -- absolute value of X:
%     abs(Q2-Q1) =\= Dist



% To generalize the solution to the N-queens problem, we can define  
% queens(+N,-S) where N is a positive integer and S should be bound to
% a solution of the the N-queens problem.

queens(N,S) :- gen_list(N,L), permutation(L, S), safe(S).
gen_list(1, [1]).
gen_list(N, L) :- N1 is N -1, gen_list(N1, L1), append(L1,[N],L).

/*
zpl, [test].
gcr(10), queens(8,S), gcr(0).
*/




%*******************************************************************
% I use the following to save typing a long string for loading 
% this file -- once it's loaded first time, I will type l. to load 
% it again after changes are made. 

l :- ['sample-prog.pl'].


% To measure the CPU time of running your query, one can use 
 
stats :- 
   statistics(runtime,[_,X]), 
   T is X/1000,
   nl,
   write('Run time: '),
   write(T), write(' sec.'), nl.


% Such as

qs :- queens(8,S), stats. 

% where the CPU time for generating the first solution in proving 
% queens(8,S) is printed.



