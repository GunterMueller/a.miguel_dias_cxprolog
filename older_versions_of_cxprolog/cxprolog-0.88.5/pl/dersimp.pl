/*
 *   This file is part of the CxProlog system

 *   dersimp.pl
 *   by A.Miguel Dias - 2002/03/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

t :- dn(f(x)*g(x), 3, x).
t0 :- d(f(x)*g(x), x).
b :- statistics(runtime,_),
     (
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A), fail ;
        derivn(f(x)*g(x),3,x,A)
     ),
     statistics(runtime,[_,T]),
     write(T), write(' milisecs'), nl.

% DERSIMP - Calculates derivatives and simplifies expressions
%           Used to test the CxProlog system

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERACTIVE PREDICATES
% 
% d(F,X)      - prints the derivative of F with respect to X
% dn(F,N,X)   - prints the derivative of F of order N with respect to X
% s(E)        - prints the arithmetic expression E simplified
%

d(F,X) :-
        deriv(F,X,D),
        write(D), nl.
        
dn(F,N,X) :-
        derivn(F,N,X,D),
        write(D), nl.
        
s(E) :-
        simplify(E,Es),
        write(Es), nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED PREDICATES
% 
% deriv(F,X,D)    - calculates the derivative of F with respect to X
% derivn(F,N,X,D) - calculates the derivative of F of order N with respect to X
% simplify(E,Ep)  - simplify the arithmetic expression E
%

deriv(F,X,D) :-
        derivative(F,X,R),
        simplify(R,D).
        
derivn(F,N,X,D) :-
        derivative_n(F,N,X,R),
        simplify(R,D).

simplify(E,Ep) :-
        simp(E,Es),
        pretty(Es,Ep).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCTION TABLE
% 
% The functions the program knows.
% New entries are welcomed.
%
%           Function        Derivative

function(   e(X)        ,   e(X)                ).
function(   log(X)      ,   1/X                 ).
function(   sin(X)      ,   cos(X)              ).
function(   cos(X)      ,   -sin(X)             ).
function(   tg(X)       ,   1/cos(X)^2          ).
function(   cotg(X)     ,   -1/sin(X)^2         ).
function(   sec(X)      ,   sin(X)/cos(X)^2     ).
function(   cosec(X)    ,   -cos(X)/sin(X)^2    ).
function(   arcsin(X)   ,   1/(1-X^2)^0.5       ).
function(   arccos(X)   ,   -1/(1-X^2)^0.5      ).
function(   arctg(X)    ,   1/(1+X^2)           ).
function(   arccotg(X)  ,   -1/(1+X^2)          ).
function(   arcsec(X)   ,   1/(X*(1+X^2)^0.5)   ).
function(   arccosec(X) ,   -1/(X*(1+X^2)^0.5)  ).
function(   sh(X)       ,   ch(X)               ).
function(   ch(X)       ,   sh(X)               ).
function(   arcsh(X)    ,   1/(1+X^2)^0.5       ).
function(   arcch(X)    ,   1/(X^2-1)^0.5       ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SYMBOLIC DIFFERENTIATOR
% 
% The derivative of any order of a function is computed. The program
% handles conventional arithmetic expressions and knows how to derivative
% the functions listed behind. Other (unknown) functions are also
% handled by the program. In this case the quote notation is used as in
% the example:
%                   derivative(x+f(x) ,x , 1+f'(x)*1).
%
% The calculated derivative is not given in a simplified form.
% The rule of the derivative of the composite function is used explicitly.
%


% derivative - Derivative of order 1

derivative(X,X,1) :- !.
derivative(C,X,0) :- atomic(C), !.
derivative(F+G,X,Fd+Gd) :- !,
        derivative(F,X,Fd),
        derivative(G,X,Gd).
derivative(F*G,X,F*Gd+G*Fd) :- !,
        derivative(F,X,Fd),
        derivative(G,X,Gd).
derivative(F^G,X,F^G*(log(F)*Gd+Fd*F^(-1)*G)) :- !,
        derivative(F,X,Fd),
        derivative(G,X,Gd).
derivative(+F,X,D) :- !,
        derivative(F,X,D).
derivative(-F,X,D) :- !,
        derivative(-1*F,X,D).
derivative(F-G,X,D) :- !,
        derivative(F+(-1)*G,X,D).
derivative(F/G,X,D) :- !,
        derivative(F*G^(-1),X,D).
derivative(FG,X,FdG*Gd) :- function(FG,FdG), !,   % composite function rule.
        FG =.. [F,G],
        derivative(G,X,Gd).
derivative(FG,X,FdG*Gd) :-
        unknown_function(FG,FdG),
        FG =.. [F,G],
        derivative(G,X,Gd).


% derivative_n - Derivative of order N

derivative_n(F,0,X,F) :- !.
derivative_n(F,1,X,Fdn) :- !,
        derivative(F,X,Fdn).
derivative_n(F,N,X,Fdn) :-
        M is N-1,
        derivative(F,X,Fd),
        simp(Fd,Fs),
        pretty(Fs,Fp),
        derivative_n(Fp,M,X,Fdn).


% unknown_function - Put a quote in from of the function name,
%                               e.g.  f(2+x) --> f'(2+x) .

unknown_function(FX,FdX) :-
        FX =.. [F,X],
        name(F,Fn),
        concat(Fn,[39],Fnd),
        name(Fd,Fnd),
        FdX =.. [Fd,X].


% concat - Concatenate lists.

concat([],L,L).
concat([H|T],L,[H|TL]) :- concat(T,L,TL).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPRESSION SIMPLIFIER
% 
% The expression is completely rebuild bottom-up. All the constants
% are folded and sub-expressions are eliminated whenever possible.
% A suitable data structure is used to represent the expression so
% that manipulation can be done efficiently:
%
% The sum An*A + Bn*B + ... + N where An, Bn, .. ,N are numeric constants
% is represented by: +[A:An, B:Bn, ..., '$n':N]. Zero is represented by: +[].
%
% The product A^An * B^Bn * ... * N where A,An,B,Bn, are arbitrary expressions
% and N is a numeric constant is represented by: *[A:An, B:Bn, ...]:N.
%
% For example the expression 2*x + 5*y^(4+r)*z + 7 have the following
% representation: +[*[z:(+['$n':1]), y:(+[r:1,'$n':4])]:5, x:2, '$n':7].
%
% This expressions are keep always sorted and in the more compact
% possible form. This form (I will call canonical form) enables a non
% ambiguous representation of expressions. An expression is in its
% simplest form if it is in the canonical form.
%
% The program is deterministic.
%

:- op(9, fx, *).
:- op(10, xfy, :).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simp - Simplifier top level.

simp(0,+[]) :- !.
simp(N,+['$n':N]) :- number(N), !.
simp(A,+[A:1]) :- atom(A), !.
simp(A+B,ABs) :- !,
        simp(A,As),
        simp(B,Bs),
        add(As,Bs,ABs).
simp(A*B,ABs) :- !,
        simp(A,As),
        simp(B,Bs),
        mult(As,Bs,ABs).
simp(A^B,ABs) :- !,
        simp(A,As),
        simp(B,Bs),
        power(As,Bs,ABs).
simp(+A,R) :- !,
        simp(A,R).
simp(-A,R) :- !,
        simp((-1)*A,R).
simp(A-B,R) :- !,
        simp(A+(-1)*B,R).
simp(A/B,R) :- !,
        simp(A*B^(-1),R).
simp(AB,+[ABs:1]) :-
        AB =.. [A,B],
        simp(B,Bs),
        ABs =.. [A,Bs].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% canon - Some conversions to the canonic form that are not implicit
%                           in the program and are done explicitly.

canon(*[]:An,+['$n':An]) :-  !.
canon(*[A:(+['$n':1])]:An,+[A:An]) :-  !.
canon(+[*A:An],*A:An) :-  !.
canon(X,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add - Add two expressions in canonical form.

add(+A,+B,R) :- !, do_add(A,B,AB), canon(+AB,R).
add(+A,B,R) :- !, add(+A,+[B],R).
add(A,+B,R) :- !, add(+[A],+B,R).
add(A,B,R) :- add(+[A],+[B],R).

% Add loop.
do_add([],B,B) :- !.
do_add(A,[],A) :- !.
do_add([A:An|At],[A:Bn|Bt],ABt) :- 0 is An+Bn, !,
        do_add(At,Bt,ABt).
do_add([A:An|At],[A:Bn|Bt],[A:ABn|ABt]) :- /*A == B,*/ !,
        ABn is An+Bn, do_add(At,Bt,ABt).
do_add([A:An|At],[B:Bn|Bt],[A:An|ABt]) :- A @> B, !,
        do_add(At,[B:Bn|Bt],ABt).
do_add([A:An|At],[B:Bn|Bt],[B:Bn|ABt]) :- /*A @< B,*/ !,
        do_add([A:An|At],Bt,ABt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mult - Multiply two expressions given in canonical form.

% Deal with one factor products
mult(+[],_,+[]) :- !.
mult(_,+[],+[]) :- !.
mult(+['$n':An],+[B:Bn],+[B:ABn]) :- !,
        ABn is An*Bn.
mult(+[A:An],+['$n':Bn],+[A:ABn]) :- !,
        ABn is An*Bn.
mult(+[A:An],+[B:Bn],R) :- !,
        mult(*[A:(+['$n':1])]:An,*[B:(+['$n':1])]:Bn,R).

% Deal with multiple factor products
mult(*A:An,*B:Bn,R) :- !,
        do_mult(A,B,AB,N),
        ABn is An*Bn*N,
        canon(*AB:ABn,R).
mult(+['$n':An],*B:Bn,*B:ABn) :- !,
        ABn is An*Bn.
mult(*A:An,+['$n':Bn],*A:ABn) :- !,
        ABn is An*Bn.
mult(+[A:An],*B:Bn,R) :- !,
        mult(*[A:(+['$n':1])]:An,*B:Bn,R).
mult(*A:An,+[B:Bn],R) :- !,
        mult(*A:An,*[B:(+['$n':1])]:Bn,R).

% Distributive propriety:     (a+b)*c = a*c + b*c
mult(+[A],B,R) :- !,
        mult(B,+[A],R).
mult(+[Ah|At],B,AB) :- !,
        canon(+[Ah],C),
        mult(B,C,R),
        mult(+At,B,S),
        add(R,S,AB).
mult(A,+[Bh|Bt],AB) :-
        canon(+[Bh],C),
        mult(A,C,R),
        mult(+Bt,A,S),
        add(R,S,AB).

% Multiply loop.
do_mult([],B,B,1) :- !.
do_mult(A,[],A,1) :- !.
do_mult([A:An|At],[A:Bn|Bt],ABt,N) :- add(An,Bn,+[]), !,
        do_mult(At,Bt,ABt,N).
do_mult([A:An|At],[A:Bn|Bt],ABt,AMN) :-
            number(A), add(An,Bn,+['$n':M]), !,
        do_mult(At,Bt,ABt,N),
        AMN is A^M*N.
do_mult([A:An|At],[A:Bn|Bt],[A:ABn|ABt],N) :- /*A == B,*/ !,
        add(An,Bn,ABn),
        do_mult(At,Bt,ABt,N).
do_mult([A:An|At],[B:Bn|Bt],[A:An|ABt],N) :- A @> B, !,
        do_mult(At,[B:Bn|Bt],ABt,N).
do_mult([A:An|At],[B:Bn|Bt],[B:Bn|ABt],N) :- /*A @< B, */
        do_mult([A:An|At],Bt,ABt,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% power - Raise a expression to another, both in canonical form 

% Some simple particular cases
power(_,+[],+['$n':1]) :- !.
power(A,+['$n':1],A) :- !.
power(+['$n':An],+['$n':Bn],+['$n':ABn]) :- !,
        ABn is An^Bn.
power(+['$n':An],B,*[An:B]:1) :- !.
power(+[A:An],+['$n':Bn],*[A:(+['$n':Bn])]:ABn) :- !,
        ABn is An^Bn.
power(+[A:1],B,*[A:B]:1) :- !.
power(+[A:An],B,*[A:B,An:B]:1) :- !.

% Exponent is a natural number
power(A,+['$n':I],R) :- integer(I), I>0, !,
        J is I-1,
        power(A,+['$n':J],P),
        mult(P,A,R).

% Exponent is a number. Distributive propriety.
power(*A:An,+['$n':Bn],*R:ABn) :- !,
        ABn is An^Bn,
        distrib_power(A,+['$n':Bn],R).

% Distributive propriety:  (a*b)^c = a^b * b^c
power(*A:1,B,*R:1) :- !,
        distrib_power(A,B,R).
power(*A:An,B,R) :- !,
        distrib_power(A,B,D),
        mult(*D:1,*[An:B]:1,R).

% Catch all
power(A,B,*[A:B]:1).


% distrib_power - Distributive propriety loop
distrib_power([],_,[]).
distrib_power([A:An|At],C,[A:ACn|ACt]) :-
        mult(An,C,ACn),
        distrib_power(At,C,ACt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPRESSION BEAUTIFIER 
% 
% Convert an expression in the canonical form to the usual representation.
% Some care are take to improve the expression presentation.
%

% pretty - Beautifier top level
pretty(+A,Ac) :- !,
        split_add(A,P,N),
        pretty_add(P,Pc),
        pretty_add(N,Nc),
        pretty_glue_add(Pc,Nc,Ac).
pretty(*A:1,Ac) :- !,
        pretty(*A,Ac).
pretty(*A:(-1),-Ac) :- !,
        pretty(*A,Ac).
pretty(*A:An,Ac) :- !,
        concat(A,[An:(+['$n':1])],AN),
        pretty(*AN,Ac).
pretty(*A,Ac) :- !,
        split_mult(A,P,N),
        pretty_mult(P,Pc),
        pretty_mult(N,Nc),
        pretty_glue_mult(Pc,Nc,Ac).
pretty(A^B,Ac^Bc) :- !,
        pretty(A,Ac),
        pretty(B,Bc).
pretty(AB,ABc) :- AB =.. [A,B], !,
        pretty(B,Bc),
        ABc =.. [A,Bc].
pretty(X,X).


% pretty_glue_add - Build a '-' node.
pretty_glue_add(A,0,A) :- !.
pretty_glue_add(0,B,-B) :- !.
pretty_glue_add(A,B,A-B).

% pretty_add - Handling an add list in the canonical representation
pretty_add([],0) :- !.
pretty_add(['$n':N],N) :- !.
pretty_add([Ah:1],Ahc) :- !,
        pretty(Ah,Ahc).
pretty_add([A:An],An*Ac) :- !,
        pretty(A,Ac).
pretty_add([Ah|At],Atc+Ahc) :-
        pretty_add([Ah],Ahc),
        pretty_add(At,Atc).

% split_add - Finds the positive and negative components of a sum
split_add([],[],[]).
split_add([Ah:Ahn|At],P,[Ah:V|N]) :- integer(Ahn), Ahn<0, !,
        V is -Ahn,
        split_add(At,P,N).
split_add([Ah:Ahn|At],[Ah:Ahn|P],N) :-
        split_add(At,P,N).


% pretty_glue_mult - Build a '/' node.
pretty_glue_mult(A,1,A) :- !.
pretty_glue_mult(A,B,A/B).

% split_mult - Finds the numerator and denominator of a division
split_mult([],[],[]).
split_mult([Ah:(+['$n':M])|At],P,[Ah:(+['$n':V])|N]) :- M<0, !,
        V is -M,
        split_mult(At,P,N).
split_mult([Ah:(+[E:(-1)])|At],P,[Ah:(+[E:1])|N]) :- !,
        split_mult(At,P,N).
split_mult([Ah:Ahn|At],[Ah:Ahn|P],N) :-
        split_mult(At,P,N).

% pretty_mult - Handling an mult list in the canonical representation
pretty_mult([],1) :- !.
pretty_mult([A:(+['$n':1])],Ac) :- !,
        pretty(A,Ac).
pretty_mult([A:An],Ac^Anc) :- !,
        pretty(A,Ac),
        pretty(An,Anc).
pretty_mult([Ah|At],Atc*Ahc) :-
        pretty_mult([Ah],Ahc),
        pretty_mult(At,Atc).
