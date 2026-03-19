/*
 *   This file is part of the CxProlog system

 *   cxbm.pl
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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

/*
cxprolog on a 700Mz PIII-M notebook

Number of iterations: 10000
local ----- 2.14719e+06 lips, 0% slower
import ---- 1.96825e+06 lips, 8.33333% slower
varimp ---- 1.90769e+06 lips, 11.1538% slower
ctxext ---- 1.91506e+06 lips, 10.8108% slower
vctxext --- 1.91506e+06 lips, 10.8108% slower
ctxdep ---- 2.04959e+06 lips, 4.54545% slower
ctxdep2 --- 1.92996e+06 lips, 10.1167% slower

AFTER CONTEXT OPTIMIZATION:
Number of iterations: 10000
local ----- 2.13793e+06 lips, 0% slower
import ---- 2.02449e+06 lips, 5.30612% slower
varimp ---- 1.96047e+06 lips, 8.3004% slower
ctxext ---- 1.984e+06 lips, 7.2% slower
vctxext --- 1.9761e+06 lips, 7.56972% slower
ctxdep ---- 2.05809e+06 lips, 3.73444% slower
ctxdep2 --- 1.95276e+06 lips, 8.66142% slower

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run:- bm(bmapp)>>bm.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unit bmapp.

visible append_imp/3.
visible append_impv/3.
visible append_dep/3.

append_imp([], L, L).
append_imp([H|T], L, [H|R]) :- append_imp(T, L, R).

append_impv([], L, L).
append_impv([H|T], L, [H|R]) :- append_impv(T, L, R).

append_dep([], L, L).
append_dep([H|T], L, [H|R]) :- append_dep(T, L, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unit bm(XXX).

import append_imp/3 from bmapp.
import append_impv/3 from XXX.

visible bm/0.

bm :-
    nrev_data(Count, List),
    write('Number of iterations: '), write(Count), nl,
    bm('local ----- ', local, Base),
    bm('import ---- ', imp, Base),
    bm('varimp ---- ', impv, Base),
    bm('ctxext ---- ', ext, Base),
    bm('vctxext --- ', extv, Base),
    bm('ctxdep ---- ', dep, Base),
    bm('ctxdep2 --- ', dep2, Base).

bm(Name, Bm, Base) :-
    write(Name),
    do_bm(Bm, Lips),
    (var(Base) -> Base = Lips ; true),
    Perc is 100*(Base - Lips)/Base,
    write(Lips), write(' lips, '),
    write(Perc), write('% slower'), nl.

do_bm(T, Lips) :-
    T0 is cputime,
    nrev_data(Count, List),
    (   
        repeat(Count),
        nrev_dummy(List),
        fail
    ;   true
    ),
    T1 is cputime,
    (
        repeat(Count),
        rev(T,List),    
        fail
    ;   true
    ),
    T2 is cputime,
    lips(Count, T0, T1, T2, Lips).

rev(local,L) :- !, naive_reverse(L, _). 
    naive_reverse([], []).
    naive_reverse([H|T], R) :- naive_reverse(T, L), append2(L, [H], R).
    append2([], L, L).
    append2([H|T], L, [H|R]) :- append2(T, L, R).
        
rev(imp,L) :- !, naive_reverse_imp(L, _). 
    naive_reverse_imp([], []).
    naive_reverse_imp([H|T], R) :-
            naive_reverse_imp(T, L), append_imp(L, [H], R).

rev(impv,L) :- !, naive_reverse_impv(L, _). 
    naive_reverse_impv([], []).
    naive_reverse_impv([H|T], R) :-
            naive_reverse_impv(T, L), append_impv(L, [H], R).

rev(ext,L) :- !, naive_reverse_ext(L, _). 
    naive_reverse_ext([], []).
    naive_reverse_ext([H|T], R) :-
            naive_reverse_ext(T, L), bmapp>>append_imp(L, [H], R).

rev(extv,L) :- !, naive_reverse_extv(L, _). 
    naive_reverse_extv([], []).
    naive_reverse_extv([H|T], R) :-
            naive_reverse_extv(T, L), XXX>>append_imp(L, [H], R).

visible naive_reverse_dep/2.
rev(dep,L) :- !, bmapp>>bm(q)>>naive_reverse_dep(L, _). 
    naive_reverse_dep([], []).
    naive_reverse_dep([H|T], R) :-  
            naive_reverse_dep(T, L), append_dep(L, [H], R).

rev(dep2,L) :- !, bmapp>>
                bm(q)>>bm(q)>>bm(q)>>bm(q)>>bm(q)>>
                bm(q)>>bm(q)>>bm(q)>>bm(q)>>bm(q)>>
                                naive_reverse_dep(L, _). 

nrev_dummy(_).

lips(Count, T0, T1, T2, Lips) :-
    Time  is (T2-T1)-(T1-T0),
    Lips is 496*Count/Time.

nrev_data(10000, [ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
                16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]).

