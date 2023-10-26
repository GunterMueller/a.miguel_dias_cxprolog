/*
 *   This file is part of the NanoProlog system

 *   NanoBoot.pl
 *   by A.Miguel Dias - 89/11/25
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 940225: "'$reconsult_process'(eof) :- !." added.
 940225: call of '$user_mode' in $lux0.
 931117: release of version 0.5

*/

'$called_from_c'(_,X) :- X, '$event'(2).
'$called_from_c'(_,_) :- '$event'(3).
'$called_from_c'(_,_).

% Normal startup
'$lux0' :- /* ['consult more system files here'], */ '$user_mode', '$TopLevel'.

% Restart after error or abort
'$lux1' :- '$TopLevel'.

% TOP LEVEL

'$TopLevel' :- repeat,
				'$notrace',
				write('?- '),
				read(G,V),
				try(G,V),
			   fail.
try(G,[]) :- '$trace', G, '$notrace', !, writeln(yes).
try(G,[H|T]) :- '$trace', G, '$notrace', showVars([H|T]), more, !.
try(_,_) :- '$notrace', writeln(no).
more :- getnb(X), more(X).
more(10) :- !, writeln(yes).
more(59) :- !, skip(10), fail.
more(_) :- skip(10), writeln(yes).
getnb(X) :- repeat, get0(X), X \= 32, !.
showVars([]).
showVars([S]) :- !, write(S).
showVars([H|T]) :- writeln(H), showVars(T).


% PATCHED PREDICATES

repeat.
repeat.

clause(H,B) :- '$clause'(H,B,[]).
'$clause'(_,_,_) :- fail.
'$clause'(H,B,_) :- '$clause'(H,B).

retract(X) :- '$retract'(X,[]).
'$retract'(_,_) :- fail.
'$retract'(X,_) :- '$retract'(X).

user_predicate(X) :- '$user_predicate'(X,[]).
'$user_predicate'(_,_) :- fail.
'$user_predicate'(X,_) :- '$user_predicate'(X).

system_predicate(X) :- '$system_predicate'(X,[]).
'$system_predicate'(_,_) :- fail.
'$system_predicate'(X,_) :- '$system_predicate'(X).



% SOME DEFINITIONS

writeln(X) :- write(X), nl.
code(F/A) :- '$code'(F,A).
code(F) :- '$code'(F).
noindex(F/A) :- noindex(F,A).

halt :- '$event'(4).
restart :- '$event'(1).	% automatically generated after any non-fatal error

call(X) :- X.

not X :- X, !, fail.
not _.

If -> Then :- If, !, Then.
If -> Then ; Else :- If, !, Then.
If -> Then ; Else :- !, Else.

A;_ :- A.
_;B :- B.

A,B :- A, B.

A \= B :- A = B, !, fail.
_ \= _.

A \== B :- A == B, !, fail.
_ \== _.

(:- X) :- X.
(?- X) :- X.

end_of_file :- halt.
eof :- halt.

T =.. [F|As] :- '$=..'(As,T,F,0).
'$=..'([],T,F,N) :- functor(T,F,N), !.
'$=..'([A|As],T,F,N) :- N1 is N+1, '$=..'(As,T,F,N1), !, arg(N1,T,A).

[].
[File|Files] :- consult([File|Files]).

(A :- B) :-
    assertz((A :- B)),
    write((A :- B)), write(' asserted.'), nl,
    restart.


% CONSULT

consult([]) :- !.
consult(-File) :- !, reconsult(File).
consult([File|Files]) :- !, consult(File), consult(Files).
consult(File) :-
		Heap0 is heapused,
		Time0 is cputime,
		seeing(SaveI), see(File),
		repeat, read(Term), '$consult_process'(Term), !,
		seen, see(SaveI),
		Time is cputime - Time0,
		Heap is heapused - Heap0,
		telling(SaveO), tell(user),
		write('File '), write(File), write(' consulted '),
		write(Heap), write(' bytes '),
		write(Time), writeln(' sec.').

'$consult_process'(end_of_file) :- !.
'$consult_process'(eof) :- !.
'$consult_process'((?-X)) :- !, X, !, fail.
'$consult_process'((:-X)) :- !, X, !, fail.
'$consult_process'(X) :- assertz(X), fail.



% RECONSULT

reconsult([]) :- !.
reconsult([File|Files]) :- !, consult(File), consult(Files).
reconsult(File) :-
		noindex('$retract_done'/1),
		Heap0 is heapused,
		Time0 is cputime,
		seeing(SaveIn), see(File),
		repeat, read(Term), '$reconsult_process'(Term), !,
		seen, see(SaveIn),
		retractall('$retract_done'(_)),
		Time is cputime - Time0,
		Heap is heapused - Heap0,
		telling(SaveOut), tell(user),
		write('File '), write(File), write(' reconsulted '),
		write(Heap), write(' bytes '),
		write(Time), writeln(' sec.'),
		told, tell(SaveOut).

'$reconsult_process'(end_of_file) :- !.
'$reconsult_process'(eof) :- !.
'$reconsult_process'((?-X)) :- !, X, !, fail.
'$reconsult_process'((:-X)) :- !, X, !, fail.
'$reconsult_process'(X) :-
		'$reconsult_head'(X,H),
		'$reconsult_retract'(H),
		assertz(X),
		fail.

'$reconsult_head'((A:-_),A) :- !.
'$reconsult_head'(A,A).

'$reconsult_retract'(H) :- '$retract_done'(H), !.
'$reconsult_retract'(H) :-
		functor(H,Func,Num),
		functor(Proc,Func,Num),
		asserta('$retract_done'(Proc)),
		retractall(Proc).



% RETRACTALL

retractall(X) :- retract(X), fail.
retractall(X) :- retract((X:-_)), fail.
retractall(_).



% FINDALL

findall(X,G,_) :-
		asserta('$findall_found'('$findall_mark')),
		call(G),
		asserta('$findall_found'(X)),
		fail.
findall(_,_,L) :- '$findall_collect'([],M), !, L=M.

'$findall_collect'(S,L) :-
		'$findall_getnext'(X), !,
		'$findall_collect'([X|S],L).
'$findall_collect'(L,L).

'$findall_getnext'(X) :- retract('$findall_found'(X)), !, X \== '$findall_mark'.


% LISTING

listing :-
		user_predicate(H),
		nl,
		clause(H,B),
		'$listing_clause'(H,B),
		fail.
listing.

listing(F/A) :-
		functor(H,F,A),
		clause(H,B),
		'$listing_clause'(H,B),
		fail.
listing(F) :-
		user_predicate(H),
		functor(H,F,_),
		clause(H,B),
		'$listing_clause'(H,B),
		fail.
listing(_).
		
'$listing_clause'(H,true) :- !, write(H), writeln('.').
'$listing_clause'(H,B) :-
		write(H), writeln(' :-'), '$listing_body'(B), writeln('.').

'$listing_body'(X) :- var(X), !, tab(8), write(X).
'$listing_body'((X,Xs)) :- !,
		tab(8), write(X), writeln(','), '$listing_body'(Xs).
'$listing_body'(X) :- tab(8), write(X).
