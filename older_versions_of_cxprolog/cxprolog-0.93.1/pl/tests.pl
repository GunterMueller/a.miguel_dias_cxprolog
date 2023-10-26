% Check BIG term

xbig_term(0,[]) :- !.
xbig_term(N,[N|R]) :- M is N-1, xbig_term(M,R).

big_term(N) :-
	write_depth(0,0),
	writeln(begin),
	xbig_term(N,R),
	writeln(created),
	tell('big_term.pl'),
copy_term(R,Z0),
copy_term(Z0,Z1),
copy_term(Z1,Z2),
copy_term(Z2,Z3),
copy_term(Z3,Z4),
	writeln(Z4),
	told.

big_term :- big_term(6500).


% Check BIG floats.

xbig_floats(X):-
	writeln(X) ,
	Y is X*X,
	xbig_floats(Y).

big_floats :-
	set_prolog_flag(float_display_precision,40),
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


% Check STACKS expansion

c_stacks([a|X]) :- c_stacks(X).

check_stacks :- c_stacks(X).


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


% Assertz LONG sequence of clauses

assert_clausesx(0) :- !.
assert_clausesx(N) :-
	M is N-1,
	assertz(a(M)),
	assert_clausesx(M).

assert_clauses(N) :-
	abolish(a,1),
    Time0 is cputime,
	assert_clausesx(N),
    Time is cputime - Time0,
    write(user,Time), writeln(user,' sec.').


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


% Check stream over buffer

c :-
    buffer_new(b),
    open_buffer_stream(b,write,s,'pt_PT.utf8'),
    put(s,50),
    put(s,51),
    put(s,52),
    put(s,233),
    put(s,50),
    put(s,50),
    close(s),
    open_buffer_stream(b,read,s,'pt_PT.utf8'),
    get0(s,X0), writeln(X0),
    get0(s,X1), writeln(X1),
    get0(s,X2), writeln(X2),
    get0(s,X3), writeln(X3),
    get0(s,X4), writeln(X4),
    get0(s,X5), writeln(X5),
    get0(s,X6), writeln(X6).



% Check PROCESSES is/2

visible child_restart/0.
child_restart :-
	writeln(child),
	process_send_father(4001),
	os_sleep(10),
	writeln(child),
	process_send_father(4002),
	os_sleep(10),
	writeln(child),
	process_send_father(4003),
	os_sleep(10),
	halt.

c :- process_new(_, restart, main>>child_restart).
r :- process_receive_from_child(P,N), writeln([P,N]).
a :- process_receive_from_child_ready.
p :- processes.


% Check buffers and streams

check_buffers_1 :-
	buffer_new(b),
	buffer_size(b,_,2),
	buffer_set(b,1,0'a),
	buffer_set(b,2,0'b),
	open_buffer_stream(b,read,s,binary),
	buffer_new(bs), buffer_write(bs),
	get_block(s,bs), buffer_write(bs).

check_buffers_2 :-
	buffer_new(b),
	buffer_size(b,_,2),
	buffer_set(b,1,0'a),
	buffer_set(b,2,0'b),
	buffer_new(bs), buffer_write(bs),
	open_buffer_stream(bs,write,s,binary),
	put_block(s,b), buffer_write(bs).


% Check insert binary info in the middle of text

check_write_mix :-
	buffer_new(Buff),
	open(rrr,read,F,binary), get_block(F,Buff), close(F), % rrr = oláolé\n
	open(qqq,write,G),
	writeln(G,olá),
	put_block(G,Buff),
	writeln(G,olé),
	close(G).

check_read_mix :-
	buffer_new(Buff),
	open(qqq,read,G),
	get0(G,C0),get0(G,C1),get0(G,C2),get0(G,_), name(A,[C0,C1,C2]), writeln(A),
	get_block(G,Buff,7), buffer_write(Buff),
	get0(G,D0),get0(G,D1),get0(G,D2),get0(G,_), name(B,[D0,D1,D2]), writeln(B),
	close(G).


% Check Buffer expansion due to a large clause which also includes
% a EnsureSpace instruction.

zz :- q( [0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
0] ), e.


/*
Too many variables in clause.

zz([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,
 A0,B0,C0,D0,E0,F0,G0,H0,I0,J0,K0,L0,M0,N0,O0,P0,Q0,R0,S0,T0,
 A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,
 A2,B2,C2,D2,E2]).
*/


% Check dynamic predicates

:- dynamic b/1.
b(1).
b(2).
b(3).

l :- clause(b(X),Y), write((b(X):-Y)), nl, r, fail.
l.

r :- retract(b(3)), write(done), nl, !.
r.

check_logical :- l,l,l.

/* OUTPUT:
b(1):-true
done
b(2):-true
b(3):-true
b(1):-true
b(2):-true
b(1):-true
b(2):-true
*/

:- dynamic s/1.
s(first) :- retract(s(_)), fail.
s(middle) :- fail.
s(last) :- true.

check_logical2(X) :- s(X), \+s(_).
/* OUTPUT: X = last.  */

:- dynamic p/0, q/0.
p :- assertz(p), fail.
p :- fail.
q :- fail.
q :- assertz(q), fail.
                                                                                
check_logical3 :- \+p, \+q, p, q.
/* OUTPUT: yes.  */


:- dynamic a/1.

a(1) :- fail.
a(2) :- retractall(a(_)), fail.
a(3) :- listing(a/1), fail.
a(4) :- code(a), fail.
a(5) :- writeln(5).


:- dynamic_iu b/1.
b(1).
b(2).
b(3).

l :- clause(b(X),Y), write((b(X):-Y)), nl, r, fail.
l.

r :- retract(b(3)), write(done), nl, !.
r.

check_iu :- l,l,l.

/* OUTPUT:
b(1):-true
done
b(2):-true
b(1):-true
b(2):-true
b(1):-true
b(2):-true
*/

:- dynamic_iu s/1.
s(first) :- retract(s(_)), fail.
s(middle) :- fail.
s(last) :- true.

check_iu2(X) :- s(X).
/* OUTPUT: no.  */

:- dynamic_iu p/0, q/0.
p :- assertz(p), fail.
p :- fail.
q :- fail.
q :- assertz(q), fail.
                                                                                
check_iu3 :- p, q.
/* OUTPUT: yes.  */



:- dynamic_iu aaa/1.
aaa(1) :- fail.
aaa(2) :- retractall(aaa(2)), fail.
aaa(3) :- writeln(3), fail.
aaa(4) :- writeln(4), fail.
/* OUTPUT:
3
4
no
*/


:- dynamic_iu aaa/1.
aaa(1) :- fail.
aaa(2) :- retractall(aaa(3)), fail.
aaa(3) :- writeln(3), fail.
aaa(4) :- writeln(4), fail.
/* OUTPUT:
4
no
*/



:- dynamic_iu aaa/1.
aaa(0):-fail.
aaa(1):-clause(aaa(X),Y), write((aaa(X):-Y)), nl, retractall(aaa(_)), fail.
aaa(2):- write(end), nl.
/* OUTPUT:
aaa(0):-fail
no
*/

:- dynamic aaa/1.
aaa(0):-fail.
aaa(1):-clause(aaa(X),Y), write((aaa(X):-Y)), nl, retractall(aaa(_)), fail.
aaa(2):- write(end), nl.
/* OUTPUT:
aaa(0):-fail
aaa(1):-clause(aaa(_G67),_G65),write((aaa(_G67):-_G65)),nl,retractall(aaa(_G68)),fail
aaa(2):-write(end),nl
end
X = 2 ;
no
*/

:- dynamic aaa/1.
aaa(0):-fail.
aaa(1):-clause(aaa(X),Y), write((aaa(X):-Y)), nl, retractall(aaa(2)), fail.
aaa(2):- write(end), nl.
/* OUTPUT:
aaa(0):-fail
aaa(1):-clause(aaa(_G67),_G65),write((aaa(_G67):-_G65)),nl,retractall(aaa(2)),fail
aaa(2):-write(end),nl
end
X = 2 ;
no
*/




:- multifile a/1.
:- dynamic_iu a/1.

a(1) :- fail.
a(2) :- [user], fail.











% Check exceptions

/*
a(X):- writeln(before_a), catch(z(X),x,h(X)), writeln(after_a).
a(qqq) :- true.
z(X):- writeln(before_z), catch(b(X),y,i(X)), writeln(after_z).

b(1) :- writeln(b1).
b(2) :- writeln(thrown), throw(x).
b(3) :- writeln(b3).

h(5) :- writeln(h5).
h(6) :- writeln(h6).

i(5) :- writeln(i5).
i(6) :- writeln(i6).

*/

a:-b.
b:-c.
c:-d.
d:-writeln(d).


/*
Check efects of throw on the C runtime stack.

a :- '$$_level', catch(b,x,c), '$$_level', writeln(end).

b :- print(r), nl.

c :- writeln(ccccc).


portray(r) :- print(s).
portray(s) :- write(sss), '$$_level', throw(x).
*/

% Check with_ivar

show_ivar :- a =: X, writeln(X).

check_with_ivar :-
	a := f(5),
	show_ivar,
	with_ivar(a,t(t(7)), (show_ivar;show_ivar;true)),
	show_ivar,
	fail.
