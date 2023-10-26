
/* ---------- */
unit a.
xover p/1, q/1.
visible p/1, q/1.
xover s/1.
%visible s/1.
xover l/1.

p(X) :- m(s(X)).
q(X) :- i(l(X)).
l(X) :- s(X).
s(a1).

/* ---------- */
unit b.
xover m/1, i/1, s/1.
visible m/1, i/1, s/1.

m(X) :- callb(X).
i(X) :- X.
s(b1).
s(b2).

/* ---------- */
unit c(P).
xover s/1, e/0, h/1.
visible s/1, e/0, h/1.
xover r/0.

h(P).

s(c1).
s(c2).


unit main.
test(X) :- c(hello)>>b>>a>>p(X).  				        % test callb
t(X) :- trace, test(X).

test2(X) :- c(hello)>>b>>a>>h(X).						% test unit parameter.
t2(X) :- trace, test2(X).

test3(X,Y) :- c(hello)>>b>>a>>(p(X),h(Y)).				% test system predicate ,/2.
t3(X,Y) :- trace, test3(X,Y).

test4(X) :- c(hello)>>b>>a>>q(X).  				        % test implicit callb
t4(X) :- trace, test4(X).



/*

EXPECTED RESULT


[main] ?- test(X).
X = a1 ? ;
X = b1 ? ;
X = b2 ? ;
X = c1 ? ;
X = c2 ? ;
no


*/
