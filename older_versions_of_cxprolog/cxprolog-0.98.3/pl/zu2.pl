test :- c(hello)>>b>>a>>p, writeln(test_done).          % test callb, gopen, gclose.
test2 :- c(hello)>>b>>a>>h, writeln(test2_done).		% test unit parameter.
test3 :- c(hello)>>b>>a>>(p,h), writeln(test3_done).	% test system predicate ,/2.

/* ---------- */
unit a.
gopen p/0, s/1.

p :- m(s(_)).
r :- s(X).
s(1) :- writeln(1), xfail.

/* ---------- */
unit b.
gclose m/1, s/1.

m(X) :- writeln(callb), callb(X), writeln(callb_done).
s(11) :- writeln(11), xfail.
s(12) :- writeln(12), xfail.

/* ---------- */
unit c(P).
gopen s/1, e/0.
gclose h/0.

r :- e.
h :- writeln(P).

s(21) :- writeln(21), xfail.
s(22) :- writeln(22).
