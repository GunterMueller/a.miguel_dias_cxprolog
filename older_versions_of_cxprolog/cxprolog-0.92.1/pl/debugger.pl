% testing debugger

a0:- a2(X,Y), Y=a, a3.
a2(X,Y):-a1(X).
a1(1).
a1(2).

% cut

b0:- b1, !, b2.
b1:-writeln(e).

c0:-c1,!,fail.
c0.
c1.

% contexts

za:-aa>>a.
zv:-bb>>aa>>f.
x:-aa>>r.

unit aa.
visible a/0.
import b/1 from bb.


a:-b(X), writeln(X),c.
f:-call(v(X)), writeln(X).


unit bb.
visible b/1, v/1.

b(1).
b(2).
b(3).
b(4).
v(10) :- call(zaqwe).
v(11).
