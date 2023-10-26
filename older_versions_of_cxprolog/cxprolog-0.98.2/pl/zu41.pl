unit a.
xover p/1.
visible p/1.
p(a1) :- z(a1).
p(a2) :- z(a2).

z(X).



unit b.
xover p/1.
visible p/1.
/* no clauses for p/1 */




unit b_1.
visible p/1.
p(b_1).

unit b_2.
xover p/1.
visible p/1.
p(b_2).

unit b_21.
visible p/1.
p(b_21).

unit b_22.
xover p/1.
p(b_22).

unit b_3.
xover p/1.
p(b_3).





unit c.
visible g/1.
%visible p/1.
%xover p/1.
%p(c1).
g(X) :- p(X), w(X).

w(X).


unit d.
%visible p/1.
xover p/1.

p(d1).
p(d2).







unit e.






unit f.
visible p/1.





unit g.
visible p/1.
/* no clauses for p/1 */





unit h.
xover p/1.
visible p/1.

p(h1).
p(h2).






unit i.
p(i1).
p(i2).






unit main.
test(X) :- a>>b>>b_1>>b_2>>b_21>>b_22>>b_3>>c>>d>>e>>f>>g>>h>>i>>g(X).
t(X) :- trace, test(X).



/*

EXPECTED RESULT


[main] ?- test(X).
X = d1 ? ;
X = d2 ? ;
X = a1 ? ;
X = a2 ? ;
no


*/
