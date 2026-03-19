[main] ?- read(X).
|: a-> -b.
[main] ?- read(X).
|: 1 + (a-> -b).
X = 1+(a-> -b) 
[main] ?- read(X).
|: d(a-> -b).
% SYNTAX ERROR (read/1): expected ')'.
d(a#HERE#-> -b).


d( (a + b) + (a + b) ).
