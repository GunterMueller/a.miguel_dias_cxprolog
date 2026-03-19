

bb :- open(bb_text, read, S, [encoding(utf8)]),
     repeat, get0(S,X), stream_property(S,position(P)), writeln([X,P]), X == -1, !.


a(2).
a(3).
a.
a.
a:- false.
a.

b(4).
b.

c :- fail.
