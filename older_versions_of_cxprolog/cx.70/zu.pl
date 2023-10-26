

unit main.
visible [test/0].

test :-
	a>>b>>c>>d(111)>>ccc.



unit a.
visible [xxx/0, yyy/0, ccc/0].
ivar [u,v,x,y,z].

xxx :- show_context, writeln('xxx: I am the a/0 version').
yyy :- show_context, writeln('yyy: I am the a/0 version').

ccc :- show_context, xxx.

s:- set_ivar(v, [1,2,Z]).
g(X) :- get_ivar(v, X).





unit b.
visible [xxx/0, yyy/0, bbb/0].

xxx :- show_context, writeln('xxx: I am the b/0 version').
yyy :- show_context, writeln('yyy: I am the b/0 version').

bbb :- show_context, xxx, show_context, ccc.





unit c.
visible [xxx/0, yyy/0, ccc/0].
import yyy/0 from d(222).

xxx :- show_context, writeln('xxx: I am the c/0 version'), show_context, down(xxx).

ccc :- show_context, xxx, show_context, bbb, show_context, yyy.





unit d(P).
visible [xxx/0, yyy/0].

xxx :- show_context, writeln('xxx: I am the d/1 version').

yyy :- show_context, unit_param(1,X), writeln(['yyy: I am the d/1 version', X]).

