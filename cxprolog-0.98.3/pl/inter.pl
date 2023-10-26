% Meta-interpreter that handles cuts in disjuntion, ifthenelse, not, ...
$i((   !                  ),C) :-    cut(C). 
$i((   call(G)            ),C) :- !, get_level(D), $i(G,D).
$i((   not G              ),C) :-    $i(G,C), !, fail.
$i((   not _              ),C) :- !.
$i((   \+ G               ),C) :-    $i(G,C), !, fail.
$i((   \+ _               ),C) :- !.
$i((   G;_                ),C) :-    $i(G,C).
$i((   _;H                ),C) :- !, $i(H,C).
$i((   G,H                ),C) :- !, $i(G,C), $i(H,C).
$i((   U>>G               ),C) :- !, U>>$i(G,C).
$i((   >G                 ),C) :- !, >$i(G,C).
$i((   <G                 ),C) :- !, <$i(G,C).
$i((   ?G                 ),C) :- !, ?$i(G,C).
$i((   G->H               ),C) :- !, $i(G,C), !, $i(H,C).
$i((   once G             ),C) :- !, $i(G,C), !.
$i((   possible G         ),C) :- !, $i(not not G,C).
$i((   try G              ),C) :- !, $i(G,C), !.
$i((   try _              ),C) :- !.
$i((   G unless X         ),C) :- !, $i(G,C), ($i(X,C), !, fail ; true).
$i((   G until X          ),C) :- !, $i(G,C), ($i(X,C), ! ; true).

$i((   repeat             ),_).
$i((   repeat             ),C) :- !, $i(repeat,C).
$i((   var(A)             ),_) :- !, var(A).
$i((   nonvar(A)          ),_) :- !, nonvar(A).
$i((   atom(A)            ),_) :- !, atom(A).
$i((   integer(A)         ),_) :- !, integer(A).
$i((   float(A)           ),_) :- !, float(A).
$i((   number(A)          ),_) :- !, number(A).
$i((   db_reference(A)    ),_) :- !, db_reference(A).
$i((   atomic(A)          ),_) :- !, atomic(A).
$i((   primitive(A)       ),_) :- !, primitive(A).
$i((   simple(A)          ),_) :- !, simple(A).
$i((   callable(A)        ),_) :- !, callable(A).
$i((   unit_designator(A) ),_) :- !, unit_designator(A).
$i((   ground(A)          ),_) :- !, ground(A).
$i((   arg(A,B,C)         ),_) :- !, arg(A,B,C).
$i((   functor(A,B,C)     ),_) :- !, functor(A,B,C).

$i((    G               ),C) :-    G.
