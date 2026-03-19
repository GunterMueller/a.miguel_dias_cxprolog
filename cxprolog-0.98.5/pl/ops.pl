

% Tests for the term writer - 15/Aug/2025

d(T) :- writeq(T), write(' - '), display(T), nl.

% d(T) :- writeq(T), nl.


:- writeln(0).

% all possible kinds of operators, with precedence's 1, 2, and 3.
% & means 'f'; @ means 'x'; # means 'y'
% the precedences 1, 2 and 3 are denoted by @, # and $.
 
:- op(1, xfx, @&@@), op(1, xfx, '@&@@a'), op(2, xfx, @&@#), op(3, xfx, @&@$). 
:- op(1, xfy, @&#@), op(2, xfy, @&##), op(3, xfy, @&#$). 
:- op(1, yfx, #&@@), op(2, yfx, #&@#), op(3, yfx, #&@$). 
:- op(1, fx, &@@), op(2, fx, &@#), op(3, fx, &@$). 
:- op(1, xf, @&@), op(2, xf, @&#), op(3, xf, @&$). 
:- op(1, fy, &#@), op(2, fx, &##), op(3, fx, &#$). 
:- op(1, yf, #&@), op(2, yf, #&#), op(3, yf, #&$). 



% xfx tests
:- writeln(1).
:- d(a @&@@ b), d(a '@&@@a' b), d(a @&@# b), d(a @&@$ b).
:- d(a @&#@ b), d(a @&## b), d(a @&#$ b).
:- d(a #&@@ b), d(a #&@# b), d(a #&@$ b).
:- d(&@@ b), d(&@# b), d(&@$ b).
:- d(a @&@), d(a @&#), d(a @&$).
:- d(&#@ b), d(&## b), d(&#$ b).
:- d(a #&@), d(a #&#), d(a #&$).



% xfx tests
:- writeln(2).

:- d( (a @&@@ b) @&@@ (a @&@@ b) ), d( (a @&@@ b) '@&@@a' (a @&@@ b) ), d( (a @&@# b) @&@@ (a @&@# b) ), d( (a @&@$ b) @&@@ (a @&@$ b) ).
:- d( (a @&@@ b) @&@# (a @&@@ b) ), d( (a @&@# b) @&@# (a @&@# b) ), d( (a @&@$ b) @&@# (a @&@$ b) ).
:- d( (a @&@@ b) @&@$ (a @&@@ b) ), d( (a @&@# b) @&@$ (a @&@# b) ), d( (a @&@$ b) @&@$ (a @&@$ b) ).



% xfy tests
:- writeln(3).

:- d( a @&#@ b ).
:- d( a '@&@@a' b ).
:- d( (a @&@@ b) @&## (a @&@@ b) ).
:- d( (a @&@# b) @&## (a @&@@ b) ).
:- d( (a @&@$ b) @&## (a @&@@ b) ).



:- writeln(4).

:- d( &@$ (a,b)@&##z), d( &@$(a,b)@&##z).



:- writeln(5).

:- op(200, yfx, @@@@@), op(199, xfx, @@@@).
:- d((a @@@@@ b) @@@@@ b).



:- writeln(6).

:- d( &@@(-1) ).
:- d( &@@(-a) ).
:- d( &@@(&@@(-a)) ).
:- d( &@#(&@@(-a)) ).
:- d((a-> -b)).
:- d((a-> - b)).
:- d((a-> (-b))).
:- d((a-> -@&@@ )).



:- writeln(7).

:- d(a @&@@ b).
:- d(@ @&@@ b).
:- d((&#$) @&@@ b).
:- d((#&$) @&@@ b).



:- writeln(8).

:- d((a -> b)).
:- d((a-> - @&@@)).



:- writeln(9).

:- op(5, fy, aaa).
:- d((aaa aaa a)).
:- d((aaa aaa (>=))).



:- writeln(10).

:- op(5, yf, bbb).
:- op(5, yf, r).
:- op(5, fy, s).
:- d((r bbb bbb)).
:- d(((>=) bbb bbb)).



:- writeln(11).

:- d(( a @&@@ c )).
:- d(( #### @&@@ #### )).
:- d(( #### '@&@@a' #### )).
:- d(( 'a###' @&@@ 'a###' )).
:- d(( (@&@@) @&@@ (@&@@) )).
:- d(( div @&@@ div )).
:- d(( @&@@ div @&@@ )).
:- d(( 'Laa' div 'Laa' )).



:- writeln(12).

:- d(( 5 + 5 )).
:- d(( '5' div '5' )).
:- d(( 5 div 5 )).
:- d(( a div c )).
:- d(( 5 @&#@ 5 )).
:- d(( 5 '@&@@a' 5 )).
:- d(( a '@&@@a' a )).
:- d(( 'a@@@@a' '@&@@a' 'a@@@@a' )).
:- d(( a@@@@a div a@@@@a )).
:- d(( (@&@@) div (@&@@) )).



:- writeln(13).

:- op(0,xfy,>>), op(999,fx,>>), op(800,xfy,?).
:- d(( >> (x,y)?z )).
:- d(( >>(x,y)?z )).

:- d( * * * ).
:- d( * * {*} ).

% falta ver:
% no codigo // ?????
% d((...)).




:- writeln(14).

:- d([(a :- (b, c))]).
:- d([(a :- b), c]).
% error    :- d([a :- b, c]).

:- op(12,xfx,:-$), op(10, xfy,',,').

:- d([(a :-$ (b',,' c))]).
:- d([(a :-$ b)',,' c]).
:- d([a :-$ b',,' c]).

:- _ = x((a :- b)).
% error    :- _ = x(a :- b).


:- writeln('').
:- writeln('---------------------------------').
:- writeln('PREFIX').

:- op(1, fy, zzz), op(1, fy, '@a@').

:- d((a -> &#@ &#@ &#@ b)).
:- d((a -> zzz zzz zzz b)).
:- d((a -> &#@ zzz zzz b)).
:- d((a -> &#@ zzz &#@ b)).
:- d((a -> zzz &#@ zzz b)).

:- d((a -> zzz '@a@' zzz b)).
:- d((a -> zzz '@a@' &#@ b)).
:- d((a -> zzz '@a@' '@a@' b)).
:- d((a -> '@a@' '@a@' '@a@' b)).
:- d((a -> '@a@' zzz '@a@' b)).
:- d((a -> '@a@' &#@  '@a@' b)).


:- writeln('').
:- writeln('---------------------------------').
:- writeln('POSTFIX').

:- op(1, yf, ppp), op(1, yf, '#a#').

:- d((a -> b #&@ #&@ #&@)).
:- d((a -> b ppp ppp ppp)).
:- d((a -> b #&@ ppp ppp)).
:- d((a -> b #&@ ppp #&@)).
:- d((a -> b ppp #&@ ppp)).

:- d((a -> b ppp '#a#' ppp)).
:- d((a -> b ppp '#a#' #&@)).
:- d((a -> b ppp '#a#' '#a#')).
:- d((a -> b '#a#' '#a#' '#a#')).
:- d((a -> b '#a#' ppp '#a#')).
:- d((a -> b '#a#' #&@ '#a#')).



:- writeln('').
:- writeln('---------------------------------').
:- writeln('VARS').

:- d((a -> g(A), A#&@ #&@ #&@)).
:- d((a -> g(A), A ppp ppp ppp)).
:- d((a -> g(A), A'#a#' '#a#' '#a#')).



/*

echo "[ops]." | ../cxprolog > lixo1 ; echo "[ops]." | cxprolog2 > lixo2 ; diff lixo1 lixo2 > lixo3

The predicates display/1, display/2, displaynl/1 and displaynl/2 now use "quoted mode", as it should always have been. For exemple, the following term is now written using quotes - '@&@@a'(a,'@&@@a') - instead of @&@@a(a,@&@@a).





:- op(200, fy, 'a+a'), op(200, fy, zzz).


:- d((a -> zzz'a+a'b)).
:- d((a -> 'a+a'zzz b)).
:- d((a -> 'a+a' 'a+a'b)).


Grammar

T -> T(1200);
T(n+1) -> T(n) for all integer n in the half-open interval [0,1200).

T(n) -> fx(n) T(n-1) except for number.
T(n) -> fy(n) T(n)
T(n) -> T(n-1) xfx(n) T(n-1)
T(n) -> T(n-1) xfy(n) T(n)
T(n) -> T(n) yfx(n) T(n-1)
T(n) -> T(n-1) xf(n)
T(n) -> T(n) yf(n)

T(1000) -> T(999) "," T(1000) The comma operator is xfy(1000).
T(0) -> a ["(" T(999) ("," T(999))* ")"] Function or constant.
T(0) -> "(" T(1200) ")'
T(0) -> "{" T(1200) "}"
T(0) -> L Lists
T(0) -> S Strings
T(0) -> N Numeric Constants
T(0) -> X Variables


N -> ["+"|"-"] Numeral
N -> ("+"|"-") ("inf" | "nan")

In order to disambiguate between an operator followed by a parenthesis versus a function-call, a requirement is imposed that "(" must immediately follow the a of the function in a function call, and "(" must be separated from fx(n) or fy(n) in an operator application.

*/


