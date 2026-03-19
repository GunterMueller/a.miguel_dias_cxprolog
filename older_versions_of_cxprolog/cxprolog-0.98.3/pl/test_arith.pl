#!/usr/local/bin/cxprolog --script

:- write('Testing arith... ').

:- X is not_arith ::: throw(error(type_error(evaluable,not_arith/0),_)).
:- X is X ::: throw(error(instantiation_error,_)).

:- A = 1, X is -A, integer(X), X == -1 ::: true.
:- A = 5.2, X is -A, float(X), X == -5.2 ::: true.

:- X is 1+1, integer(X), X == 2 ::: true.
:- X is 5.2+1, float(X), X == 6.2 ::: true.
:- X is max_int + 1, float(X) ::: true.

:- X is 1-1, integer(X), X == 0 ::: true.
:- X is 5.2-1, float(X), X == 4.2 ::: true.
:- X is min_int - 1, float(X) ::: true.

:- X is 1*1, integer(X), X == 1 ::: true.
:- X is 5.2*1, float(X), X == 5.2 ::: true.

:- X is 1/1, integer(X), X == 1 ::: true.
:- X is 5.2/1, float(X), X == 5.2 ::: true.
:- X is 5 / 0 ::: throw(error(evaluation_error(zero_divisor),_)).
:- X is 5 / 0.0 ::: throw(error(evaluation_error(zero_divisor),_)).

:- X is 5//2, integer(X), X == 2 ::: true.
:- X is -5//2, integer(X), X == -2 ::: true.
:- X is 5// -2, integer(X), X == -2 ::: true.
:- X is -5// -2, integer(X), X == 2 ::: true.
:- X is 5//2, integer(X), X == 2 ::: true.
:- X is 5 // 0 ::: throw(error(evaluation_error(zero_divisor),_)).
:- X is 5.2//2 ::: throw(error(type_error(integer,5.2),_)).

:- X is 5 rem 2, integer(X), X == 1 ::: true.
:- X is -5 rem 2, integer(X), X == -1 ::: true.
:- X is 5 rem -2, integer(X), X == 1 ::: true.
:- X is -5 rem -2, integer(X), X == -1 ::: true.
:- X is 5 rem 0 ::: throw(error(evaluation_error(zero_divisor),_)).
:- X is 5.2 rem 2 ::: throw(error(type_error(integer,5.2),_)).

:- X is 5 div 2, integer(X), X == 2 ::: true.
:- X is -5 div 2, integer(X), X == -3 ::: true.
:- X is 5 div -2, integer(X), X == -3 ::: true.
:- X is -5 div -2, integer(X), X == 2 ::: true.
:- X is 5 div 0 ::: throw(error(evaluation_error(zero_divisor),_)).
:- X is 5.2 div 2 ::: throw(error(type_error(integer,5.2),_)).

:- X is 5 mod 2, integer(X), X == 1 ::: true.
:- X is -5 mod 2, integer(X), X == 1 ::: true.
:- X is 5 mod -2, integer(X), X == -1 ::: true.
:- X is -5 mod -2, integer(X), X == -1 ::: true.
:- X is 5 mod 0 ::: throw(error(evaluation_error(zero_divisor),_)).
:- X is 5.2 mod 2 ::: throw(error(type_error(integer,5.2),_)).

:- X is 2**4, float(X), X == 16.0 ::: true.
:- X is 2.0**4, float(X), X == 16.0 ::: true.
:- X is 2**4.0, float(X), X == 16.0 ::: true.

:- X is 2^4, float(X), X == 16.0 ::: true.
:- X is 2.0^4, float(X), X == 16.0 ::: true.
:- X is 2^4.0, float(X), X == 16.0 ::: true.

:- X is exp(2,4), float(X), X == 16.0 ::: true.
:- X is exp(2.0,4), float(X), X == 16.0 ::: true.
:- X is exp(2,4.0), float(X), X == 16.0 ::: true.

:- X is 2\/8, integer(X), X == 10 ::: true.
:- X is 5.2\/4 ::: throw(error(type_error(integer,5.2),_)).

:- X is 2/\10, integer(X), X == 2 ::: true.
:- X is 5.2/\4 ::: throw(error(type_error(integer,5.2),_)).

:- X is 2<<3, integer(X), X == 16 ::: true.
:- X is 5.2<<4 ::: throw(error(type_error(integer,5.2),_)).

:- X is 16>>3, integer(X), X == 2 ::: true.
:- X is 5.2>>4 ::: throw(error(type_error(integer,5.2),_)).

:- X is \3, integer(X), X == -4 ::: true.
:- X is \5.2 ::: throw(error(type_error(integer,5.2),_)).

:- X is abs(-3), integer(X), X == 3 ::: true.
:- X is abs(-5.2), float(X), X == 5.2 ::: true.

:- X is acos(1), float(X), X == 0.0 ::: true.
:- X is asin(0), float(X), X == 0.0 ::: true.
:- X is atan(0), float(X), X == 0.0 ::: true.
:- X is ceiling(5.5), integer(X), X == 6 ::: true.
:- X is cos(0), float(X), X == 1.0 ::: true.
:- X is exp(1), float(X), Y is e, X == Y ::: true.
:- X is float(1), float(X), X == 1.0 ::: true.

:- X is float_fractional_part(5.2), float(X), abs(X - 0.2)<epsilon ::: true.
:- X is float_fractional_part(-5.2), float(X), abs(X + 0.2)<epsilon ::: true.

:- X is float_integer_part(5.2), integer(X), X == 5 ::: true.
:- X is float_integer_part(-5.2), integer(X), X == -5 ::: true.

:- X is floor(5.2), integer(X), X == 5 ::: true.
:- X is floor(-5.2), integer(X), X == -6 ::: true.

:- X is integer(5.2), integer(X), X == 5 ::: true.
:- X is integer(-5.2), integer(X), X == -5 ::: true.

:- X is log(e), float(X), X == 1.0 ::: true.

%:- X is log10(10), float(X), X == 1.0 ::: true.

:- X is max(5.2, 1), float(X), X == 5.2 ::: true.
:- X is max(5.2, 10), integer(X), X == 10 ::: true.

:- X is min(5.2, 10), float(X), X == 5.2 ::: true.
%:- X is min(5.2, 1), integer(X), X == 1 ::: true.

:- X is random(10), integer(X) ::: true.
:- X is random(5.2) ::: throw(error(type_error(integer,5.2),_)).

:- X is round(5.4), integer(X), X == 5 ::: true.
:- X is round(5.5), integer(X), X == 6 ::: true.
:- X is round(5.6), integer(X), X == 6 ::: true.
:- X is round(-5.4), integer(X), X == -5 ::: true.
:- X is round(-5.5), integer(X), X == -6 ::: true.
:- X is round(-5.6), integer(X), X == -6 ::: true.

:- X is sin(pi/2), float(X), X == 1.0 ::: true.

:- X is sign(pi), integer(X), X == 1 ::: true.
:- X is sign(-pi), integer(X), X == -1 ::: true.
:- X is sign(0), integer(X), X == 0 ::: true.

:- X is sqrt(4), float(X), X == 2.0 ::: true.
:- X is tan(0), float(X), X == 0.0 ::: true.

:- X is truncate(5.2), integer(X), X == 5 ::: true.
:- X is truncate(-5.2), integer(X), X == -5 ::: true.

:- X is cputime, float(X) ::: true.
:- X is currtime, integer(X) ::: true.
:- X is e, float(X) ::: true.
:- X is epsilon, float(X) ::: true.
:- X is float_size, integer(X) ::: true.
:- X is global, integer(X) ::: true.
:- X is heapused, integer(X) ::: true.
:- X is int_size, integer(X) ::: true.
:- X is inf, float(X) ::: true.
:- X is local, integer(X) ::: true.
:- X is max_int, integer(X) ::: true.
:- X is min_int, integer(X) ::: true.
:- X is nan, float(X) ::: true.
:- X is pi, float(X) ::: true.
:- X is random, float(X) ::: true.

:- X is [1], integer(X) ::: true.
:- X is [5.2], float(X) ::: true.
:- X is [1,2] ::: throw(error(type_error(evaluable,[1,2]),_)).

:- succ(1,2) ::: true.
:- succ(a,X) ::: throw(error(type_error(integer,a),_)).
:- succ(-1,X) ::: throw(error(domain_error(not_less_than_zero,-1),_)).

:- plus(1,2,3) ::: true.
:- plus(a,Y,Z) ::: throw(error(type_error(integer,a),_)).
:- plus(X,Y,Z) ::: throw(error(instantiation_error,_)).
:- M is max_int, succ(M,X) ::: throw(error(domain_error(less_than_max_int,M),_)).

:- writeln('done'), exit_script.
