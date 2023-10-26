#!/usr/local/bin/cxprolog --script

:- write('Testing misc... ').

/* Unit tests for misc */

:- see('test_io.pl'), get(X), writeln(X), seen ::: writeln(35).
:- see(nosuchfile) ::: throw(error(existence_error(source_sink,nosuchfile),_)).
:- see(Z) ::: throw(error(instantiation_error,_)).
:- see(123) ::: throw(error(type_error(atom,123),_)).
:- see([1]) ::: throw(error(type_error(atom,[1]),_)).
:- see(zz(1)) ::: throw(error(type_error(atom,zz(1)),_)).

:- name(Z,ola) ::: throw(error(type_error(list,ola),_)).
:- functor(X,d(1),0) ::: throw(error(type_error(atomic,d(1)),_)).
:- sub_atom(abc,2,-1,X,Y) ::: throw(error(domain_error(not_less_than_zero,-1),_)).

:- nosuchpred(2) ::: throw(error(existence_error(procedure,nosuchpred/1),_)).
:- abolish(abolish/1) ::: throw(error(permission_error(modify,static_procedure,abolish/1),_)).
:- abolish(abolish/(-1)) ::: throw(error(domain_error(not_less_than_zero,-1),_)).
:- (current_prolog_flag(max_arity,M),N is M+1,abolish(foo/N)) ::: throw(error(representation_error(max_arity), _)).

:- arg(0,atom,Z) ::: throw(error(type_error(compound,atom),_)).
:- arg(-3,foo(a,b),Z) ::: throw(error(domain_error(not_less_than_zero,-3),_)).

:- Z =\= 5 ::: throw(error(instantiation_error,_)).

:- X is floot ::: throw(error(type_error(evaluable,floot/0),_)).
:- (stream_property(S,_), X is S) ::: throw(error(type_error(evaluable,S),_)).
:- X is floot(1) ::: throw(error(type_error(evaluable,floot/1),_)).
:- X is X ::: throw(error(instantiation_error,_)).
:- X is \5.2 ::: throw(error(type_error(integer,5.2),_)).
:- X is random(5.2) ::: throw(error(type_error(integer,5.2),_)).
:- X is 5.2//4 ::: throw(error(type_error(integer,5.2),_)).
:- X is 5.2/\4 ::: throw(error(type_error(integer,5.2),_)).
:- X is 5.2\/4 ::: throw(error(type_error(integer,5.2),_)).
:- X is 5.2>>4 ::: throw(error(type_error(integer,5.2),_)).
:- X is 5.2<<4 ::: throw(error(type_error(integer,5.2),_)).
:- X is 5.2 mod 4 ::: throw(error(type_error(integer,5.2),_)).
:- X is 5.2 rem 4 ::: throw(error(type_error(integer,5.2),_)).
:- X is 5 // 0 ::: throw(error(evaluation_error(zero_divisor),_)).
:- X is [1,2] ::: throw(error(type_error(evaluable,[1,2]),_)).
:- succ(a,X) ::: throw(error(type_error(integer,a),_)).
:- succ(-1,X) ::: throw(error(domain_error(not_less_than_zero,-1),_)).
:- (M is max_int, succ(M,X)) ::: throw(error(domain_error(less_than_max_int,M),_)).
:- plus(a,Y,Z) ::: throw(error(type_error(integer,a),_)).
:- plus(X,Y,Z) ::: throw(error(instantiation_error,_)).

:- atom_chars(f(a),X) ::: throw(error(type_error(atom,f(a)),_)).
:- atom_chars(X,[a,f(b)]) ::: throw(error(type_error(character,f(b)),_)).

:- atom_concat(X,Y,f(b)) ::: throw(error(type_error(atom,f(b)),_)).

:- call(1) ::: throw(error(type_error(callable,1),_)).
:- call(X) ::: throw(error(instantiation_error,_)).

:- char_code(X,-5) ::: throw(error(representation_error(character_code),_)).


:- asserta((atom(X):-true)) ::: throw(error(permission_error(modify,static_procedure,atom/1),_)).

:- writeln('done'), exit_script.
