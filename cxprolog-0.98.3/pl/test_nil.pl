#!/usr/local/bin/cxprolog --flags nil_is_special=true --script

:- write('Testing nil... ').

/* Unit tests for nil */

:-	writeln(['[]',[]])
::: writeln('[[],[]]').

:-	writeqln(['[]',[]])
::: writeln('[''[]'',[]]').

:-	[] == '[]'
::: false.

:-	atom([])
::: false.

:-	atom('[]')
::: true.

:-	atomic([])
::: false.

:-	callable([])
::: true.

:-	[]
::: true.

:-	[] =.. X, writeln(X)
::: writeln([[]]).

:-	name([], _)
::: fail.

:- atom_termq('[]','[]')
::: fail.

:- atom_termq('''[]''','[]')
::: true.

:- writeln('done'), exit_script.
