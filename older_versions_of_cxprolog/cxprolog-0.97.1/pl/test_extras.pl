#!/usr/local/bin/cxprolog --script

/* Unit tests for io */

test_dict :-
	dict_new(x),
	dict_set(x,1,um),
	dict_set(x,v,vv),
	dict_set(x,2,dois),
	dict_get(x,1,um).

:-	test_dict
::: true.

:- writeln(done), exit_script.
