#!/usr/local/bin/cxprolog --script

:- writeln('IN CASE OF ANY PROBLEM, PLEASE SEND THE AUDIT FILE TO "amd@fct.unl.pt".').

:- flags(store), flags(defaults).

% Sets the audit level to maximum detail
:- set_prolog_flag(sys_trace, detailed).

% Forces the stack-shifter to be called thousands of times per second
:- set_prolog_flag(test_relocation, true).

% Forces the garbage-collector of extras to be called thousands of times per second
:- set_prolog_flag(test_garbage_collection, 1).

% Checks invariants at each call
:- set_prolog_flag(test_invariants, true).

% print cxprolog state
:- nl, features, nl, flags, nl.

:- writeln('*** THE TESTS ARE STARTING NOW... ').
:- '$$_utests_start'.
:- silent_consult(test_arith).
:- silent_consult(test_call_cleanup).
:- silent_consult(test_cut).
:- silent_consult(test_exceptions).
:- silent_consult(test_extras).
:- silent_consult(test_ins_preds).
:- silent_consult(test_format).
:- silent_consult(test_io).
:- silent_consult(test_java).
:- silent_consult(test_wxwidgets).
:- silent_consult(test_par).
:- silent_consult(test_portray).
:- silent_consult(test_reentrancy).
:- silent_consult(test_subsumes).
:- silent_consult(test_misc).
:- '$$_utests_ok'
	-> 	write('*** ALL THE TESTS PASSED!'),
		'$$_delete_sys_trace_file',
		writeln(' THE AUDIT FILE WAS DELETED.')
	;	write('*** MISTAKES WERE DETECTED.'),
		writeln(' PLEASE, SEND THE AUDIT FILE TO "amd@fct.unl.pt".').

:- nl, flags(restore), exit_script.
