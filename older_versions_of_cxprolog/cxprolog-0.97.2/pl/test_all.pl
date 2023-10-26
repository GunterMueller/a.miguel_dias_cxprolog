#!/usr/local/bin/cxprolog --script

:- os_run('test_call_cleanup.pl').
:- os_run('test_cut.pl').
:- os_run('test_exceptions.pl').
:- os_run('test_extras.pl').
:- os_run('test_io.pl').
:- os_run('test_java.pl').
:- os_run('test_reentrancy.pl').

:- writeln(done), exit_script.
