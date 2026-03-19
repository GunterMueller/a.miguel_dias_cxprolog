#!/usr/local/bin/cxprolog --script

:- write('Testing wxwidgets...').

/* Unit tests for Java */

test :-
	wxw('Testing wxwidgets...', W),
	wxt(W, 'Testing wxwidgets...'),
	os_sleep(2),
	wxc(W).

:- test
::: true
<<: current_prolog_flag(wxwidgets_available,true).

:- writeln('done'), exit_script.
