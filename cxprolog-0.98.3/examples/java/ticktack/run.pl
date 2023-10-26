#!/usr/local/bin/cxprolog --script

run :-
	java_compile('*.java'),
	java_new('ticktack/MyJFrame', 'Ljava/lang/String;', [ola], Frame),
    repeat,
		foreign_event_get(event(Frame,Event)),
		writeln(event(Frame,Event)),
	Event == stop,
	java_call(Frame, 'dispose:()V', [], _),
	java_clean(ticktack).

:- run, exit_script_fast.
