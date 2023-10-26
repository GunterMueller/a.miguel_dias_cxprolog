
run :-
	java_new('ticktack2/MyJFrame', 'Ljava/lang/String;', [ola], Frame),
    repeat,
		java_event_get(event(Frame,Event)),
		writeln(event(Frame,Event)),
	Event == stop,
	java_call(Frame, 'close:()V', [], _),
	writeln(bye).
