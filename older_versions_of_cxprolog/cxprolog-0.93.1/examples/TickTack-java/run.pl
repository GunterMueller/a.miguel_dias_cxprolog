#!/usr/local/bin/cxprolog -script

run :-
	(fs_exists('MyJFrame.class') -> true ; os_run('javac *.java')),
	java_call('MyJFrame', '<init>:(Ljava/lang/String;)V', [ola], Frame),
    repeat,
		gui_event_get(event(Frame,Event)),
		writeln(event(Frame,Event)),
	Event == stop,
	java_call(Frame, 'dispose:()V', [], _).

:- run, exit.


/*

Running this program:

	cd examples/TickTack-java
	run.pl


Requirements:

  Shell variable defined:
	export CLASSPATH=/usr/local/lib/cxprolog:.

  Instaled files:
	/usr/local/bin/cxprolog
	/usr/local/lib/cxprolog/java.pl
	/usr/local/lib/cxprolog/CxProlog.java
	/usr/local/lib/cxprolog/CxProlog.class

	examples/TickTack-java/run.pl
	examples/TickTack-java/ColoredButton.java
	examples/TickTack-java/MyJFrame.java
	examples/TickTack-java/TickTack.java
	examples/TickTack-java/ColoredButton.class
	examples/TickTack-java/MyJFrame.class
	examples/TickTack-java/MyJFrame$1.class
	examples/TickTack-java/MyJFrame$2.class
	examples/TickTack-java/MyJFrame$3.class
	examples/TickTack-java/TickTack.class
*/
