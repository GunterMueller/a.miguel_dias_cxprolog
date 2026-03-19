% CXPROLOG - JAVA GUI   (A.Miguel Dias - 2004)

% EVENTS: gui_event_get/1, gui_event_is_available/0, gui_event_discard_all/0
% Requires class CxProlog (in file CxProlog.java)

gui_event_get(Event) :-
	java_call('CxProlog', 'GetNextEvent:()Ljava/lang/String;', [], E),
	java_convert('Ljava/lang/String;', E, Event).

gui_event_is_available :-
	java_call('CxProlog', 'HowManyEvents:()I', [], N),
	N > 0.

gui_event_discard_all :-
	java_call('CxProlog', 'DiscardEvents:()V', [], _).


% FILE CHOOSER: gui_file_chooser/2, gui_file_chooser_multiple/2

'$$_gui_file_chooser_process'([],[]).
'$$_gui_file_chooser_process'([X|Xs],[Z|Zs]) :-
	java_call(X, 'getPath:()Ljava/lang/String;', [], Name),
	java_convert('Ljava/lang/String;', Name, Name2),
	'$fs_atom_path'(Name2, Z),
	'$$_gui_file_chooser_process'(Xs,Zs).

gui_file_chooser(Mesg, Path) :-
	java_call('javax/swing/JFileChooser', '<init>:()V', [], Chooser),
	java_call(Chooser, 'showDialog:(Ljava/awt/Component;Ljava/lang/String;)I', [null,Mesg], Result),
	java_field('javax/swing/JFileChooser', 'APPROVE_OPTION:I', ApproveOp, ApproveOp),
	(Result == ApproveOp
		->  java_call(Chooser, 'getSelectedFile:()Ljava/io/File;', [], File),
			'$$_gui_file_chooser_process'([File],[Path])).
		
gui_file_chooser_multiple(Mesg, Paths) :-
	java_call('javax/swing/JFileChooser', '<init>:()V', [], Chooser),
	java_call(Chooser, 'setMultiSelectionEnabled:(Z)V', [true], _),
	java_call(Chooser, 'showDialog:(Ljava/awt/Component;Ljava/lang/String;)I', [null,Mesg], Result),
	java_field('javax/swing/JFileChooser', 'APPROVE_OPTION:I', ApproveOp, ApproveOp),
	(Result == ApproveOp
		->  java_call(Chooser, 'getSelectedFiles:()[Ljava/io/File;', [], Files),
			java_convert('[Ljava/io/File;', Files, FileList),
			'$$_gui_file_chooser_process'(FileList,Paths)).


% CXPROLOG - JAVA GUI
java_test :-
 	java_call('MyJFrame', '<init>:(Ljava/lang/String;)V', [ola], Frame).
