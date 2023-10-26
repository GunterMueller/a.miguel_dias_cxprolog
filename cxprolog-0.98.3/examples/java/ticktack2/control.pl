
ttt(X) :-
	buffer_new(b),
	buffer_set_text_contents(b, X),
	open_buffer_stream(b,read,T),
	read(T,W),
	writeln(W),
	close(T).
	
qq :- ttt('a(123).').


mythread2 :- '$cxprolog_top_level_goal'.

change_streams :- buffer_new(b), buffer_set_text_contents(b, 'a(123).'), open_buffer_stream(b,read,s), set_input(s).


mythread3 :- between(0,10000,X), write(X), os_sleep(0.04), write('.'), true, fail.


mythread :- java_coroutining_input_line, between(0,10000,X), write(X), os_sleep(0.04), write('.'), get_char(Y), writeln(Y), write('*'), true, fail.

% read(Y), writeln(Y), 


run :-
	thread_new(mythread,mythread,halt),
	threads,
	streams,
	java_coroutining_start,
	java_new('ticktack2/MyJFrame', 'Ljava/lang/String;', [ola], Frame),
    repeat,
		java_event_get(event(F,Event)),
		event_process(event(F,Event)),
	Event == stop,
	java_coroutining_stop,
	java_call(Frame, 'close:()V', [], _),
	writeln(bye).

event_process(event(noframe, runabit)) :- !, thread_run_a_bit(mythread), java_coroutining_run_a_bit_done.
event_process(event(_,color(_))) :- java_coroutining_input_line.
event_process(event(_,_)) :- writeln(''), writeln(event(F,E)).

java_coroutining_start :- java_call('prolog/Prolog', 'coroutiningStart:()V', [], _).
java_coroutining_stop :- java_call('prolog/Prolog', 'coroutiningStop:()V', [], _).
java_coroutining_run_a_bit_done :- java_call('prolog/Prolog', 'coroutiningRunABitDone:()V', [], _).
java_coroutining_input_line :- java_call('prolog/Prolog', 'coroutiningInputLine:(Ljava/lang/String;Ljava/lang/String;)V', [mythread, 'ola.\n'], _).
