% Server example

% You connect with this server using "telnet localhost 30007"

server:-
% install server
    net_install(30007),
	repeat,    % repeat-fail cicle
% wait next incoming connection
    	net_accept(R,W),
% spawn child process to handle the connection
		process_new(_, main>>child_start(R,W), main>>child_restart),
% father process close streams but child continues running
		close(R), close(W),
	fail.

visible child_start/2.
child_start(R,W) :-
    '$set_user_streams'(R,W,W),
    version,
    restart.

visible child_restart/0.
child_restart :-
	try main>>'$cxprolog_top_level_goal',
	restart.

% End of server example
% --------------------------------------

log :-
	open('log_file', write, LogFile),
    '$set_user_streams'(user,user,LogFile).
	
c:- net_connect(sunra2,30007,R,W),
	repeat,
		get_line(X), writeln(W,X),
		get_line(R,Y), writeln(Y),
	fail.
	
