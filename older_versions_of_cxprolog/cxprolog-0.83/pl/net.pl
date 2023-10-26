run_server :- s.

% connect with server using "telnet localhost 30007"

s:-	net_install(30007),
	repeat,
    	net_accept(R,W),
	writeln([R,W]),
		process_new(_, main>>child(R,W), '$$_lux1'),
		close(R), close(W),
	fail.

visible child/2.
child(R,W) :-
    set_user_streams(R,W,W),
    version,
    restart.

log :-
	open('log', write, LogFile),
    set_user_streams(user,user,LogFile).
	
c:- net_connect(sunra2,30007,R,W),
	repeat,
		get_line(X), writeln(W,X),
		get_line(R,Y), writeln(Y),
	fail.
	
