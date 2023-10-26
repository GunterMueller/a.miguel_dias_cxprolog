% Server example

% You connect with this server using "telnet localhost 30007"

server:-
    net_install(30007),
	repeat,    % repeat-fail cicle
        writeln('--- Wait next incoming connection...'),
    	net_accept(R,W),
streams,
        writeln('--- Connection established.'),
        writeln([R, W]),
		get0(R, X), writeln(X),
		close(R), close(W),
	fail.

noserver :-
	net_uninstall.

c:- net_connect(localhost,30007,R,W),
	writeln([R,W]),
	writeln(W, 'ola.\n').
	
