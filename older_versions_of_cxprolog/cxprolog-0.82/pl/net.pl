s :-
	server_install(14000),
    server_wait_connection(R,W),
    set_user_streams(R,W,W),
    version,
    restart.

:- s.
