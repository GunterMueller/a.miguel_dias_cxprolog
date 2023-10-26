s :-
	ysocket_install(3007),
	xappl_launch('/usr/bin/bc'),
	xappl_set_timeout(2),
	xappl_set_prompt('
'),
	handle_requests,
	xappl_close.

handle_requests :-
	not comms.
comms :-
	repeat,
		ysocket_get(X1), str_write(X1),
		append("nano=bc&data=",X,X1),
		(X = [10] ->
			html_reply("",""),
			writeln('END'),
			!
		 ;
			xappl_send(X),
			xappl_receive(Y), str_write(Y),
			html_reply(X,Y)
		),
	fail.


html_reply(X,Y) :-

Start =
"Content-type: text/html


<HTML>
<HEAD>
</HEAD>
<BODY>
",

End =
"</BODY>
</HTML>
",

	append(X, " = ", X1),
	append(X1, Y, XY),
	append(Start, XY, XY1),
	append(XY1, End, XY2),
	ysocket_reply(XY2).


append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).
