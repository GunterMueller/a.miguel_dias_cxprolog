a :-
system('date > /tmp/date'),
open('/tmp/date', read, S),
get_line(S, D),
close(S),
writeln(D).
