on_interrupt(debug) :- writeln(d), debug.
on_interrupt(trace) :- writeln(t), trace.
on_interrupt(abort) :- writeln(a), throw(exxxxxc), writeln(b).
on_interrupt(continue) :- writeln(c), true.
on_interrupt(exit) :- writeln(e), exit.
loop:-loop.

run :- catch(loop, X, true), writeln(X), writeln(end).

