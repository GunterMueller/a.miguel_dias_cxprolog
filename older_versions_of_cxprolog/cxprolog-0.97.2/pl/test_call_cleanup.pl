#!/usr/local/bin/cxprolog --script

:- writeln('Testing call_cleanup\n').

/* Unit tests for call_cleanup */

:- (call_cleanup(writeln(1), (writeln(cleanup);writeln(ignored))), writeln(continuation), fail) ; writeln(done)
::: writeln('1\ncleanup\ncontinuation\ndone').


:- (call_cleanup(writeln(1), (writeln(cleanup);writeln(ignored))), writeln(continuation), fail) ; writeln(done)
::: writeln('1\ncleanup\ncontinuation\ndone').


:- (call_cleanup((writeln(1);writeln(2)), (writeln(cleanup);writeln(ignored))), writeln(continuation), fail) ; writeln(done)
::: writeln('1\ncontinuation\n2\ncleanup\ncontinuation\ndone').


:- (call_cleanup(fail, (writeln(cleanup);writeln(ignored))), writeln(continuation), fail) ; writeln(done)
::: writeln('cleanup\ndone').


:- (call_cleanup((writeln(1);writeln(2);fail), (writeln(cleanup);writeln(ignored))), writeln(continuation), fail) ; writeln(done)
::: writeln('1\ncontinuation\n2\ncontinuation\ncleanup\ndone').


:- (call_cleanup((writeln(1);!;writeln(2)), (writeln(cleanup);writeln(ignored))), writeln(continuation), fail) ; writeln(done)
::: writeln('1\ncontinuation\ncleanup\ncontinuation\ndone').


:- (call_cleanup((writeln(1);writeln(2)), (writeln(cleanup);writeln(ignored))), writeln(continuation), !, fail) ; writeln(done)
::: writeln('1\ncontinuation\ncleanup'), fail.


:- (call_cleanup((writeln(1);writeln(2)), (writeln(cleanup);writeln(ignored))), !, writeln(continuation), fail) ; writeln(done)
::: writeln('1\ncleanup\ncontinuation'), fail.


:- (call_cleanup((writeln(1);writeln(2)), fail), writeln(continuation), fail) ; writeln(done)
::: writeln('1\ncontinuation\n2\ncontinuation\ndone').


:- call_cleanup(see(nosuchfile), writeln(cleanup)), writeln(continuation)
::: writeln('cleanup'), throw(_).


:- call_cleanup((see(nosuchfile);fail), writeln(cleanup)), writeln(continuation)
::: writeln('cleanup'), throw(_).


:- catch((call_cleanup(see(nosuchfile), writeln(cleanup)), writeln(continuation)), _, true)
::: writeln('cleanup').


:- call_cleanup((call_cleanup((writeln(1);writeln(2)), writeln(cleanup1))), writeln(cleanup2)), fail
::: writeln('1\n2\ncleanup1\ncleanup2'), fail.


:- catch((call_cleanup((call_cleanup((writeln(1);see(nosuchfile)), writeln(cleanup1))), writeln(cleanup2)), fail), _, true) 
::: writeln('1\ncleanup1\ncleanup2').


:- catch((call_cleanup((call_cleanup((see(nosuchfile);writeln(2)), writeln(cleanup1))), writeln(cleanup2)), fail), _, true)
::: writeln('cleanup1\ncleanup2').


:- writeln(done), exit_script.
