#!/usr/local/bin/cxprolog --script

:- writeln('Testing io\n').

/* Unit tests for io */

test_buffer_io(L,Encoding) :-
	buffer_new(B),
	open_buffer_stream(B,write,S1,[encoding(Encoding)]),
	name(T,L),
	write(S1,T),
	close(S1),
	open_buffer_stream(B,read,S2,[encoding(Encoding)]),
	read(S2,X),
	close(S2),
	buffer_delete(B),
	concat([X,'.\n'],T).

test_file_io(L,Encoding) :-
	FileName = 'test_file_io_1.txt',
	open(FileName,write,S1,[encoding(Encoding),bom(true)]),
	name(T,L),
	write(S1,T),
	close(S1),
	open(FileName,read,S2),    % relies on bom
	read(S2,X),
	close(S2),
	fs_delete(FileName),
	concat([X,'.\n'],T).

:-	test_buffer_io([111], iso_latin_1), true
::: true.

:-	test_file_io([111], iso_latin_1), true
::: true.

:- writeln(done), exit_script.
