#!/usr/local/bin/cxprolog --script

:- write('Testing io... ').

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
	%fs_delete(FileName),
	concat([X,'.\n'],T).

:-	current_prolog_flag(encoding,Curr),
	test_buffer_io([111,108,233,225,237,234,97,97,0'.,10], Curr),
	test_buffer_io([27700,122,111070,0'.,10], utf8),
	test_buffer_io([27700,122,111070,0'.,10], utf16be),
	test_buffer_io([27700,122,111070,0'.,10], utf16le),
	test_buffer_io([27700,122,111070,0'.,10], utf32be),
	test_buffer_io([27700,122,111070,0'.,10], utf32le),
	test_buffer_io([27700,122,6666,0'.,10], unicode_be),
	test_buffer_io([27700,122,6666,0'.,10], unicode_le),
	test_buffer_io([27700,122,6666,0'.,10], unicode),
	test_buffer_io([27700,122,66666,0'.,10], utf16),
	test_buffer_io([27700,122,66666,0'.,10], utf32)
::: true.


:-	current_prolog_flag(encoding,Curr),
	test_file_io([111,108,233,225,237,234,97,97,0'.,10], Curr),
	test_file_io([27700,122,111070,0'.,10], utf8),
	test_file_io([27700,122,111070,0'.,10], utf16be),
	test_file_io([27700,122,111070,0'.,10], utf16le),
	test_file_io([27700,122,111070,0'.,10], utf32be),
	test_file_io([27700,122,111070,0'.,10], utf32le),
	test_file_io([27700,122,6666,0'.,10], unicode_be),
	test_file_io([27700,122,6666,0'.,10], unicode_le),	
	test_file_io([27700,122,6666,0'.,10], unicode),	
	test_file_io([27700,122,66666,0'.,10], utf16),	
	test_file_io([27700,122,66666,0'.,10], utf32)
::: true.

:- writeln('done'), exit_script.
