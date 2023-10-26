#!/usr/local/bin/cxprolog --script

run :-
	test_fields,
	test_calls,
	test_arrays,
	test_conversions,
	test_subtypes,
	writeln(done).

%*** TEST FIELDS ********************************************
test_fields :-
	writeln(''), writeln('FIELDS'),
	java_call('Test', '<init>:()V', [], R0), java_field(R0, 'z:Z', A, A), writeln(A),
	java_call('Test', '<init>:()V', [], R1), java_field(R1, 'b:B', B, B), writeln(B), 
	java_call('Test', '<init>:()V', [], R2), java_field(R2, 'c:C', C, C), writeln(C), 
	java_call('Test', '<init>:()V', [], R3), java_field(R3, 's:S', D, D), writeln(D), 
	java_call('Test', '<init>:()V', [], R4), java_field(R4, 'i:I', E, E), writeln(E), 
	java_call('Test', '<init>:()V', [], R5), java_field(R5, 'j:J', F, F), writeln(F), 
	java_call('Test', '<init>:()V', [], R6), java_field(R6, 'f:F', G, G), writeln(G), 
	java_call('Test', '<init>:()V', [], R7), java_field(R7, 'd:D', H, H), writeln(H), 
	%java_call('Test', '<init>:()V', [], R8), java_field(R8, 'str:Ljava/lang/String;', I, I), java_convert('Ljava/lang/String;',I,Txt), writeln(Txt), 
	java_call('Test', '<init>:()V', [], R9), java_field(R9, 'ad:[D', J, J), writeln(J).
	
test_fields_err :-	
	java_call('Test', '<init>:()V', [], R), java_field(R, 'zz:Z', Old, Old).

%*** TEST CALLS *********************************************
test_calls :-
	writeln(''), writeln('CALLS'),
	java_call('Test', 'main:([Ljava/lang/String;)V', [['Miguel', 'ola']], Z), writeln(Z),
	java_call('Test', '<init>:()V', [], R1), java_call(R1, 'toBool:(I)Z', [33], Z1), writeln(Z1),
	java_call('Test', '<init>:()V', [], R2), java_call(R2, 'toByte:(I)B', [33], Z2), writeln(Z2),
	java_call('Test', '<init>:()V', [], R3), java_call(R3, 'toChar:(I)C', [33], Z3), writeln(Z3),
	java_call('Test', '<init>:()V', [], R4), java_call(R4, 'toShort:(I)S', [33], Z4), writeln(Z4),
	java_call('Test', '<init>:()V', [], R5), java_call(R5, 'toInt:(I)I', [33], Z5), writeln(Z5),
	java_call('Test', '<init>:()V', [], R6), java_call(R6, 'toLong:(I)J', [33], Z6), writeln(Z6),
	java_call('Test', '<init>:()V', [], R7), java_call(R7, 'toFloat:(I)F', [33], Z7), writeln(Z7),
	java_call('Test', '<init>:()V', [], R8), java_call(R8, 'toDouble:(I)D', [33], Z8), writeln(Z8),
	java_call('Test', '<init>:()V', [], R9), java_call(R9, 'toString:(I)Ljava/lang/String;', [33], Z9), java_convert('Ljava/lang/String;',Z9,S9), writeln(S9),
	java_call('Test', '<init>:()V', [], R10), java_call(R10, 'doubleString:(Ljava/lang/String;)Ljava/lang/String;', ['ola'], Z10), java_convert('Ljava/lang/String;',Z10,S10), writeln(S10),
	java_call('Test', '<init>:()V', [], R11), java_call(R11, 'toString:()Ljava/lang/String;', [], N11), java_convert('Ljava/lang/String;',N11,Z11), writeln(Z11),
	java_call('Test', 'toString:()Ljava/lang/String;', [], Z12), java_convert('Ljava/lang/String;', Z12, S12), writeln(S12),
	java_call('Test', 'getName:()Ljava/lang/String;', [], Z13), java_convert('Ljava/lang/String;', Z13, S13), writeln(S13),
	java_call('Z', 'getName:()Ljava/lang/String;', [], Z14), java_convert('Ljava/lang/String;', Z14, S14), writeln(S14).

test_calls_err :-	
	java_call('Test', '<init>:()V', [], R), java_call(R, 'ola:(I)V', [20], _).

%*** TEST ARRAYS ********************************************
test_arrays :-
	writeln(''), writeln('ARRAYS'),
	test_array1,
	test_array2,
	test_array3,
	test_array4,
	test_array5.

test_array1 :-
	java_call('Test', '<init>:()V', [], R),
	java_call(R, 'arrString:([Ljava/lang/String;I)Ljava/lang/String;', [['ola','ole','oli'], 1], Z),
	java_call(Z, 'concat:(Ljava/lang/String;)Ljava/lang/String;', [Z], ZZ),
	java_convert('Ljava/lang/String;',ZZ,S),
	writeln(S).

test_array2 :-
	java_call('Test', '<init>:()V', [], R),
	java_call(R, 'arrString2:([[Ljava/lang/String;II)Ljava/lang/String;', [[['ola','ole','oli'], [za,zb,zc,zd]], 1,3], Z),
	java_convert('Ljava/lang/String;',Z,S).

test_array3 :-
	java_call('Test', '<init>:()V', [], R),
	java_call(R, 'arrInt:([[[IIII)I', [[[[1,2,3], [11,12, 13, 14]], [[8, 9]]], 1,0,1], Z).

test_array4 :-
	java_call('[I', '<init>:(I)V', [30], R0), java_convert('[I', R0, Z0), writeln(Z0),
	java_call('[[Ljava/lang/String;', '<init>:(I)V', [5], R1), java_convert('[[Ljava/lang/String;', R1, Z1), writeln(Z1).
	
test_array5 :-
	java_field('Ljava/lang/Integer;','TYPE:Ljava/lang/Class;', C,C), writeln(C),
	java_call('Ljava/lang/reflect/Array;', 'newInstance:(Ljava/lang/Class;I)Ljava/lang/Object;', [C, 5], R),
	java_convert('[I',R,Z), writeln(Z).

st_arrays_err :-
	java_call('[V', '<init>:(I)V', [30], R).

%*** TEST CONVERSIONS ***************************************
test_conversions :-
	writeln(''), writeln('CONVERSIONS'),
	java_convert('Z',true,X0), writeln(X0),
	java_convert('B',127,X1), writeln(X1),
	java_convert('B',X2,127), writeln(X2),
	java_convert('D',X3,127), writeln(X3),
	java_convert('[D',X4,null), writeln(X4),
	java_convert('Ljava/lang/String;',X5,'ola'), java_convert('Ljava/lang/String;',X5,Z5), writeln(Z5),
	java_convert('[I',X6,[1,2,3,4,5]), java_convert('[I',X6,Z6), writeln(Z6),
	java_convert('[[I',X7,[[1],[2],[3],[4],[5]]), java_convert('[[I',X7,Z7), writeln(Z7),
	java_convert('[Ljava/lang/String;',X8,[ola,ole,oli,olo,olu]), java_convert('[Ljava/lang/String;',X8,Z8), writeln(Z8),
	java_convert('[I',X9,[10,20,30,40,50]), java_array('[I',X9,3,Z9,99999), writeln(Z9),
	java_convert('Ljava/lang/Class;',X10,'Test'), java_convert('Ljava/lang/Class;',X10,Z10), writeln(Z10),
	java_convert('Ljava/lang/Class;',X11,'[I'), java_convert('Ljava/lang/Class;',X11,Z11), writeln(Z11),
	java_convert('Ljava/lang/Class;',X12,'I'), writeln(X12), java_convert('Ljava/lang/Class;',X12,Z12), writeln(Z12),
	java_convert('Ljava/lang/Class;',X,'I'), writeln(X), java_field('Ljava/lang/Integer;', 'TYPE:Ljava/lang/Class;',Y,Y),
														writeln(Y), (X==Y -> writeln(same) ; writeln(different)).
	
test_conversions_err :-
	java_convert('D',X4,null).

%*** TEST null **********************************************
test_null_err1 :-
	java_call(null, 'a:()I', [], R).

test_null_err2 :-
	java_field(null, 'a:[I', Old, R).

test_null_err3 :-
	java_field(null, 'a:[I', Old, R).

%*** TEST subtypes ******************************************
test_subtypes :-
	writeln(''), writeln('SUBTYPES'),
	test_subtype1.

test_subtype1 :-
	java_call('Test', '<init>:()V', [], R),
	java_field(R, 'as:[Ljava/lang/String;', Old, Old),
	java_field(R, 'ob:Ljava/lang/Object;', Old2, Old),
	writeln(ok).

test_subtype_err1 :-
	java_call('Test', '<init>:()V', [], R),
	java_field(R, 'ob:Ljava/lang/Object;', Old, Old),
	java_field(R, 'as:[Ljava/lang/String;', Old2, Old).
	
test_subtype_err2 :-
	java_call('Test', '<init>:()V', [], R),
	java_field(R, 'ad:[D', Old, Old),
	java_field(R, 'as:[Ljava/lang/String;', Old2, Old).
	
%************************************************************

:- run, exit.

/*

FIELDS: all type-checked when set
   JAVA          PROLOG
Z boolean      'false', 'true'
B byte         integer [-128..127]
C char         char
S short        integer [-32768..32767]
I int          integer
J long         number (float used if number does not fit an integer)
F float        number
D double       number
L object       java-object extra (subtypes considered
[ array        java-object extra   (java_convert/3 convert arrays to lists)

SAY: Intercepts all java exceptions

Testing for the need for ids cache

t :-
	X is cputime,
	a := X,
	java_call('Test', '<init>:()V', [], R),
    repeat(100),
	java_field(R, 'z:Z', Old, Old),
writeln(Old),
	fail.
t :-
	a =: X,
	Z is cputime - X,
	writeln(Z).
*/
