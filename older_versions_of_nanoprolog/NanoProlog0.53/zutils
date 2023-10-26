
/*
skipeol :- skip( 10 ).

skipeol(10) :- !.
skipeol(_) :- skip( 10 ).

read_till_eol( ChList ) :-
    get0(user,Ch),
    read_till_eol( Ch, ChList ).

read_till_eol( 10, [10] ) :- !.
read_till_eol( Ch0, [Ch0|ChList] ) :-
    get0(user,Ch),
    read_till_eol( Ch, ChList ).
*/

system_predicate( A , P ):-system_predicate( P ) , P =.. [ A | _ ].

append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).

append_u([], L, L) :- !.
append_u([H|T1], L2, [H|T3]) :- append_u(T1, L2, T3).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

member_u(X, [X|_]) :- !.
member_u(X, [_|T]) :- member_u(X, T).

appendList( [], [] ) :- !.
appendList( [H|T], L ) :- append_u( H, T1, L ), appendList(T, T1).

% Append a list of atoms producing an atom
append_name(NameL, Name) :-
    '$append_name'(NameL, List),
    name(Name, List).
'$append_name'([], []) :- !.
'$append_name'([NameH|NameT], NameL) :-
    name(NameH, NameHL),
    append_u(NameHL, NameTL, NameL),
    '$append_name'(NameT, NameTL).

% reconsults but changes to the directory before doing so
sop_reconsult(File) :-
    chdir(PWD),
    (	splitPath(File, Dir, FileName),
	chdir(Dir),
	reconsult(FileName)
	    -> chdir(PWD)
    ;	chdir(PWD), fail
    ).

% Extracts the class name from a path
extractClassName( Path, Class ) :-
    splitPath( Path, _, Filename ),
    name( Filename, FilenameList ),
    extractClassName1( FilenameList, ClassList ),
    name( Class, ClassList ).
extractClassName1( [0'.|_], [] ) :- !.
extractClassName1( [H|T], [H|T1] ) :-
    extractClassName1( T, T1 ).    


% splitPath( +Path, -Directory, -FileName )
splitPath( Path, Dir, Filename ) :-
    name( Path, PathList ),
    splitPath1( PathList, DirList, FilenameList ), !,
    name( Dir, DirList ),
    name( Filename, FilenameList ).
splitPath( Path, '', Path ).	% When path is simple file name
splitPath1( [0'/|P], [0'/|D], F ) :- !,
    ( splitPath1( P, D, F ) ; F = P ).
splitPath1( [C|P], [C|D], F ) :-
    splitPath1( P, D, F ).

writeStr( Str ) :-
	name( A, Str ),
	write( A ).

% Devolve o comprimento de uma lista
length( [], 0 ) :- !.
length( [_|R], S ) :-
    length( R, SS ),
    S is SS + 1.

% Alguns predicados para ter umas estruturas de controle mais procedimentais

ifthen( Expr, Then ) :-
    (   call( Expr ) -> call( Then )
    ;   true
    ).

ifthenelse( Expr, Then, Else ) :-
    (   call( Expr ) -> call( Then )
    ;   call( Else )
    ).

switch( [] ).
switch( [(Case -> Stat)|T] ) :-
    (   call( Case ) -> call( Stat )
    ;   switch( T )
    ).

while( Expr, Stat ) :-
    call( Expr ), !,
    call( Stat ),
    while( Expr, Stat ).
while( _, _ ).

dowhile( Stat, Expr ) :-
    call( Stat ),
    while( Expr, Stat ).

format( Stream , String , Param ) :- format( String , Param ).
format( String , Param ):- name( String , L ), format2( L , Param ).

format2([],_).
format2( [ 126,110|T ] , P ):-!, nl , format2( T , P ). 
format2( [ 126,78|T ] , P ):-!, nl , format2( T , P ). 
format2( [ 126,_|T ] , [] ):-!, format2( T , [] ). 
format2( [ 126,R|T ] , [ H|T2 ] ):- !, f_write( R , H ), format2( T , T2 ).
format2( [ 126,R | T ] , Param ):- ! ,f_write( R , Param ), format2( T , [] ).
format2( [ 32|T ] , Param ):- ! , write( ' ' ) ,format2( T , Param ).
format2( [ H|T ] , Param ):- write( H ) ,format2( T , Param ).

f_write( L , P ):- member_u( L , [107,112,113,119] ) ,!, write( P ).
f_write( L , P ):- member_u( L , [114,100] ) ,number(P) ,!, write( P ).
f_write( 99 , P ):- number(P) ,!. 
f_write( 105 , P ):- !.
f_write( 97 , P ):- atom(P) ,!, write( P ).

ground( T ):- var( T ) , !, fail.
ground( T ):- atomic( T ) , !.
ground( T ):- T =..[ H | T2],!, groundl( T2 ).

groundl( [] ):- !.
groundl( [ H | T ] ):- ! , ground( H ), groundl( T ).
