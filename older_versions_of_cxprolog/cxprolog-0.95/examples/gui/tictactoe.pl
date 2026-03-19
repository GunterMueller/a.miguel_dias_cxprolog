#!/usr/local/bin/cxprolog --script

% This file provides an example of the use of specific graphical predicates 
% and some text boxes as well as dialog messages.
%
% This is an implementation of the tic-tac-toe game for two human players 
% (sorry, no AI). Each player is allowed to play in turns. After one of the 
% player wins or when there is a draw, there is the possibility of starting
% a new game.
%
% To try this example, consult this file in cxProlog and call the 
% jogo_do_galo predicade.
%
% by Sergio Lopes
%

member([X|Xs],X).
member([X|Xs],Y) :- member(Xs,Y).

line(X0,Y0,X1,Y1,W) :- gui_gfx_line(W,X0,Y0,X1,Y1,100,0,0,_).

/*galo*/
galo_white(X,Y,W) :- gui_gfx_rectangle_filled(W,X,Y,100,100,255,255,255,_).
galo_black(X,Y,W) :- gui_gfx_rectangle_filled(W,X,Y,100,100,0,0,0,_).

galo_board(X,Y,W) :- X1 is X+100, X2 is X+200, Y1 is Y+100, Y2 is Y+200,
	galo_white(X,Y,W), galo_black(X1,Y,W), galo_white(X2,Y,W),
	galo_black(X,120,W), galo_white(X1,Y1,W), galo_black(X2,Y1,W),
	galo_white(X,220,W), galo_black(X1,Y2,W), galo_white(X2,Y2,W).
	
cross(X,Y,W) :- 
	X0 is X + 25, 
	Y0 is Y + 25, 
	X1 is X0+50, 
	Y1 is Y0 + 50,
	gui_gfx_line(W,X0,Y0,X1,Y1,100,100,0,_), 
	gui_gfx_line(W,X1,Y0,X0,Y1,100,100,0,_).
circle(X,Y,R,W) :- 
	X1 is X+50, 
	Y1 is Y+50,
	gui_gfx_circle(W,X1,Y1,R,255,100,0,_).

draw_value(0,0,W) :- !,
	cross(20,20,W),
	cross(21,20,W),
	cross(22,20,W).
draw_value(0,1,W) :- !,
	cross(120,20,W),
	cross(121,20,W),
	cross(122,20,W).
draw_value(0,2,W) :- !,
	cross(220,20,W),
	cross(221,20,W),
	cross(222,20,W).
draw_value(0,3,W) :- !,
	cross(20,120,W),
	cross(21,120,W),
	cross(22,120,W).
draw_value(0,4,W) :- !,
	cross(120,120,W),
	cross(121,120,W),
	cross(122,120,W).
draw_value(0,5,W) :- !,
	cross(220,120,W),
	cross(221,120,W),
	cross(222,120,W).
draw_value(0,6,W) :- !,
	cross(20,220,W),
	cross(21,220,W),
	cross(22,220,W).
draw_value(0,7,W) :- !,
	cross(120,220,W),
	cross(121,220,W),
	cross(122,220,W).
draw_value(0,8,W) :- !,
	cross(220,220,W),
	cross(221,220,W),
	cross(222,220,W).

draw_value(1,0,W) :- !,
	circle(20,20,23,W), 
	circle(20,20,24,W), 
	circle(20,20,25,W).
draw_value(1,1,W) :- !,
	circle(120,20,23,W), 
	circle(120,20,24,W), 
	circle(120,20,25,W).
draw_value(1,2,W) :- !,
	circle(220,20,23,W), 
	circle(220,20,24,W),
	circle(220,20,25,W).
draw_value(1,3,W) :- !,
	circle(20,120,23,W),
	circle(20,120,24,W),
	circle(20,120,25,W).
draw_value(1,4,W) :- !,
	circle(120,120,23,W),
	circle(120,120,24,W), 
	circle(120,120,25,W).
draw_value(1,5,W) :- !,
	circle(220,120,23,W),
	circle(220,120,24,W), 
	circle(220,120,25,W).
draw_value(1,6,W) :- !,
	circle(20,220,23,W),
	circle(20,220,24,W),
	circle(20,220,25,W).
draw_value(1,7,W) :- !,
	circle(120,220,23,W), 
	circle(120,220,24,W), 
	circle(120,220,25,W).
draw_value(1,8,W) :- !,
	circle(220,220,23,W), 
	circle(220,220,24,W), 
	circle(220,220,25,W).

draw_matrix(Matrix,Window):- draw_matrix_tmp(Matrix, 0,Window).

draw_matrix_tmp([-1|Xs], N,Window):-!,
	N1 is N+1,
	draw_matrix_tmp(Xs,N1,Window).
draw_matrix_tmp([V|Xs], N,Window):-!,
	draw_value(V,N,Window),
	N1 is N+1,
	draw_matrix_tmp(Xs,N1,Window).
draw_matrix_tmp(_, 9,Window):-!.

invert(1,0).
invert(0,1).

scale(X,0) :- X =< 100,!.
scale(X,1) :- X =< 200,!.
scale(X,2) :- X =< 300,!.
scale(X,Y,XOut,YOut):- scale(X,XOut),scale(Y,YOut).


position_from_pixels(PLX,PLY,X,Y):- 
	X0 is PLX-20, 
	Y0 is PLY-20, 
	scale(X0,Y0,X,Y).
	
treat_matrix(X,Y,Matrix,Turn,Window) :-
	Pos is 3*Y+X,
	treat_matrix_aux(Matrix,Pos,Turn,Matrix,Pos,Window).

copy_list([],[]).
copy_list([X|Xs],[X|Ys]) :- copy_list(Xs,Ys).

write_list([],_,[],_).
write_list([X|Xs],0,K,V) :- !, copy_list([V|Xs],K).
write_list([X|Xs],N,[X|K],V) :- N2 is N-1, write_list(Xs,N2,K,V).

treat_matrix_aux([-1|Xs],0,Turn,Matrix,I,Window) :- !, 
	NT is Turn mod 2, 
	write_list(Matrix,I,NewMatrix,NT),
	analyze_matrix(NewMatrix,Turn,Window).
treat_matrix_aux([_|Xs],0,Turn,Matrix,I,Window) :- !, 
	gui_alert('Movimento inválido!'), 
	galo_cycle(Matrix,Turn,Window).
treat_matrix_aux([X|Xs],N,Turn,Matrix,I,Window) :-
	NN is N-1,
	treat_matrix_aux(Xs,NN,Turn,Matrix,I,Window).

analyze_matrix(Matrix,Turn,Window):-
	winner(Matrix,N),!,
	announce_winner(Matrix,N,Window).
analyze_matrix(Matrix,Turn,Window):-!,
	NT is Turn + 1,
	draw_matrix(Matrix,Window), 
	galo_cycle(Matrix,NT,Window).

announce_winner(Matrix,0,Window) :- !, 
	draw_matrix(Matrix,Window), 
	gui_alert('Ganharam as cruzes!'), 
	gui_choice('quer jogar de novo?',X),
	new_game(X,Window).
announce_winner(Matrix,1,Window) :- !, 
	draw_matrix(Matrix,Window), 
	gui_alert('Ganharam os circulos!'), 
	gui_choice('quer jogar de novo?',X),
	new_game(X,Window).

new_game(yes,Window) :- !, galo(Window).
new_game(_,Window) :- !, gui_gfx_close(Window).

check_lines(Matrix,N) :- winner_first_line(Matrix,N), N =\= -1,!.
check_lines(Matrix,N) :- winner_second_line(Matrix,N), N =\= -1,!.
check_lines(Matrix,N) :- winner_third_line(Matrix,N), N =\= -1,!.

winner_first_line([N,N,N|_],N).
winner_second_line([_,_,_,N,N,N|_],N).
winner_third_line([_,_,_,_,_,_,N,N,N|_],N).

check_columns(Matrix,N) :- winner_first_column(Matrix,N), N =\= -1,!.
check_columns(Matrix,N) :- winner_second_column(Matrix,N), N =\= -1,!.
check_columns(Matrix,N) :- winner_third_column(Matrix,N), N =\= -1,!.

winner_first_column([N,_,_,N,_,_,N|_],N).
winner_second_column([_,N,_,_,N,_,_,N|_],N).
winner_third_column([_,_,N,_,_,N,_,_,N|_],N).

check_diagonals(Matrix,N) :- winner_first_diagonal(Matrix,N), N =\= -1,!.
check_diagonals(Matrix,N) :- winner_second_diagonal(Matrix,N), N =\= -1,!.

winner_first_diagonal([N,_,_,_,N,_,_,_,N|_],N).
winner_second_diagonal([_,_,N,_,N,_,N|_],N).


winner(Matrix,N) :- check_lines(Matrix,N),!.
winner(Matrix,N) :- check_columns(Matrix,N),!.
winner(Matrix,N) :- check_diagonals(Matrix,N),!.


/* 	EVENTOS */

process(event(Window,mouse_down(X,Y)),Matrix,Turn,Window):- /*movimento válido*/
	X >= 20, Y >= 20, X =< 320, Y =< 320,!,
	/*calcular posicao*/
	position_from_pixels(X,Y,POSX,POSY),
	/*afectar matriz*/
	treat_matrix(POSX,POSY,Matrix,Turn,Window).

process(event(Window,close_window),Matrix,Turn,Window) :- !,
	gui_gfx_close(Window).

/*movimento inválido*/
process(event(Window,mouse_down(X,Y)),Matrix,Turn,Window):- 
	gui_alert('movimento fora da zona de jogo!'), 
	galo_cycle(Matrix,Turn,Window).


/*eventos de outras janelas*/
process(_,Matrix,Turn,Window) :- !,
	galo_cycle(Matrix,Turn,Window). 


galo_cycle(_,Turn,Window) :- 
	Turn >= 9, 
	gui_choice('Já não existem movimentos válidos. Quer jogar de novo?',X), 
	new_game(X,Window).

galo_cycle(Matrix,Turn,Window) :- 
	repeat, 
	gui_event_get(Z), 
	process(Z,Matrix,Turn,Window).


galo(Window) :- 
	galo_board(20,20,Window),
	galo_cycle([-1,-1,-1,-1,-1,-1,-1,-1,-1],0,Window).

jogo_do_galo :- 
	gui_gfx_create('Jogo do Galo',10,10,500,500,Window), 
	galo(Window).

:- jogo_do_galo, exit.
