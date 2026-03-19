#!/usr/local/bin/cxprolog --script

% This file provides an example of the use of specific graphical predicates.
%
% This example provides a graphical ilustration of all the possible solutions
% to the n-queens problem. All the solutions are generated in back-tracking.
% Some solutions appear to be repeated, but they're not. Two apparently 
% equal solutions differ in the order of the queens (all the queens have the
% same drawing, so the solutions appear to be equal).
%
% To try this example, consult this file in cxProlog and call the nqueens(N) 
% predicade where N is the number of queens.
%
% by Sergio Lopes
%

pair(X) :- Z is (X mod 2),!, Z == 0.

member([X|Xs],X).
member([X|Xs],Y) :- member(Xs,Y).

listlength([],0) :-!.
listlength([X|Xs],N) :- listlength(Xs,N1), N is N1 + 1.

blackSquare(X,Y,Window) :- 
	gui_gfx_rectangle_filled(Window,X,Y,45,45,0,0,0,_).
whiteSquare(X,Y,Window) :- 
	gui_gfx_rectangle_filled(Window,X,Y,45,45,200,200,200,_).


drawQueen(CornerX, CornerY,Window) :-
	X0 is CornerX+8, Y0 is CornerY+7,
	gui_gfx_circle_filled(Window,X0,Y0,3,255,255,255,_),
	X1 is CornerX+15, Y1 is CornerY+5,
	gui_gfx_circle_filled(Window,X1,Y1,3,255,255,255,_),
	X2 is CornerX+22, Y2 is CornerY+3,
	gui_gfx_circle_filled(Window,X2,Y2,3,255,255,255,_),
	X3 is CornerX+29, Y3 is CornerY+5,
	gui_gfx_circle_filled(Window,X3,Y3,3,255,255,255,_),
	X4 is CornerX+36, Y4 is CornerY+7,
	gui_gfx_circle_filled(Window,X4,Y4,3,255,255,255,_),
	X5 is CornerX+22, Y5 is CornerY+30,
	gui_gfx_circle_filled(Window,X5,Y5,15,255,255,255,_),
	gui_gfx_line(Window,X0,Y0,X5,Y5,255,255,255,_),
	gui_gfx_line(Window,X1,Y1,X5,Y5,255,255,255,_),
	gui_gfx_line(Window,X2,Y2,X5,Y5,255,255,255,_),
	gui_gfx_line(Window,X3,Y3,X5,Y5,255,255,255,_),
	gui_gfx_line(Window,X4,Y4,X5,Y5,255,255,255,_).

blackLine(0,X,Y,Window) :- !.
blackLine(1,X,Y,Window) :- !,
	blackSquare(X,Y,Window).
blackLine(2,X,Y,Window) :- !,
	blackSquare(X,Y,Window),
	X1 is X+45,
	whiteSquare(X1,Y,Window).
blackLine(N,X,Y,Window) :- 
	blackSquare(X,Y,Window),
	X1 is X+45,
	whiteSquare(X1,Y,Window),
	X2 is X1+45,
	N2 is N-2,
	blackLine(N2,X2,Y,Window),!.

whiteLine(0,X,Y,Window) :- !.
whiteLine(1,X,Y,Window) :- !,
	whiteSquare(X,Y,Window).
whiteLine(2,X,Y,Window) :- !,
	whiteSquare(X,Y,Window),
	X1 is X+45,
	blackSquare(X1,Y,Window).
whiteLine(N,X,Y,Window) :- 
	whiteSquare(X,Y,Window),
	X1 is X+45,
	blackSquare(X1,Y,Window),
	X2 is X1+45,
	N2 is N-2,
	whiteLine(N2,X2,Y,Window),!.

drawBoard(0,_,_,_,_) :- !.
drawBoard(1,X,Y,N,Window) :- !,
	whiteLine(N,X,Y,Window).
drawBoard(Lines,X,Y,N,Window) :-
	pair(N),!,
      blackLine(N,X,Y,Window),
	Y1 is Y - 45,
      whiteLine(N,X,Y1,Window),
	Y2 is Y1-45,
	L1 is Lines-2,
      drawBoard(L1,X,Y2,N,Window).
drawBoard(Lines,X,Y,N,Window) :- !,
	whiteLine(N,X,Y,Window),
	Y1 is Y - 45,
	blackLine(N,X,Y1,Window),
	Y2 is Y1-45,
	L1 is Lines-2,
	drawBoard(L1,X,Y2,N,Window).

drawBoard(N,Window) :- 
	Y is 45*N-25,
	drawBoard(N,20,Y,N,Window).

calculateCornerPos(X,Y,N,XOut,YOut) :- !,
	XOut is 45*X-25, 
	YOut is 45*Y-25.


drawQueens([],_,_) :-!.
drawQueens([queen(X,Y)|Xs],N,Window) :-!, 
	calculateCornerPos(X,Y,N,X1,Y1),
	drawQueen(X1,Y1,Window),
	drawQueens(Xs,N,Window).


drawSolution(N,Q,Window) :- !,
	drawBoard(N,Window), 
	drawQueens(Q,N,Window).



isinside(X,0) :- !,fail.
isinside(X,X).
isinside(X,N) :- 
	N1 is N - 1, 
	isinside(X,N1).

buildMatrix(N,L) :-
	findall(square(X,Y),
	(isinside(X,N),isinside(Y,N)),L),!.

onsameDiagonal(square(0,_), square(_,_)):- !,fail.
onsameDiagonal(square(_,_), square(0,_)):- !,fail.

onsameDiagonal(square(X,Y), square(N,L)):- M1 is (X-N), M2 is (Y-L),M1 == M2,!.
onsameDiagonal(square(X,Y), square(N,L)):- M1 is (N-X), M2 is (Y-L),M1 == M2,!.


cutsquares(square(X,Y),[],[]) :-!.
cutsquares(square(X,Y),[square(X,_)|Xs],Zs) :-!, 
	cutsquares(square(X,Y),Xs,Zs).
cutsquares(square(X,Y),[square(_,Y)|Xs],Zs) :-!, 
	cutsquares(square(X,Y),Xs,Zs).
cutsquares(square(X,Y),[square(K,L)|Xs],Zs) :- 
	onsameDiagonal(square(X,Y), square(K,L)),!, 
	cutsquares(square(X,Y),Xs,Zs).
cutsquares(square(X,Y),[square(K,L)|Xs],[square(K,L)|Zs]) :-!, 
	cutsquares(square(X,Y),Xs,Zs).

processV(yes,_) :- !, fail.
processV(no,Window) :- !, gui_gfx_close(Window).

getSolution(N,[],Q,Q,Window) :- 
	listlength(Q,N),
	drawSolution(N,Q,Window), 
	gui_choice('Deseja continuar?',Z),
	processV(Z,Window). %solucao
getSolution(_,[],_,_,_) :- !, fail.
getSolution(N,L,Q,Queens,Window) :- 
	listlength(L,LL),
	listlength(Q,QL),
	LL + QL < N, 
	fail.
getSolution(N,L,Q,Queens,Window) :- 
	member(L,square(X,Y)), 
	cutsquares(square(X,Y),L,L2), 
	getSolution(N,L2,[queen(X,Y)|Q],Queens,Window).


queens(N,L,Window) :- 
	getSolution(N,L,[],Queens,Window),!.
queens(_,_,Window) :- !,
	gui_alert('Prima OK para sair!'),
	gui_gfx_close(Window).

nqueens(N) :- N < 4,!,
	gui_alert('Não existem soluções para o problama das N-Rainhas com N < 4!').
nqueens(N) :- !,
	buildMatrix(N,L),
	gui_gfx_create('Soluções para o problema das N-Rainhas',10,10,500,500,Window),
	queens(N,L,Window).

:- nqueens(7), exit_script_fast.
