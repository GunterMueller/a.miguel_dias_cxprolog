#!/usr/local/bin/cxprolog --script

% This file provides an example of the use of specific graphical predicates
% and general predicades working together.
% It creates several "buttons", with which the user can interact to. The 
% functions provided by these "buttons" are very simple, but they ilustrate 
% the generic predicades developed (predicades that are generic and don't 
% belong neither to the graphical predicades nor to the text editor 
% predicades).
%
% To try this example, consult this file in cxProlog and call the botoes 
% predicade.
%
% by Sergio Lopes
%

printFiles([]):-!.
printFiles([X|Xs]):-
	gui_alert(X),
	printFiles(Xs).

botao(X,Y,DX,DY,Text,TextX,TextY,Window):-
	gui_gfx_rectangle(Window,X,Y,DX,DY,255,255,0,_),
	X1 is X+1, Y1 is Y+1, DX1 is DX-2, DY1 is DY-2,
	gui_gfx_rectangle_filled(Window,X1,Y1,DX1,DY1,240,120,0,_), %fundo do botao
	gui_gfx_draw_text(Window,Text,TextX,TextY,_).

isExit(X,Y) :- X >= 150, Y >= 400, X =< 150+45, Y =< 400+40,!.

%ola
processMouse(X,Y,Window) :- X >= 50, Y >= 50, X =< 50+40, Y =< 50+40,!, 
				gui_gfx_rectangle(Window,50,50,40,40,255,255,0,_), 
				gui_gfx_rectangle(Window,51,51,38,38,255,255,0,_), 
				%efeito de carregar no botao
				gui_gfx_rectangle_filled(Window,52,52,36,36,0,120,0,_), 
				gui_gfx_draw_text(Window,'ola',60,60,_),
				gui_alert('Ola!!!!!!!!!!!!!'),
				botao(50,50,40,40,'ola',60,60,Window).
%escolher um ficheiro
processMouse(X,Y,Window) :- X >= 150, Y >= 50, X =< 150+150, Y =< 50+40,!, 
				gui_gfx_rectangle(Window,150,50,150,40,255,255,0,_), 
				gui_gfx_rectangle(Window,151,51,148,38,255,255,0,_), 
				gui_file_chooser_simple_msg('Escolha um ficheiro!',Fich),
				gui_alert(Fich),
				botao(150,50,150,40,'Escolher 1 ficheiro',160,60,Window).

%escolher varios ficheiros
processMouse(X,Y,Window) :- X >= 150, Y >= 120, X =< 150+200, Y =< 120+40,!,
				gui_gfx_rectangle(Window,150,120,200,40,255,255,0,_), 
				gui_gfx_rectangle(Window,151,121,198,38,255,255,0,_), 
				gui_file_chooser_multiple_msg('Seleccione um grupo de ficheiros!',Fichs),
				printFiles(Fichs),
				botao(150,120,200,40,'Escolher varios ficheiros',160,130,Window).

%escolher uma directoria
processMouse(X,Y,Window) :- X >= 150, Y >= 190, X =< 150+150, Y =< 190+40,!,
				gui_gfx_rectangle(Window,150,190,150,40,255,255,0,_),
				gui_gfx_rectangle(Window,151,191,148,38,255,255,0,_), 
				gui_directory_chooser_msg('Seleccione uma directoria!',Dir),
				gui_alert(Dir),
				botao(150,190,150,40,'Escolher 1 directoria',160,200,Window).

treatResult(ok) :- !,
	gui_alert('Escolheu OK!').
treatResult(cancel) :- !,
	gui_alert('Escolheu CANCEL!').
treatResult(yes) :- !,
	gui_alert('Escolheu YES!').
treatResult(no) :- !,
	gui_alert('Escolheu NO!').

%yes no
processMouse(X,Y,Window) :- 
	X >= 380, Y >= 50, X =< 380+80, Y =< 50+40,!,
	gui_gfx_rectangle(Window,380,50,80,40,255,255,0,_),
	gui_gfx_rectangle(Window,381,51,78,38,255,255,0,_),
	gui_choice('Escolha Yes ou No',R),
	treatResult(R),
	botao(380,50,80,40,'Yes No',390,60,Window).

%yes no cancel
processMouse(X,Y,Window) :- 
	X >= 380, Y >= 120, X =< 380+110, Y =< 120+40,!,
	gui_gfx_rectangle(Window,380,120,110,40,255,255,0,_),
	gui_gfx_rectangle(Window,381,121,108,38,255,255,0,_),
	gui_choice_yes_no_cancel('Escolha Yes ou No ou Cancel',R),
	treatResult(R),
	botao(380,120,110,40,'Yes No Cancel',390,130,Window).

%ok cancel
processMouse(X,Y,Window) :- 
	X >= 380, Y >= 190, X =< 380+80, Y =< 190+40,!,
	gui_gfx_rectangle(Window,380,190,80,40,255,255,0,_),
	gui_gfx_rectangle(Window,381,191,78,38,255,255,0,_),
	gui_choice_ok_cancel('Escolha OK ou Cancel',R),
	treatResult(R),
	botao(380,190,80,40,'Ok Cancel',390,200,Window).


%inserir texto
processMouse(X,Y,Window) :- 
	X >= 150, Y >= 300, X =< 150+110, Y =< 300+40,!,
	gui_gfx_rectangle(Window,150,300,110,40,255,255,0,_),
	gui_gfx_rectangle(Window,151,301,108,38,255,255,0,_),
	gui_get_text('Insira texto!',Text),
	gui_alert(Text),
	botao(150,300,110,40,'Inserir Texto',160,310,Window).


processMouse(_,_,_) :- !.


/********* EVENTOS **************************/
process(event(_,close_window),0,_) :- !.
process(event(Window,mouse_down(X,Y)),0,Window) :- 
	isExit(X,Y),!.
process(event(Window,mouse_down(X,Y)),1,Window) :- !, 
	processMouse(X,Y,Window).
process(_,1,Window) :- !.


/* Desenhar botoes */
botoes(Window) :-
	botao(50,50,40,40,'ola',60,60,Window),
	gui_gfx_rectangle(Window,145,45,210,190,0,255,0,_),
	botao(150,50,150,40,'Escolher 1 ficheiro',160,60,Window),
	botao(150,120,200,40,'Escolher varios ficheiros',160,130,Window),
	botao(150,190,150,40,'Escolher 1 directoria',160,200,Window),
	botao(150,400,45,40,'Sair',160,410,Window),
	gui_gfx_rectangle(Window,370,45,125,190,0,255,0,_),
	botao(380,50,80,40,'Yes No',390,60,Window),
	botao(380,120,110,40,'Yes No Cancel',390,130,Window),
	botao(380,190,80,40,'Ok Cancel',390,200,Window),
	botao(150,300,110,40,'Inserir Texto',160,310,Window).


/******** CICLO ****************************/
cycle(Window) :- repeat,
		gui_event_get(X),
		process(X,Exit,Window),
		Exit = 0,!,
		gui_gfx_close(Window).
		


botoes :- gui_gfx_create('Botoes',10,10,520,500,Window), botoes(Window), cycle(Window).

:- botoes, exit_script.

