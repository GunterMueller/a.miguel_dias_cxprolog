#!/usr/local/bin/cxprolog --script

%
% by Henrique Oliveira
%

notifyUser('Option 1') :- gui_alert('You chose Option 1').
notifyUser('Option 2') :- gui_alert('You chose Option 2').
notifyUser('Option 3') :- gui_alert('You chose Option 3').
notifyUser('Option 4') :- gui_alert('You chose Option 4').

% EVENT TREATMENT
% Event from another window
process(event(OtherWindow,_),1,Window,_,_,_) :-
	Window \= OtherWindow,!.

% Exit Button was pressed
process(event(Window,button(ExitButton)),0,Window,_,_,ExitButton).

% Ok Button was pressed
process(event(Window,button(OkButton)),1,Window,List,OkButton,_) :-
	gui_gfx_list_getitem(List,Item),
	notifyUser(Item).

process(event(Window,close_window),0,Window,_,_,_).
/*
process(event(Window,mouse_down(_,_)),1,Window,List,OkButton,_) :-
	gui_gfx_list_getitem(List,Item),
	notifyUser(Item).
*/
% MAIN CYCLE
cycle(Window, List, OkButton, ExitButton) :-
	repeat,
	gui_event_get(Event),
	process(Event, Exit, Window, List, OkButton, ExitButton),
	Exit = 0,!,
	gui_gfx_close(Window).

% WINDOW AND GRAPHICAL OBJECTS CREATION
draw_window(Window, List, OkButton, ExitButton) :-
	gui_gfx_create('Test List', 10, 10, 300, 300, Window),
	gui_gfx_list(Window, ['Option 1', 'Option 2', 'Option 3', 'Option 4'], 10, 10, List),
	gui_gfx_button(Window, 'OK', 100, 200, OkButton),
	gui_gfx_button(Window, 'Exit', 200, 200, ExitButton).
	
main :- 
	draw_window(Window, List, OkButton, ExitButton), 
	cycle(Window, List, OkButton, ExitButton).

:- main, exit_script.
