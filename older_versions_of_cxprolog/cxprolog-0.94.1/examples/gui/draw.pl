%
% by Henrique Oliveira
%

getPosition(Window,X,Y) :-
	repeat,
	gui_event_get(Event),
	Event = event(Window,mouse_down(X,Y)),!.

getColor('Green',0,255,0).
getColor('Blue',0,0,255).
getColor('Red',255,0,0).


distance(X1,Y1,X2,Y2,Dist) :-
	A1 is (X2 - X1) ^ 2,
	A2 is (Y2 - Y1) ^ 2,
	Aux is sqrt(A1 + A2),
	Dist is round(Aux).
	
drawFigure(Window,'Pixel',Fill,Color) :- 
	gui_alert('Please click on the point.'),
	getPosition(Window,X,Y),
	getColor(Color,R,G,B),
	gui_gfx_putpixel(Window,X,Y,R,G,B,_).
	
drawFigure(Window,'Line',Fill,Color) :- 
	gui_alert('Please click on the first point.'),
	getPosition(Window,X0,Y0),
	gui_alert('Please click on the second point.'),
	getPosition(Window,X1,Y1),
	getColor(Color,R,G,B),
	gui_gfx_line(Window,X0,Y0,X1,Y1,R,G,B,_).

drawFigure(Window,'Circle',Fill,Color) :- 
	gui_alert('Please click on the center.'),
	getPosition(Window,Xc,Yc),
	gui_alert('Please click on the border.'),
	getPosition(Window,Xr,Yr),
	distance(Xc,Yc,Xr,Yr,Radius),
	getColor(Color,R,G,B),
	(Fill = 'Filled', gui_gfx_circle_filled(Window,Xc,Yc,Radius,R,G,B,_);
	 Fill = 'Not Filled',gui_gfx_circle(Window,Xc,Yc,Radius,R,G,B,_)).

drawFigure(Window,'Rectangle',Fill,Color) :- 
	gui_alert('Please click on the top left corner.'),
	getPosition(Window,X0,Y0),
	gui_alert('Please click on the bottom right corner.'),
	getPosition(Window,Xr,Yr),
	Width is Xr - X0,
	Height is Yr - Y0,
	getColor(Color,R,G,B),
	(Fill = 'Filled', gui_gfx_rectangle_filled(Window,X0,Y0,Width,Height,R,G,B,_);
	 Fill = 'Not Filled',gui_gfx_rectangle(Window,X0,Y0,Width,Height,R,G,B,_)).

% EVENT TREATMENT
% Event from another window
process(event(OtherWindow,_),1,Window,_,_,_,_,_,_) :-
	Window \= OtherWindow,!.

% Ok Button was pressed
process(event(Window,button(DrawButton)),1,Window,FigureList,FillList,ColorList,DrawButton,_,_) :-
	gui_gfx_list_getitem(FigureList,Figure),
	gui_gfx_list_getitem(FillList,Fill),
	gui_gfx_list_getitem(ColorList,Color),
	drawFigure(Window,Figure,Fill,Color).
	
% Help Button was pressed
process(event(Window,button(HelpButton)),1,Window,_,_,_,_,HelpButton,_) :-
	gui_alert(
'Choose a figure, press the Draw button and then follow the 
instructions. You may also choose a color for the figure, 
and if you want it to be filled or not. This last option 
applies only to circles and rectangles.').

% Exit Button was pressed
process(event(Window,button(ExitButton)),0,Window,_,_,_,_,_,ExitButton).

% Close window event
process(event(Window,close_window),0,Window,_,_,_,_,_,_).

% MAIN CYCLE
cycle(Window, FigureList, FillList, ColorList, DrawButton, HelpButton, ExitButton) :-
	repeat,
	gui_event_get(Event),
	process(Event, Exit, Window, FigureList, FillList, ColorList, DrawButton, HelpButton, ExitButton),
	Exit = 0,!,
	gui_gfx_close(Window).

% WINDOW AND GRAPHICAL OBJECTS CREATION
draw_window(Window, FigureList, FillList, ColorList, DrawButton, HelpButton, ExitButton) :-
	gui_gfx_create('Draw', 10, 10, 600, 600, Window),
	gui_gfx_list(Window, ['Pixel','Line','Circle', 'Rectangle'], 10, 10, FigureList),
	gui_gfx_list(Window, ['Filled', 'Not Filled'], 80, 10, FillList),
	gui_gfx_list(Window, ['Green', 'Blue', 'Red'], 150, 10, ColorList),
	gui_gfx_button(Window, 'Draw', 10, 100, DrawButton),
	gui_gfx_button(Window, 'Help', 10, 125, HelpButton),
	gui_gfx_button(Window, 'Exit', 10, 150, ExitButton).
	
main :- 
	draw_window(Window, FigureList, FillList, ColorList, DrawButton, HelpButton, ExitButton), 
	cycle(Window, FigureList, FillList, ColorList, DrawButton, HelpButton, ExitButton).

:- main, exit.
