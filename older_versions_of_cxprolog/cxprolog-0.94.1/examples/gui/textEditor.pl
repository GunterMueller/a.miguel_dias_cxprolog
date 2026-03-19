% This file provides an example of the use of specific text-editor 
% predicates.
%
% This example provides an example of a simple text editor with user-defined 
% menus. The text-editor is very simple as it contains the very basic 
% functionalities of a text editor. However, it ilustrates what is possible 
% to do with the developed predicates. The interface is very simple and the 
% right mouse button menu allows copying and pasting selected text in the 
% editor.
%
% To try this example, consult this file in cxProlog and call the textEditor 
% predicade.
%
% by Sergio Lopes
%

/********* Process Events *********************************/

processEvent(Window,event(Window,close_window)) :- 
     gui_text_close(Window).

processEvent(Window,event(Window,usermenu(open))) :- 
     gui_text_open_file(Window),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(save))) :- 
     gui_text_save_file(Window),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(save_as))) :- 
     gui_text_save_file_as(Window),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(quit))) :- 
     gui_text_close(Window).

processEvent(Window,event(Window,usermenu(ola))) :- 
     gui_alert(oooooooooooooooollllllllllaaaaaaaaaaa),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(text020))) :- 
     gui_text_get_text(Window,0,20,X),
     gui_alert(X),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(text2040))) :- 
     gui_text_get_text(Window,20,40,X),
     gui_alert(X),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(append))) :- 
     gui_get_text('Insira o texto a ser inserido no fim do texto',X),
     gui_text_append(Window,X),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(selected))) :- 
     gui_text_get_selected_text(Window,X),
     gui_alert(X),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(replace02))) :- 
     gui_get_text('Insira o texto a ser inserido',X),
     gui_text_replace(Window,0,2,X),
     gui_alert('texto subsituido entre 0 e 2!'),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(getpath))) :- 
     gui_text_get_file_path(Window,Path),
     gui_alert(Path),
     runEditor(Window).

processEvent(Window,event(Window,usermenu(setselection))) :- 
     gui_text_set_selection(Window,10,20),
     runEditor(Window).

%not an even from these windows...
processEvent(Window,event(W,_)) :- !, writeln([Window,W]),runEditor(Window). 


/********* Running the Editor ****************************/

runEditor(Window) :- gui_event_get(Z), processEvent(Window,Z).

textEditor :- 
    gui_text_create(
          'My Text Editor',
          10,10,200,200,
          [['&File',open,'|',save,'save_as','|',quit],['&Menu',ola,text020,
          	text2040,'|',append,selected,replace02,setselection,getpath]],
	    Window),
    runEditor(Window).

:- textEditor, exit.
