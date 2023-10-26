#!/usr/local/bin/cxprolog --script

% This file provides a very simplistic example of how to create two
% distinct text windows. The windows are independent and is is up to the 
% programmer to control and maintain them.
%
% To try this example, consult this file in cxProlog and call the janelas 
% predicade.
%
% by Sergio Lopes
%

janelas :- gui_text_create(olaJanela1,10,10,250,500,[],Text1),
	     gui_text_create(olaJanela2,300,10,550,500,[],Text2),
		writeln([Text1,Text2]),
		gui_text_append(Text1,'janela 1'),
		gui_text_append(Text2,'janela 2'),
            writeln([Text1,Text2]),
		gui_alert('Carregue em OK para destruir as janelas'),
		gui_text_close(Text1),gui_text_close(Text2).

:- janelas, exit_script_fast.
