/*
 *   This file is part of the CxProlog system

 *   CxBoot.pl
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                                               %
  % This is an EXAMPLE of a CxProlog ALTERNATIVE BOOT FILE. The   %
  % contents of this file matches the defaults used by CxProlog   %
  % when no such boot file is provided. Deleting this file would  %
  % cause no harm to the CxProlog system.                         %
  %                                                               %
  % An ALTERNATIVE BOOT FILE allows you to:                       %                                                              %
  %  - Specify the top level iteraction;                          %                                                      %
  %  - Add extra builtin predicates;                              %
  %  - Redefine CxProlog predefined builtin predicates (you       %
  %    must issue an abolish_builtin/2 command prior to each such %
  %    redefinition).                                             %
  %                                                               %
  % Defining the predicates '$cxprolog_startup'/0 and             %
  % '$cxprolog_restart' is mandatory in every alternative boot    %
  % file.                                                         %
  %                                                               %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



  
% ACTIVATION	

% Normal startup
'$cxprolog_startup':-	
	version,
	flag(fail_on_error,_,off),
	'$env_context' := [main],
	restart.

% Restart after error, restart/0 call or CNTL-C interrupt
'$cxprolog_restart' :-	
	'$env_context' =: C,
	call_on_context(C,'$top_level').

% HANDLING CONTEXT 	

push U :-	
	U>>true,	/* validate unit U */
	'$env_context' =: C,
	'$env_context' := [U|C],
	restart.
pop :-		
	'$env_context' =: [_|C],
	'$env_context' := C,
	restart.

% TOP LEVEL

'$top_level' :-
	repeat,
		(flag(trace,on) ->
			write(user,'(trace) ') ; true),
		context(C), write(user,C),
		write(user,' ?- '),
		read(user,G), varnames(V),
		question(G,V),
	fail.

trace :- flag(trace,_,on).
notrace :- flag(trace,_,off).
