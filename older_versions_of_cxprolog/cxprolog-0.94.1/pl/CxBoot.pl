/*
 *   This file is part of the CxProlog system

 *   CxBoot.pl
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                                                %
  % This is a mere EXAMPLE of a CxProlog ALTERNATIVE BOOT FILE.    %
  % Deleting this file causes no harm to CxProlog.                 %
  %                                                                %
  % The contents of this file matches the defaults used by         %
  % CxProlog when no alternative boot file is provided.            %
  %                                                                %
  % An ALTERNATIVE BOOT FILE allows the user to do the following:  %
  % - Replacing the default top level user iteration;              %
  % - Adding extra builtin predicates written in Prolog: all       %
  %   predicates defined inside the boot file, or loaded from      %
  %   commands issued inside the boot file, are taken as builtin;  %
  % - Redefining predefined builtin predicates: it is necessary to %
  %   issue an abolish_builtin/2 command, prior to each such       %
  %   redefinition;                                                %
  % - Renaming predefined builtin predicates: this is done by      %
  %   issuing a rename_builtin/3 command;                          %
  %   issue an abolish_builtin/2 command, prior to each such       %
  %   redefinition;                                                %
  % - Defining MUTABLE builtin predicates: it is necessary to issue%
  %   an mutable_builtin/2 command, prior to each such definition. %
  %                                                                %
  % IMPORTANT NOTE: Is mandatory to define the predicates          %
  % '$cxprolog_initialise'/0 and '$cxprolog_top_level_goal'/0      %
  %  inside any ALTERNATIVE BOOT FILE.                             %
  %                                                                %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ACTIVATION

'$cxprolog_initialise':-
    version,
    '$env_context' := [main].

'$cxprolog_top_level_goal' :-
    '$env_context' =: C,
    call_on_context(C,'$top_level').    

% TOP LEVEL

'$top_level' :-
    ( flag(debug,1) -> write(user,'(debug) ')
    ; flag(debug,2) -> write(user,'(trace) ')
    ; true ),
    context(C), write(user,C),
    write(user,' ?- '),
    read(user,G), varnames(V),
    question(G,V).

% HANDLING CONTEXT

push U :-
    U>>true,    /* validate unit U */
    '$env_context' =: C,
    '$env_context' := [U|C],
    restart.
pop :-
    '$env_context' =: [_|C],
    '$env_context' := C,
    restart.
