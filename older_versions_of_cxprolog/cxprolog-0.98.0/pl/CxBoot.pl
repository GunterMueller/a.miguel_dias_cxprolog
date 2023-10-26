/*
 *   This file is part of the CxProlog system

 *   CxBoot.pl
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2015 A.Miguel Dias, CITI, DI/FCT/UNL

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
  % - Replace the default top level user iteration;                %
  % - Add extra builtin predicates written in Prolog: all          %
  %   predicates defined inside the boot file, or loaded from      %
  %   commands issued inside the boot file, are taken as builtin;  %
  % - Redefine predefined builtin predicates: it is necessary to   %
  %   issue an abolish_builtin/2 command, prior to each such       %
  %   redefinition;                                                %
  % - Rename predefined builtin predicates: this is done by        %
  %   issuing a rename_builtin/3 command;                          %
  %   issue an abolish_builtin/2 command, prior to each such       %
  %   redefinition;                                                %
  % - Define MUTABLE builtin predicates: it is necessary to issue  %
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
  P0 = '\n',
  ( current_prolog_flag(debug, debug)
			-> atom_concat(P0,'(debug) ',P1)
  ; current_prolog_flag(debug, trace)
			-> atom_concat(P0,'(trace) ',P1)
  ; P1=P0 ),
	context(C), atom_term(A, C),
	atom_concat(P1, A, P2),
	atom_concat(P2, ' ?- ', P3),
	'$$_top_read'(P3, '| ', G), varnames(V),
	try (G==end_of_file -> writeln(user,''), halt),
	abolish('$$_top_call'/1),
	'$$_top_assert'(('$$_top_call'(V):-G)),
	question('$$_top_call'(V),V).


% HANDLING CONTEXT

push U :-
	'$$_ctx_ext'(U,true),	/* validate unit U */
	'$env_context' =: C,
	'$env_context' := [U|C],
	abort.
pop :-
	'$env_context' =: [_|C],
	'$env_context' := C,
	abort.

pwd :- fs_cwd(X), writeqln(X).
