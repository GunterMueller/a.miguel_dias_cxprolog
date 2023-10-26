/*
 *   This file is part of the CxProlog system

 *   Debug.c
 *   by A.Miguel Dias - 2000/05/05
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

#include "CxProlog.h"
#include <signal.h>

/*
[user] ?- a.
   (  1)  0 call: [user]: a ? ?

 Debugging options:
  <cr> creep     r    retry       w    write          +  spy this
   c   creep     r<i> retry i     d    display        -  nospy this
   l   leap      f    fail        g    ancestors      a  abort
   s   skip      f<i> fail i      g<n> n ancestors    e  exit
   n   nodebug   =    debugging   :    statistics     ?h help
   b   break     <<n> set depth   2    twolevel ctx   !@ command

   (  1)  0 call: [user]: a ? :

 Statistics:
      Code Area: 277K ( 68828 bytes used + 64 recoverable bytes)
         Stacks: 155K (  1844 bytes used (1740 local + 104 global))
          Trail:  18K (     8 bytes used)
        Scratch:  32K (     0 bytes used)
        Runtime:  23.3167s.

   (  1)  0 call: [user]: a ?
   (  2)  1 call: [user]: b ? 
*/

static Bool InterDebug()
{
	int comm, arg ;

	WriteStd(" ? ") ;
	GetCharCommand(&comm, &arg) ;
	switch( comm ) {
		case 'c':
		case '\n': {
			return true ;
		}
		case 'n': {
			SetDebug(false) ;
			return true ;
		}
		case 'a': {
			SetInTopGoal(false) ;
			SetDebug(false) ;
			PrologEvent(5) ;
			return true ;
		}
		case 'e': {
			SetInTopGoal(false) ;
			SetDebug(false) ;
			PrologEvent(4) ;
			return true ;
		}
		case ':': {
			WriteStd("Statistics:\n") ;
			Statistics() ;
			return false ;
		}
		default: {
			WriteStd("CxProlog interrupt/debug options:\n\
   <cr> creep   : statistics   a abort\n\
      c creep   n notrace      e exit    ?h help\n") ;
			return false ;
		}
	}
}

static void InterruptHandler(int a)
{
	signal(SIGINT, SIG_IGN) ;
	if( CheckInTopGoal() )
		SetDebug(true) ;
	else {
		WriteStd("Use ^D or \"halt.\" to exit.\n") ;
		WritePrompt() ;
	}
	signal(SIGINT, InterruptHandler) ;
}

void InitDebug()
{
	signal(SIGINT, InterruptHandler) ;
}

void DebugCall(PredicatePt p)
{
	int i, arity ;

	for(;;) {
		WriteTermStd(C) ;
		WriteStd(" %s", PredNameArity(p)) ;
		if( (arity = PredArity(p)) > 0 ) {
			WriteStd("(") ;
			dotimes(i, arity) {
				WriteTermStd(Xc(i)) ;
				if( i != arity - 1 ) WriteStd(", ") ;
			}
			WriteStd(")") ;
		}
		if( InterDebug() ) break ;
	}
}	

