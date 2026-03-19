/*
 *   This file is part of the CxProlog system

 *   Debug.c
 *   by A.Miguel Dias - 2000/05/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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

Bool traceActive_flag, restartNow = false ;
static Bool debugging_allowed ;

void GetCharCommand(int *comm, int *arg)
{
	int c, n ;
	FlushStream(userOut) ;
	while( ( c = GetStream(userIn) ) <= ' ' && c != '\n' && not cx_iseof(c) ) ;
	*comm = InRange(c,'A','Z') ? (c - 'A' + 'a') : c ;
	n = 0 ;
	while( c != '\n' && not cx_iseof(c) ) {
		if( InRange(c, '0', '9') )
			n = n * 10 + c - '0' ;
		c = GetStream(userIn) ;
	}
	*arg = n ;
	while( c != '\n' && not cx_iseof(c) )
		c = GetStream(userIn) ;
}

static void InterDebug(Pt cx, Pt t)
{
	int comm, arg ;
	for(;;) {
		WriteStd("%s ", TermAsStr(cx)) ;
		WriteStd("%s ? ", TermAsStr(t)) ;
		GetCharCommand(&comm, &arg) ;
		switch( comm ) {
			case 'c':
			case '\n': {
				return ;
			}
			case 'n': {
				traceActive_flag = false ;
				return ;
			}
			case 'f': {
				EventForceFail() ;
				return ;
			}
			case 'a': {
				WriteStd("Execution aborted.\n") ;
				EventRestart() ;
				return ;
			}
			case 'e': {
				WriteStd("Bye.\n") ;
				EventExit() ;
				return ;
			}
			case ':': {
				WriteStd("Statistics:\n") ;
				ShowStatistics() ;
				break ;
			}
			default: {
				WriteStd("\
CxProlog interrupt/debug options:\n\
   <cr> creep   f fail      : statistics  a abort\n\
      c creep   n notrace   ?h help       e exit\n") ;
				break ;
			}
		}
	}
}

void DebugCall(PredicatePt pr, FunctorPt f)
{
	Hdl saveH = H ;
	if( restartNow ) {
		restartNow = false ;
		traceActive_flag = false ;
		EventRestart() ;
	}
	if( pr != nil )
		if( PredIsTraceable(pr) )
			f = PredFunctor(pr) ;
		else return ;
	InterDebug(C,MakeStruct(f, X)) ;
	H = saveH ;
}	

static void InterruptHandler(int a)
{
	int dummy = a ;		/* This avoids a compiler warning */
	signal(SIGINT, SIG_IGN) ;
	restartNow = not (debugging_allowed && debugging_flag) ;
	traceActive_flag = true ;
	signal(SIGINT, InterruptHandler) ;
}

void DebugReset()
{
	debugging_allowed = false ;
	DebugUpdate() ;
	signal(SIGINT, InterruptHandler) ;
}

void DebugUpdate()
{
	trace_flag = trace_flag && debugging_flag ;
	traceActive_flag = debugging_allowed && trace_flag ;
}

/* CXPROLOG C'BUILTINS */

static void PInitDebugging()
{
	debugging_allowed = true ;
	DebugUpdate() ;
	JumpNext()
}

static void PEndDebugging()
{
	debugging_allowed = false ;
	DebugUpdate() ;
	JumpNext()
}

void InitDebug()
{
	PredicatePt pr ;
	siginterrupt(SIGINT, 1) ;
	DebugReset() ;
	pr = InstallCBuiltinPred("$$_init_debugging", 0, PInitDebugging) ;
	PredIsTraceable(pr)	= false ;
	pr = InstallCBuiltinPred("$$_end_debugging", 0, PEndDebugging) ;
	PredIsTraceable(pr)	= false ;
	PredIsTraceable(LookupPredicate(LookupFunctorByName("trace", 0), false)) = false ;
	PredIsTraceable(LookupPredicate(LookupFunctorByName("notrace", 0), false)) = false ;
}

/*
Long time goal:

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
