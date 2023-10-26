/*
 *   This file is part of the CxProlog system

 *   Attention.c
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2009 A.Miguel Dias, CITI, DI/FCT/UNL

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

#include "CxProlog.h"
#include <signal.h>
#include <setjmp.h>

Bool attention = true ; /* must start true */
Bool interrupted = false ;
Bool booting = true ;

static Bool CallInterruptPred(CharPt comm)
{
	PredicatePt pr = LookupPredicateByName("on_interrupt", 1) ;
	if( !PredHasClauses(pr) ) return false ;
	return CallProlog(MakeUnStruct(PredFunctor(pr), MakeAtom(comm)), nil) == peSucc ;
}	

static void InterruptMenu()
{
	for(;;) {
		WriteStd("\n") ;
		switch( GetCharCommand("Action (h for help): ", nil) ) {
			case 'a':
				if( !CallInterruptPred("abort") ) {
					WriteStd("%% Aborted.\n") ;
					EventRestart() ;
				}
				return ;
			case 'c':
				CallInterruptPred("continue") ;
				return ;
			case 'd':
				if( !CallInterruptPred("debug") )
					DebugInterrupt(1) ;
				return ;
			case EOF:
				WriteEOF() ;
				/* fall through */
			case 'e':
				if( !CallInterruptPred("exit") ) {
					WriteStd("%% Bye.\n") ;
					EventExit() ;
				}
				return ;
			case 't':
				if( !CallInterruptPred("trace") ) 
					DebugInterrupt(2) ;
				return ;
			default:
				if( !CallInterruptPred("help") )
					WriteStd("\
    a abort\n\
    c continue\n\
    d debug\n\
    e exit\n\
    h help\n\
    t trace\n") ;
		}
	}
}

/* The Windows document
 *     http://msdn.microsoft.com/library/en-us/vclib/html/_crt_signal.asp
 * states: "When a CTRL+C interrupt occurs, Win32 operating systems
 * generate a new thread to specifically handle that interrupt. */

/* On Linux: All signals in the other threads are blocked. So, the
   InMainThread(false) call is not really necessary... but doesn't hurt. */

static void IHandler(int sig)
{
	signal(SIGINT, SIG_IGN) ;
	if( onInterrupt_flag > 0 && InMainThread(false) ) {
		errno = 0 ;	/* Clear EINTR error */
		Interrupted() = true ;
		OSHandleRunInterruptibleFun() ;
		Attention() = true ;
	}
	signal(SIGINT, IHandler) ;
}

void RunProtected(Fun fun)
{
	OSRunInterruptibleFun(fun, IHandler, InterruptHandle) ;
}

void InterruptOff()
{
	if( InteractiveSession() )
		signal(SIGINT, SIG_IGN) ;
}

void InterruptOn()
{
	if( InteractiveSession() ) {
		signal(SIGINT, IHandler) ;
	}
}

Bool InterruptHandle(void)
{
	if( !Interrupted() )
		return false ;
	Interrupted() = false ;
	UnsetRawInput() ;
	switch( onInterrupt_flag ) {
		case 1:
			if( !CallInterruptPred("debug") )
				DebugInterrupt(1) ;
			break ;
		case 2:
			if( !CallInterruptPred("trace") )
				DebugInterrupt(2) ;
			break ;
		case 3:
			if( !CallInterruptPred("restart") )
				EventRestart() ;
			break ;
		case 4:
			InterruptMenu() ;
			break ;
		default: InternalError("InterruptHandle") ;
	}
	return true ;
}

#if USE_THREADED_CODE
static void CheckThreadedCode()
{
	static int *p = nil ;
	int i ;
	if( p != &i ) {
		if( p == nil ) p = &i ;
		else Write("Threaded code is damaged") ;
	}
}
#endif

Bool AttentionHandle(PredicatePt pr)
{
	Attention() = false ;
	ExtraGC() ;
	InterruptHandle() ;
	SysTraceHandle(pr) ;
#if USE_THREADED_CODE
	CheckThreadedCode() ;
#endif
	return DebugCall(pr) ;
}