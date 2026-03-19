/*
 *   This file is part of the CxProlog system

 *   Attention.c
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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

Bool attention = true ; /* must start true */
Bool interrupted = false ;
Bool booting = true ;

static void InterruptMenu()
{
	for(;;) {
		WriteStd("\nAction (h for help): ") ;
		switch( GetCharCommand(nil) ) {
			case 'a':
				WriteStd("%% Aborted.\n") ;
				EventRestart() ;
				return ;
			case 'c':
				return ;
			case 'd':
				DebugInterrupt(1) ;
				return ;
			case EOF:
				WriteEOF() ;
				/* fall through */
			case 'e':
				WriteStd("%% Bye.\n") ;
				EventExit() ;
				return ;
			case 't':
				DebugInterrupt(2) ;
				return ;
			default:
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

static void IHandler(int a)
{
	signal(SIGINT, SIG_IGN) ;
	Interrupted() = true ;
	Attention() = true ;
	signal(SIGINT, IHandler) ;
}

void InterruptOff()
{
	signal(SIGINT, SIG_IGN) ;
}

void InterruptOn()
{
	signal(SIGINT, IHandler) ;
	DisallowInterruptibleSysCalls() ;
}

Bool InterruptHandle()
{
	if( !Interrupted() )
		return false ;
	Interrupted() = false ;
	UnsetRawInput() ;
	switch( interrupt_flag ) {
		case 0:
			break ;
		case 1:
			DebugInterrupt(1) ;
			break ;
		case 2:
			DebugInterrupt(2) ;
			break ;
		case 3:
			InterruptOn() ;
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
