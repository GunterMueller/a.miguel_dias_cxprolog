/*
 *   This file is part of the CxProlog system

 *   Interrupt.c
 *   by A.Miguel Dias - 2001/04/24
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

/* INTERRUPT */

static Bool forceCxRestart = false ;

static void IHandler(int a)
{
	signal(SIGINT, SIG_IGN) ;
	if( DebugActivate() ) /* nothing */ ;
	else forceCxRestart = true ;
	AttentionActivate() ;
	signal(SIGINT, IHandler) ;
}

void InterruptHandle()
{
	if( forceCxRestart ) {
		forceCxRestart = false ;
		EventRestart() ;
	}
}

void InterruptOff()
{
	signal(SIGINT, SIG_IGN) ;
}

void InterruptOn()
{
	signal(SIGINT, IHandler) ;
#ifdef unix
	siginterrupt(SIGINT, 1) ;
#endif
}


/* ATTENTION */

Bool attention = false ;

void AttentionHandle(PredicatePt pr, FunctorPt f)
{
	AtomsGC() ;
	InterruptHandle() ;
	DebugCall(pr, f) ;
	attention = DebugTraceIsOn() ;
}
