/*
 *   This file is part of the CxProlog system

 *   Attention.c
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

Bool attention = true ; /* must be true */
Bool interrupted = false ;
Bool booting = true ;
/*static Bool _filler ;*/

static void InterruptMenu()
{
	int comm, arg ;
	for(;;) {
		WriteStd(" Action (h for help): ") ;
		GetCharCommand(&comm, &arg) ;
		switch( comm ) {
			case 'a':
				WriteStd("Execution aborted.\n") ;
				EventRestart() ;
				return ;
			case 'c':
				return ;
			case 'd':
				DebugInterrupt(1) ;
				return ;
			case 'e':
				WriteStd("Bye.\n") ;
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

Bool InterruptHandle()
{
	if( !interrupted )
		return false ;
	interrupted = false ;
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
			EventRestart() ;
			break ;
		case 4:
			InterruptMenu() ;
			break ;
		default:
			Default("InterruptHandle") ;
	}
	return true ;
}

Bool AttentionHandle(PredicatePt pr, Bool visReq)
{
	Attention() = false ;
	AtomsGC() ;
	InterruptHandle() ;
	return DebugCall(pr, visReq) ;
}
