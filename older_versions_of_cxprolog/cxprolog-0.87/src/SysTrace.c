/*
 *   This file is part of the CxProlog system

 *   SysTrace.c
 *   by A.Miguel Dias - 2003/08/06
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

/* flag(sys_trace, _, 1). */

static StreamPt sysTraceStream = nil ;

void SysTraceMachineRun()
{
	for(;;) {
		DisassembleInst(sysTraceStream, P) ;
		StreamFlush(sysTraceStream) ;
		InstRun() ;
	}
}

void SysStraceUpdateFlag(int newValue)
{
	sysTrace_flag = newValue ;
	if( sysTraceStream == nil && sysTrace_flag > 0 )
#if 0
		sysTraceStream = currOut ;
#else
		sysTraceStream = FileStreamOpen("sys_trace.log", mWrite, nil) ;
#endif
	if( sysTrace_flag == 1 || sysTrace_flag == 2 )
		Attention() = true ;
	elif( sysTrace_flag == 3 )
		EventContinue() ;
}

void SysTraceHandle(PredicatePt pr)
{
	CharPt s ;
	if( sysTrace_flag == 1 || sysTrace_flag == 2 ) {
		Attention() = true ;		
		if( sysTrace_flag == 1 && (!PredIsBuiltin(pr) || PredIsMeta(pr)) ) return ;
		HSave() ;
		s = TermAsStr(MakeStruct(PredFunctor(pr), X)) ;
		HRestore() ;
		StreamWrite(sysTraceStream, "%s\n", s) ;
		StreamFlush(sysTraceStream) ;
	}
}

void SysTraceInit()
{
/* These functors are made META only to reduce the output when sys_trace == 1.
   Note it is safe to do this only for zeroary functors.
*/
	FunctorIsMeta(LookupFunctorByName("fail", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("false", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("true", 0)) = true ;
}
