/*
 *   This file is part of the CxProlog system

 *   SysTrace.c
 *   by A.Miguel Dias - 2003/08/06
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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

static void EnsureFileIsOpen(void)
{
	if( sysTraceStream == nil && SysTraceFlag() > 0 )
#if 0
		sysTraceStream = currOut ;
#else
		sysTraceStream = FileStreamOpen("/tmp/sys_trace.log", mWrite, nil) ;
#endif
}

void SysTraceMachineRun()
{
	for(;;) {
		DisassembleInst(sysTraceStream, P) ;
		StreamFlush(sysTraceStream) ;
		InstRun() ;
	}
}

void SysTraceUpdateFlag(int newValue)
{	/* sysTrace_flag >= 10 overrides flag/3 */
	if( sysTrace_flag >= 10 && newValue < 10 ) return ;

	sysTrace_flag = newValue ;
	EnsureFileIsOpen() ;
	if( SysTraceFlag() == 1 || SysTraceFlag() == 2 )
		Attention() = true ;
	elif( SysTraceFlag() == 3 && P != &NotRunning )
		EventContinue() ;
}

void SysTraceHandle(PredicatePt pr)
{
	CharPt s ;
	if( SysTraceFlag() == 1 || SysTraceFlag() == 2 ) {
		Attention() = true ;		
		if( SysTraceFlag() == 1 && (!PredIsBuiltin(pr) || PredIsMeta(pr)) ) return ;
		HSave() ;
		s = TermAsStr(MakeStruct(PredFunctor(pr), X)) ;
		HRestore() ;
		StreamPutStrNl(sysTraceStream, s) ;
		StreamFlush(sysTraceStream) ;
	}
}

void SysTraceWrite(CharPt s)
{
	EnsureFileIsOpen() ;
	StreamPutStr(sysTraceStream, s) ;
	StreamFlush(sysTraceStream) ;
}

void SysTraceInit()
{
/* These functors are made META only to reduce the output when sys_trace == 1.
   Note it is safe to do this only for zeroary functors. */
	FunctorIsMeta(LookupFunctorByName("fail", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("false", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("true", 0)) = true ;
}
