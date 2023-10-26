/*
 *   This file is part of the CxProlog system

 *   SysTrace.c
 *   by A.Miguel Dias - 2003/08/06
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
		ExtraSetPermanent(sysTraceStream) ;
#endif
}

void SysTraceMachineRun()
{
#if USE_THREADED_CODE
	if( SysTraceFlag() == 3 ) {
		Warning("Threaded code is active so sys_trace level was reduced to 2") ;
		SysTraceUpdateFlag(2) ;
	}
#else
	if( SysTraceFlag() == 3 ) {
	    EnsureFileIsOpen() ;
	    for(;;) {
		    DisassembleOneInst(sysTraceStream, P) ;
		    StreamFlush(sysTraceStream) ;
		    InstRun() ;
	    }
	}
#endif
}

void SysTraceUpdateFlag(int newValue)
{	/* sysTrace_flag >= 10 overrides flag/3 */
	if( sysTrace_flag >= 10 && newValue < 10 ) return ;

	sysTrace_flag = newValue ;
	if( SysTraceFlag() > 0 ) {
		Attention() = true ;
		if( SysTraceFlag() == 3 && Running() )
			EventContinue() ;
	}
}

void SysTraceHandle(PredicatePt pr)
{
	CharPt s ;
	if( SysTraceFlag() > 0 ) {
		Attention() = true ;		
		if( SysTraceFlag() == 1 && (!PredIsBuiltin(pr) || PredIsMeta(pr)) ) return ;
		HSave() ;
		s = TermAsStr(MakeStruct(PredFunctor(pr), X)) ;
		HRestore() ;
		EnsureFileIsOpen() ;
		StreamPutStrNl(sysTraceStream, s) ;
		StreamFlush(sysTraceStream) ;
	}
}

void SysTraceWrite(CharPt s)
{
	if( SysTraceFlag() > 0 && Running() ) { /* required */
		EnsureFileIsOpen() ;
		StreamPutStr(sysTraceStream, s) ;
		StreamFlush(sysTraceStream) ;
	}
}

void SysTraceInit()
{
/* These functors are made META only to reduce the output when sys_trace == 1.
   Note it is safe to do this only for zeroary functors. */
	FunctorIsMeta(LookupFunctorByName("fail", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("false", 0)) = true ;
	FunctorIsMeta(LookupFunctorByName("true", 0)) = true ;
}
