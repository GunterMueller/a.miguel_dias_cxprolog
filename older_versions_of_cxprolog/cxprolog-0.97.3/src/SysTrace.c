/*
 *   This file is part of the CxProlog system

 *   SysTrace.c
 *   by A.Miguel Dias - 2003/08/06
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

/* flag(sys_trace, _, 1). */


static StreamPt sysTraceStream = nil ;

/* TryOpen is to handle the situation "/tmp/sys_trace" already exists
   and belong to a different user */

static StreamPt TryOpen(CharPt fName)
{
	CharPt fn = WithTmpDirPath(fName, "log") ;
	if( OSExists(fn) && !OSPropWritable(fn) ) {
		Warning("Could not open '%s'", fn) ;
		return nil ;
	}
	else {
		StreamPt srm = FileStreamOpen(fn, mWrite, nil) ;
		ExtraPermanent(srm) ;
		FileAllowsNumericEscapes(StreamChannel(srm)) ;
		return srm ;
	}
}

static void EnsureFileIsOpen(void)
{
	if( sysTraceStream == nil ) {
#if 1
		if( (sysTraceStream = TryOpen("sys_trace")) != nil ) ;
		elif( (sysTraceStream = TryOpen("sys_trace1")) != nil ) ;
		elif( (sysTraceStream = TryOpen("sys_trace2")) != nil ) ;
		else
#endif
			sysTraceStream = currOut ;
		Info(1, "The sys_trace output will be placed in '%s'",
								AtomName(StreamPath(sysTraceStream))) ;
	}
}

void SysTraceMachineRun()
{
#if USE_THREADED_CODE
	if( sysTrace_flag == 4 ) {
		Warning("Threaded code is active so sys_trace level was reduced to 3") ;
		SysTraceUpdateFlag(3) ;
	}
#else
	if( sysTrace_flag == 4 && sysTraceStream != nil )
		for(;;) {
			DisassembleOneInst(sysTraceStream, P) ;
			StreamFlush(sysTraceStream) ;
			InstRun() ;
		}
#endif
}

void SysTraceUpdateFlag(int newValue)
{
	if( sysTrace_debugging > 0 )
		newValue = sysTrace_debugging ;	
	sysTrace_flag = newValue ;
	if( sysTrace_flag > 0 )
		EnsureFileIsOpen() ;
	if( sysTrace_flag > 1 ) {
		Attention() = true ;
		if( sysTrace_flag == 4 && Running() )
			EventContinue() ;
	}
}

void SysTraceHandle(PredicatePt pr)
{
	CharPt s ;
	if( sysTrace_flag > 1 ) {
		Attention() = true ;
		if( sysTrace_flag == 2 && (!PredIsBuiltin(pr) || PredIsMeta(pr)) ) return ;
		HSave() ;
		s = TermAsStr(MakeStruct(PredFunctor(pr), X)) ;
		HRestore() ;
		if( sysTraceStream != nil  ) {
			StreamPutStrNl(sysTraceStream, s) ;
			StreamFlush(sysTraceStream) ;
		}
	}
}

void SysTraceWrite(CharPt s)
{
	if( sysTrace_flag > 0 && sysTraceStream != nil && sysTraceStream != currOut ) {
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
