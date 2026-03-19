/*
 *   This file is part of the CxProlog system

 *   Process.c
 *   by A.Miguel Dias - 2002/01/12
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL

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

typedef struct Message {
	int pid ;
	int info ;
} Message ;

typedef struct Process {
	ExtraDef(Process) ;
	int id ;
	int alive ;
} Process, *ProcessPt ;

#define cProcessPt(x)			((ProcessPt)(x))

#define ProcessId(p)			((p)->id)
#define ProcessAlive(p)			((p)->alive)

static ExtraTypePt processType ;
static Bool isFather = true ;
static int rw[2] = {-1} ;

/* simple single message buffer */
static Message mesgBuf = {-1} ;
#define MesgAvailable()		( mesgBuf.pid != -1 )
#define MesgConsume(m)		( m = mesgBuf, mesgBuf.pid = -1 )
#define MesgSave(m)			( mesgBuf = (m) )


static void ProcessDeleteAllAux(VoidPt x)
{
	ExtraDelete(processType, x) ;
}
static void ProcessDeleteAll(void)
{
	ExtraForEach(processType, ProcessDeleteAllAux) ;
}

static Bool IdToProcessAux(VoidPt x, VoidPt arg)
{
	return ProcessId(cProcessPt(x)) == (int)arg ;
}
static ProcessPt IdToProcess(int id)
{
	return ExtraFindFirst(processType, IdToProcessAux, cVoidPt(id)) ;
}

static ProcessPt ProcessNew(Pt startGoal, Pt restartGoal)
{
	int child ;
	if( rw[0] == -1 )
		OSPipe(rw) ;
	if( (child = OSFork()) > 0 ) {	/* PARENT */
		ProcessPt p = ExtraNew(processType) ;
		ProcessId(p) = child ;
		ProcessAlive(p) = true ;
		return p ;
	}
	else {							/* CHILD */
		ProcessDeleteAll() ;
		isFather = false ;	/* mark "this is a child" */
	/*	termSegm[3] = termSegm[0] = cPt(LookupFunctorByName(">>", 2)) ;
		termSegm[4] = termSegm[1] = MakeAtom("main") ;
		termSegm[2] = startGoal ;
		termSegm[5] = restartGoal ;
		ActiveThreadReplace(TagStruct(termSegm), TagStruct(termSegm+3)) ;*/
		ActiveThreadReplace(startGoal, restartGoal) ;	
		ActiveThreadStart() ;
	}
	return nil ;
}

static Bool ProcessRead(Message *mesg, Bool blocking)
{
	Bool b = OSRead(rw[0], mesg, sizeof(Message), blocking) ;
	if( mesg->info == -1 ) {
		ProcessAlive(IdToProcess(mesg->pid)) = false ;
		OSWait() ;
	}
	return b ;
}

static void ProcessWrite(Message mesg)
{
	OSWrite(rw[1], &mesg, sizeof(Message)) ;
}

Bool ProcessIsChild()
{
	return not isFather ;
}

void ProcessMesgToFather(int info)
{
	Message mesg ;
	if( not ProcessIsChild() )
		Error("The parent process cannot send") ;
	if( rw[0] == -1 )
		Error("The pipe was not open by parent") ;
	mesg.pid = OSGetPid() ;
	mesg.info = info ;
	ProcessWrite(mesg) ;
}

/* CXPROLOG C'BUILTINS */

static void PProcessCheck()
{
	if( XExtraCheck(processType, X0) ) JumpNext()
	DoFail()
}

static void PProcessNew()
{
	if( ProcessIsChild() )
		Error("A child process cannot do that") ;
	BindVarWithExtra(X0, ProcessNew(Drf(X1), Drf(X2))) ;
	JumpNext()
}

static void PProcessSend()
{
	ProcessMesgToFather(XTestNat(X0)) ;
	JumpNext()
}

static void PProcessSendMax()
{
	if( Unify(X0, MakeInt(OSPipeBufferSize() / sizeof(Message))) )
		JumpNext()
	DoFail()
}

static void PProcessReceive()
{
	Message mesg ;
	ProcessPt p ;
	if( ProcessIsChild() )
		Error("A child process cannot receive") ;
	if( rw[0] == -1 )
		Error("No pipe created yet") ;
	if( MesgAvailable() )
		MesgConsume(mesg) ;
	else
		ProcessRead(&mesg, true) ;
	if( (p = IdToProcess(mesg.pid)) == nil )
		Error("Invalid process") ;
	if( Unify(X0, TagExtra(p)) && Unify(X1, MakeInt(mesg.info)) )
		JumpNext()
	DoFail()
}

static void PProcessReceiveReady()
{
	if( ProcessIsChild() )
		Error("A child process cannot do that") ;
	if( rw[0] == -1 )
		Error("No pipe created yet") ;
	if( MesgAvailable() )
		JumpNext()
	else {
		Message mesg ;
		if( ProcessRead(&mesg, false) ) {
			MesgSave(mesg) ;
			JumpNext() 
		}
		DoFail()
	}
}

static void ProcessesCleanDeadAux(VoidPt x)
{
	ProcessPt p = cProcessPt(x) ;
	if( not ProcessAlive(p) )
		ExtraDelete(processType, p) ;
}
static void PProcessesCleanDead()
{
	ExtraForEach(processType, ProcessesCleanDeadAux) ;
}

static void PNDCurrentProcess()
{
	PNDCurrentExtra(processType) ;
	JumpNext()
}

static void ProcessesAux(VoidPt x)
{
	ProcessPt p = cProcessPt(x) ;
	AtomPt at = IVarWith(TagExtra(p)) ;
	Write("  %16s -> id = %d, alive = %d", 
				TermAsStr(TagExtra(p)),
				ProcessId(p),
				ProcessAlive(p)) ;
	if( at != nil )
		Write(" (in ivar '%s')", AtomName(at)) ;
	Write("\n") ;
}
static void PProcesses()
{
	VersionShow() ;
	Write("Processes:\n") ;
	ExtraForEach(processType, ProcessesAux) ;
	JumpNext()
}

static void POSRun()
{
	if( OSRun(XTestAtomName(X0)) ) JumpNext()
	DoFail()
}

static void PSh()
{
	if( OSRun("sh") ) JumpNext()
	DoFail()
}

static void POSGetEnv()
{
	CharPt s ;
	if( (s = OSGetEnv(XTestAtomName(X0))) != nil
						&& Unify(X1, MakeTempAtom(s)) )
		JumpNext()
	DoFail()
}

static void POSSleep()
{
	OSSleep(XTestNat(X0)) ;
	JumpNext()
}

void ProcessesInit()
{
	processType = ExtraTypeNew("process", WordsOf(Process)) ;

	InstallCBuiltinPred("process", 1, PProcessCheck) ;
	InstallCBuiltinPred("process_new", 3, PProcessNew) ;
	InstallCBuiltinPred("process_send_father", 1, PProcessSend) ;
	InstallCBuiltinPred("process_send_father_max", 1, PProcessSendMax) ;
	InstallCBuiltinPred("process_receive_from_child", 2, PProcessReceive) ;
	InstallCBuiltinPred("process_receive_from_child_ready", 0,
												PProcessReceiveReady) ;
	InstallCBuiltinPred("processes_clean_dead", 0, PProcessesCleanDead) ;
	InstallNDeterCBuiltinPred("current_process", 1, PNDCurrentProcess) ;
	InstallCBuiltinPred("processes", 0, PProcesses) ;

	InstallCBuiltinPred("os_run", 1, POSRun) ;
	InstallCBuiltinPred("system", 1, POSRun) ;			/* c-prolog */
	InstallCBuiltinPred("sh", 0, PSh) ;					/* c-prolog */
	InstallCBuiltinPred("os_env", 2, POSGetEnv) ;
	InstallCBuiltinPred("os_sleep", 1, POSSleep) ;
}

static void ProcessesFinishAux(VoidPt x)
{
	ProcessPt p =  cProcessPt(x) ;
	ExtraDelete(processType, p) ;
	OSKill(ProcessId(p)) ;
}
void ProcessesFinish()
{
	if( ProcessIsChild() )
		ProcessMesgToFather(-1) ;
	else
		ExtraForEach(processType, ProcessesFinishAux) ;
}

/*
static void PProcessIsChild()
{
	if( ProcessIsChild() ) JumpNext()
	DoFail()
}

static void PProcessDelete()
{
	ProcessPt p =  XTestExtra(processType,X0) ;
	kill(ProcessId(p), 9) ;
	ExtraDelete(processType, p) ;
	JumpNext()
}
	InstallCBuiltinPred("process_is_child", 0, PProcessIsChild) ;
	InstallCBuiltinPred("process_delete", 1, PProcessDelete) ;
*/
