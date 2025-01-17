/*
 *   This file is part of the CxProlog system

 *   Thread.c
 *   by A.Miguel Dias - 1993/07/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL

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

/*	
What is called threads here, are really simply coroutines.
Must change thread->coroutine in the name of the predicates.

    Threads organization: tree

	Killing a thread also kills all of its descendents
	ThreadTransfer always produces one of these results:
							going, completed, failed, killed, error
*/

/* @@@ Basic garbage collection may need to be revised for inactive threads */

#define initialStacksCapacity	(stacksDebugging_flag ? 32 K K : 16 K)
#define initialTrailCapacity	(trailDebugging_flag ? 128 K : 256)

typedef struct Thread {
	ExtraDef(Thread) ;
	Hdl stacksBegin0, stacksEnd0 ;
	Hdl stacksBegin, stacksEnd ;
	Hdl trailBegin, trailEnd ;
	Hdl P, CP, H, HB, TR ;
	FinalizerPt F ;
	ChoicePointPt B, B0, R, L ;
	EnvironmentPt E ;
	Pt C, CH, X[maxX] ;
	Pt startGoal, restartGoal ;
	struct Thread *father, *sons, *brother ;
	Bool result ;
	
	StreamPt userIn, userOut, userErr ;
	Bool waitingInput, autoOutput ;
	int retries ;
	int marksSeen ;
	int interrupted ;
} Thread ;

#define	cThreadPt(t)			((ThreadPt)(t))

#define ThreadStacksBegin0(th)	((th)->stacksBegin0)
#define ThreadStacksEnd0(th)	((th)->stacksEnd0)
#define ThreadStacksBegin(th)	((th)->stacksBegin)
#define ThreadStacksEnd(th)		((th)->stacksEnd)
#define ThreadTrailBegin(th)	((th)->trailBegin)
#define ThreadTrailEnd(th)		((th)->trailEnd)
#define ThreadReg(th,R)			((th)->R)
#define ThreadX(th)				((th)->X)
#define ThreadStartGoal(th)		((th)->startGoal)
#define ThreadRestartGoal(th)	((th)->restartGoal)
#define ThreadFather(th)		((th)->father)
#define ThreadSons(th)			((th)->sons)
#define ThreadBrother(th)		((th)->brother)
#define ThreadResult(th)		((th)->result)

#define ThreadUserIn(th)		((th)->userIn)
#define ThreadUserOut(th)		((th)->userOut)
#define ThreadUserErr(th)		((th)->userErr)
#define ThreadBufferIn(th)		StreamChannel(ThreadUserIn(th))
#define ThreadBufferOut(th)		StreamChannel(ThreadUserOut(th))
#define ThreadWaiting(th)		((th)->waitingInput)
#define ThreadAutoOutput(th)	((th)->autoOutput)
#define ThreadRetries(th)		((th)->retries)
#define ThreadMarksSeen(th)		((th)->marksSeen)
#define ThreadInterrupted(th)	((th)->interrupted)


static ExtraTypePt threadType ;
static ThreadPt root = nil, active = nil ;
static InstPt NotRunningAddr ;
static Size termStartPos = -1 ;

/* PRIVATE */

static Bool IsAncestor(ThreadPt a, ThreadPt b)
{
	ThreadPt th ;
	if( a == b )
		return true ;
	doseq(th, ThreadSons(a), ThreadBrother(th))
		if( IsAncestor(th, b) )
			return true ;
	return false ;
}

static void ThreadReset(register ThreadPt th)
{
	ThreadReg(th,H) = ThreadStacksBegin(th) + 3 ;	/* Initial context is [main] */
	ThreadReg(th,HB) = ThreadStacksBegin(th) ;
	ThreadReg(th,TR) = ThreadTrailBegin(th) ;
	ThreadReg(th,F) = cFinalizerPt(ThreadTrailEnd(th)) ;
	ThreadReg(th,B) = cChoicePointPt(ThreadStacksEnd(th)) - 1 ; /* Space for a CP, required */
	ChoicePointField(ThreadReg(th,B), C) = TagList(ThreadStacksBegin(th) + 1) ; /* Required by GC */
	*cHdl(ThreadReg(th,B)) = endOfChainMark ;	/* Fill end-of-chains slot */	
	ThreadReg(th,B)->H = ThreadReg(th,HB) ;		/* Will be restored at end of execution */
	ThreadReg(th,B0) = ThreadReg(th,B) ;
	ThreadReg(th,R) = ThreadReg(th,B) ;
	ThreadReg(th,L) = ThreadReg(th,B) ;
	ThreadReg(th,E) = cEnvironmentPt(ThreadStacksEnd(th)-1) ; /* B<E for TopOfLocalStack(), within */
	*cHdl(ThreadReg(th,E)) = endOfChainMark ;	/* Fill end-of-chains slot */
	ThreadReg(th,P) = NotRunningAddr ;	/* Flag that does not put nil on stacks */
	ThreadReg(th,CP) = NotRunningAddr ; /* It's ok because B is on top */
	ThreadReg(th,C) = TagList(ThreadReg(th,H)-2) ;	/* Initial context is [main] */
	ThreadReg(th,CH) = tNilAtom ;
	EmptyRangeN(ThreadX(th), maxX) ; /* This is needed */
	
	ThreadWaiting(th) = false ;
	ThreadAutoOutput(th) = false ;
	ThreadRetries(th) = 0 ;
	ThreadInterrupted(th) = false ;
}

static void ThreadRestore(register ThreadPt th)
{
	stacksBegin0 = ThreadStacksBegin0(th) ;
	stacksEnd0 = ThreadStacksEnd0(th) ;
	stacksBegin = ThreadStacksBegin(th) ;
	stacksEnd = ThreadStacksEnd(th) ;
	trailBegin = ThreadTrailBegin(th) ;
	trailEnd = ThreadTrailEnd(th) ;

	H = ThreadReg(th,H) ;
	HB = ThreadReg(th,HB) ;
	TR = ThreadReg(th,TR) ;
	F = ThreadReg(th,F) ;
	B = ThreadReg(th,B) ;
	B0 = ThreadReg(th,B0) ;
	R = ThreadReg(th,R) ;
	L = ThreadReg(th,L) ;
	E = ThreadReg(th,E) ;
	P = ThreadReg(th,P) ;
	CP = ThreadReg(th,CP) ;
	C = ThreadReg(th,C) ;
	CH = ThreadReg(th,CH) ;
	CopyUntilEmpty(X, ThreadX(th)) ;
	
	if( StreamKind(ThreadUserIn(th)) == threadInputBufferStream  ) {
		currIn = userIn = ThreadUserIn(th) ; // currIn ??? incomplete
		currOut = userOut = ThreadUserOut(th) ; // currIn ??? incomplete
		userErr = ThreadUserErr(th) ;
	}
	else {
		userIn = ThreadUserIn(th) ; // currIn ??? incomplete
		currOut = ThreadUserOut(th) ; // currIn ??? incomplete
		userErr = ThreadUserErr(th) ; // currIn ??? incomplete
	}
	StreamsSetUser(userIn, userOut, userErr);
	active = th ;
}

static void ThreadSave(register ThreadPt th)
{
	ThreadStacksBegin0(th) = stacksBegin0 ;
	ThreadStacksEnd0(th) = stacksEnd0 ;
	ThreadStacksBegin(th) = stacksBegin ;
	ThreadStacksEnd(th) = stacksEnd ;
	ThreadTrailBegin(th) = trailBegin ;
	ThreadTrailEnd(th) = trailEnd ;

	ThreadReg(th,H) = H ;
	ThreadReg(th,HB) = HB ;
	ThreadReg(th,TR) = TR ;
	ThreadReg(th,F) = F ;
	ThreadReg(th,B) = B ;
	ThreadReg(th,B0) = B0 ;
	ThreadReg(th,R) = R ;
	ThreadReg(th,L) = L ;
	ThreadReg(th,E) = E ;
	ThreadReg(th,P) = P ;
	ThreadReg(th,CP) = CP ;
	ThreadReg(th,C) = C ;
	ThreadReg(th,CH) = CH ;
	CopyUntilEmpty(ThreadX(th), X) ;
}

static ThreadPt ThreadNew(Pt startGoal, Pt restartGoal)
{
	ThreadPt th = ExtraNew(threadType, 0) ;
	ThreadStacksBegin0(th) =
		AllocateSegmentEmpty(initialStacksCapacity, &ThreadStacksEnd0(th)) ;
	ThreadStacksBegin(th) = ThreadStacksBegin0(th) ;
	ThreadStacksEnd(th) = ThreadStacksEnd0(th) ;
	ThreadTrailBegin(th) =
		AllocateSegmentEmpty(initialTrailCapacity, &ThreadTrailEnd(th)) ;

/* Setup the first three words of the global stack, the initial [main] context. */
	Hdl h = ThreadStacksBegin(th) ;
	h[0] = cPt(TermToUnit(tMainAtom, nil)) ; /* Unit is ref stored in the global stack */
	h[1] = tMainAtom ;					/* Link unit-term in the context list */
	h[2] = tNilAtom ;
	
	ThreadReset(th) ;
	if( root == nil ) { // creating the first thread
		ThreadUserIn(th) = userIn ;
		ThreadUserOut(th) = userOut ;
		ThreadUserErr(th) = userErr ;
	}
	else {			// creating another thread
		BufferPt buff = BufferNew() ;
		ExtraPermanent(buff) ;
		ThreadUserIn(th) = BufferStreamOpen(buff, mRead, nil);
		StreamKind(ThreadUserIn(th)) = threadInputBufferStream;
		BufferAutoAdjustOn(ThreadBufferIn(th));

		buff = BufferNew() ;
		ExtraPermanent(buff) ;
		ThreadUserOut(th) = BufferStreamOpen(buff, mWrite, nil);
		ThreadUserErr(th) = ThreadUserOut(th) ;
//		ThreadUserOut(th) = userOut ;
//		ThreadUserErr(th) = userErr ;
	}
	ThreadStartGoal(th) = AllocateTermForAssign(startGoal) ;
	ThreadRestartGoal(th) = AllocateTermForAssign(restartGoal) ;
	ThreadFather(th) = active ;
	ThreadSons(th) = nil ;
	ThreadBrother(th) = nil ;
	ThreadResult(th) = false ;
	if( active != nil ) {
		ThreadBrother(th) = ThreadSons(active) ;
		ThreadSons(active) = th ;
	}
	return th ;
}

static void ThreadRelease(ThreadPt th)  // pre: th != root, th != active
{
	register ThreadPt t ;
	doseq(t, ThreadSons(th), ThreadBrother(t))
		ThreadRelease(t) ;
	ReleaseSegment(ThreadStacksBegin0(th), ThreadStacksEnd0(th) - ThreadStacksBegin0(th)) ;
	ReleaseSegment(ThreadTrailBegin(th), ThreadTrailEnd(th) - ThreadTrailBegin(th)) ;
	ReleaseTerm(ThreadStartGoal(th)) ;
	ReleaseTerm(ThreadRestartGoal(th)) ;	
	StreamClose(ThreadUserIn(th), nil);
	StreamClose(ThreadUserOut(th), nil);
}

static void ThreadPrepareToRun(ThreadPt th)
{
	if( P == NotRunningAddr ) {
		Pt goal = ThreadStartGoal(th) ;
		CheckPredicate(XTestFunctor(goal)) ;
		X0 = ZPushTerm(goal) ;
		P = PredCode(CheckPredicateByName("$$_run_thread", 1)) ;
	}
}

static void ThreadDelete(ThreadPt th)
{
	if( ThreadSons(ThreadFather(th)) == th )	/* first is removed */
		ThreadSons(ThreadFather(th)) = ThreadBrother(th) ;
	else {
		register ThreadPt x ;
		for( x = ThreadSons(ThreadFather(th)) ;
							ThreadBrother(x) != th ; x = ThreadBrother(x) ) ;
		ThreadBrother(x) = ThreadBrother(th) ;
	}
	ThreadRelease(th) ;
}

static void ThreadReplaceGoals(ThreadPt th, Pt startGoal, Pt restartGoal)
{
	ReleaseTerm(ThreadStartGoal(th)) ;
	ThreadStartGoal(th) = AllocateTermForAssign(startGoal) ;
	ReleaseTerm(ThreadRestartGoal(th)) ;
	ThreadRestartGoal(th) = AllocateTermForAssign(restartGoal) ;
}

static Size ThreadSizeFun(CVoidPt x)
{
	Unused(x) ;
	return WordsOf(Thread) ;
}

static void ThreadSwitchTo(ThreadPt th)
{
	if( active != nil )
		ThreadSave(active) ;
	ThreadRestore(th) ;
	ThreadPrepareToRun(th) ;
}

static void ThreadTransferTo(ThreadPt th, Pt message)
{
	ThreadSwitchTo(th) ;

/* This is a transfer_to_thread syncronize point */
/* Message from leaving thread to incoming thread */
	if( UnifyWithAtomic(X1, message) )
		EventContinue() ;
	else EventForceFail() ;
}


/* PUBLIC */

ThreadPt ThreadByName(Str alias)
{
	ThreadPt th = cThreadPt(AliasGet(threadType, MakeTempAtom(alias)));
	if( th == nil )
		Error("ThreadGet returned nil for alias: '%s'", alias) ;
	return th ;
}

Bool ThreadActiveIsRoot()
{
	return active == root ;
}

void ThreadRootCreate(Pt startGoal, Pt restartGoal)
{
	if( root != nil )
		FatalError("The root thread can only be created once") ;
	active = root = ThreadNew(startGoal, restartGoal) ;
	BindVarWithExtra(MakeAtom("root"), root) ;
	ExtraPermanent(root) ;
	ThreadRestore(root) ;	/* initialize trailBegin, stacksBegin0, etc. */
}

void ActiveThreadReplace(Pt startGoal, Pt restartGoal)
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadReplace") ;
	ThreadReplaceGoals(active, startGoal, restartGoal) ;
}

void ActiveThreadReset()
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadReset") ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadReset(active) ;
	ThreadRestore(active) ;
	ThreadPrepareToRun(active) ;
}

void ActiveThreadStart()
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadStart") ;
	ActiveThreadReset() ;
	EventContinue() ;
}

void ActiveThreadRestart()
{
	if( active == nil )
		FatalError("No active thread in ActiveThreadRestart") ;
	ActiveThreadReset() ;
	EventContinue() ;
}


void ThreadSwitchToRoot(void)
{
	if( active != root ) {
		ThreadSwitchTo(root) ;
	}
}

static void ActiveThreadFinish(Pt message)
{
	ThreadPt father = ThreadFather(active) ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadDelete(active) ;
	active = nil ;
	ThreadTransferTo(father, message) ;
}


/* Concurrency */

/*

[user].
z :- true.
a :- repeat, writeln(a), os_sleep(0.1), z, '$$_thread_inc_call_counter', fail.
q :- between(0,10000,X), writeln(X), os_sleep(0.1), z, fail.
qq :- writeln(qqq), halt.
r :- repeat, writeln(rrr1), thread_run_a_bit(q), z, writeln(rrr2), fail.
end_of_file.

thread_new(q,q,qq).

threads_concurrency(true), a.


[user].
z :- true.
b(X) :- writeln(X), os_sleep(0.1), z, Y is X + 1, '$$_thread_inc_call_counter', b(Y).
bb :- writeln(bbb), halt.
r :- repeat, writeln(rrr1), thread_run_a_bit(b), z, writeln(rrr2), fail.
end_of_file.

thread_new(b,b(0),bb).


swt
Display.syncExec(Runnable runnable)

threads_concurrency(true), a.

*/

#define THREAD_DEBUG	1
#define TIMER_PERIOD	0.1

static Bool concurrencyIsOn = false ;
Bool threadSwitchRequest = false ;
static ThreadPt restoreFromRunABit = nil ;
static Size sws = 0, callCount = 0 ;
static WChar cc = '\n';

static void ThreadInputReset(void)
{
	BufferClear(ThreadBufferIn(active));
	ThreadInputGetCharReset();
	cc = '\n' ;
}

void ThreadSetAutoOutput(ThreadPt th, Bool ao)
{
	ThreadAutoOutput(th) = ao ;
}

static void timerAction(void)
{
	threadSwitchRequest = true;
}

static void ThreadRestoreFromRunABit(void)
{
//	Mesg("ThreadRestoreFromRunABit %s (%s)", AtomName(AliasSearch(active)), AtomName(AliasSearch(restoreFromRunABit)));
	if( restoreFromRunABit != nil ) {
		threadSwitchRequest = false ;
		ThreadSwitchTo(restoreFromRunABit);
		restoreFromRunABit = nil;
	}
}

static void ThreadRunABitSwitch(ThreadPt th)	/* pre: th != active */
{
	if( th != active ) {
		if( restoreFromRunABit != nil )
			FatalError("ThreadRunABit is not reentrant") ;
		threadSwitchRequest = false ;
		restoreFromRunABit = active ;
		ThreadSwitchTo(th);
		OSAlarm(TIMER_PERIOD, timerAction);
	}
}

void ThreadRunABit(ThreadPt th)
{
//	Mesg("ThreadRunABit %s (%s)", AtomName(AliasSearch(th)), AtomName(AliasSearch(active)));
	if( ThreadInterrupted(th) ) {
		ThreadInterrupted(th) = false ;
		ThreadRunABitSwitch(th) ;
		ThreadInputReset() ;
		Throw(MakeAtom("abort")) ;
	}
	elif( !ThreadWaiting(th) )
		ThreadRunABitSwitch(th) ;
}

Bool ThreadWaitingInput(ThreadPt th)
{
	return ThreadWaiting(th);
}

void ThreadInput(ThreadPt th, Str line)
{
	StreamPutStr(ThreadUserIn(th), line);
	ThreadWaiting(th) = false;
}

Str ThreadOutputText(ThreadPt th)
{
	return BufferGetAllAndClear(ThreadBufferOut(th));
}

#define SEEN_DELIMITER '\0'

void ThreadInputGetCharReset(void)
{
	termStartPos = -1 ;
	ThreadRetries(active) = 0 ;
}

/* SEEN_DELIMITER and ThreadMarksSeen(active) are used to control
 * skipping text already processed by the term reader.
 */

WChar ThreadInputGetChar(void)
{
	extern Bool startOfReadingTerm ;
/* start of term */
	if( startOfReadingTerm ) {
/* save start pos to restore later, if necessary */
		if( termStartPos != -1 )
			FatalError("ThreadInputGetChar is not reentrant");
		termStartPos = BufferGetReadPos(ThreadBufferIn(active));
	}

start:
	if( cc == '\n' )
		InterLineWritePrompt(ThreadRetries(active) > 0) ;
	cc = BufferGetChar(ThreadBufferIn(active)) ;
	if( ThreadMarksSeen(active) == ThreadRetries(active) && cc != EOF ) {
		if( ThreadAutoOutput(active) )
			StreamPut(ThreadUserOut(active), cc);
		return cc;
	}
	elif( cc == SEEN_DELIMITER )
		{ ThreadMarksSeen(active)++; goto start; }
	elif( cc == '\n' ) /* already seen - should no generate prompt */
		{ cc = ' '; return '\n'; }

/* fire EOF */
	if( cc == EOF ) {
		if( termStartPos >= 0 ) {
			StreamPut(ThreadUserIn(active), SEEN_DELIMITER);
			BufferSetReadPos(ThreadBufferIn(active), termStartPos) ;
			ThreadMarksSeen(active) = 0 ;
		}
		termStartPos = -1 ;
		ThreadRetries(active)++ ;
		P--;
		ThreadWaiting(active) = true ;
		ThreadRestoreFromRunABit();
		EventContinue();
	}
	return cc ;
}

WChar ThreadInputPeekChar(void)
{
	WChar c = BufferPeekChar(ThreadBufferIn(active)) ;
	while( c == SEEN_DELIMITER )
		c = BufferGetChar(ThreadBufferIn(active)) ;
	return  c;
}

void ThreadRestart(ThreadPt th)
{
	ThreadInterrupted(th) = true ;
}

static void ThreadDebug(ThreadPt next)
{
	Mesg("%5d  %s->%s    calls=%d    cputime=%g", sws,
			AtomName(AliasSearch(active)), AtomName(AliasSearch(next)),
			callCount, CpuTime()) ;
	callCount = 0 ;
}

void ThreadConcurrencyHandle()
{
	if( restoreFromRunABit != nil ) {
		// must change. Runabit is not interruptable
		ThreadRestoreFromRunABit();
		return ;
	}
	ThreadPt next ;
	if( !concurrencyIsOn ) return ;
	threadSwitchRequest = false ;
	if( (next = ExtraGetNext(active)) == nil )
		next = ExtraGetFirst(threadType);
#if THREAD_DEBUG
	ThreadDebug(next) ;
#endif
	if( next != active ) {
		ThreadSwitchTo(next);
		sws++;
	//	if( sws == 10 ) OSStopTimer() ;
	}
}

void ThreadConcurrencyEnable()
{
	concurrencyIsOn = true ;
	OSStartTimer(TIMER_PERIOD, timerAction);
}

void ThreadConcurrencyDisable()
{
	concurrencyIsOn = false ;
	OSStopTimer();
}


/* STATISTICS */

static void AreaStatistics(Str name, Size space, Size used)
{
	Write("%30s: %7ldKB (%10ld bytes used)\n",
				name,
				WordsAsKBytes(space),
				WordsAsBytes(used)) ;
}

static void ThreadStatistics(ThreadPt th)
{
	if( th != root )
		Write("--- thread <%s> -------\n", ExtraAsStr(th)) ;
	AreaStatistics("Global stack + Local stack",
					ThreadStacksEnd(th) - ThreadStacksBegin(th),
					(ThreadReg(th,H) - ThreadStacksBegin(th))
						+ (ThreadStacksEnd(th)
						- Min(cHdl(ThreadReg(th,B)), cHdl(ThreadReg(th,E))))) ;
	AreaStatistics("Trail stack",
				ThreadTrailEnd(th) - ThreadTrailBegin(th),
				ThreadReg(th,TR) - ThreadTrailBegin(th)) ;
	doseq(th, ThreadSons(th), ThreadBrother(th))
		ThreadStatistics(th) ;
}

void StatisticsShow()
{
	ShowVersion() ;
	Write("STATISTICS:\n") ;
	AreaStatistics("Heap (program space)", ProgramSpaceSize(), ProgramSpaceUsed()) ;
	ThreadSave(active) ;	/* trailBegin, stacksBegin0 might have moved */
	ThreadStatistics(root) ;
	AreaStatistics("Scratch pads", ScratchCapacity()
							+ GStrCapacity() + BigStrCapacity(), 0) ;
	Write("%30s: %8g sec\n", "Total runtime", CpuTime()) ;
#if 0
	Write("\n") ;
	Write("%30s: %5ld\n", "Number of Units", CountUnits()) ;
	Write("%30s: %5ld\n", "Number of Predicates", CountPredicates()) ;
	Write("%30s: %5ld\n", "Number of Clauses", CountClauses()) ;
#endif
}


/* CXPROLOG C'BUILTINS */

static void PThreadCheck()
{
	MustBe( XExtraCheck(threadType, X0) ) ;
}

static void PThreadNew()
{
	BindVarWithExtra(X0, ThreadNew(Drf(X1), Drf(X2))) ;
	JumpNext() ;
}

static void PThreadTransfer()
{
	ThreadTransferTo(XTestExtra(threadType,X0), tGoingAtom) ;
	InternalError("PThreadTransfer") ;
}

static void PThreadKill()
{
	ThreadPt th = XTestExtra(threadType,X0) ;
	if( th == root )
		ImperativeError("The root thread cannot be killed") ;
	elif( th == active )
		ActiveThreadFinish(tKilledAtom) ;
	elif( IsAncestor(th, active) )
		ImperativeError("A thread cannot kill an ancestor thread") ;
	else ThreadDelete(th) ;
	JumpNext() ;
}

static void PActiveThread()
{
	MustBe( UnifyWithAtomic(X0, TagExtra(threadType, active)) ) ;
}

static void PThreadRunABit()
{
	ThreadRunABit(XTestExtra(threadType,X0)) ;
	JumpNext() ;
}

static void PThreadsConcurrency(void)
{
	callCount = 0;
	if( XTestBool(X0) )
		ThreadConcurrencyEnable() ;
	else
		ThreadConcurrencyDisable() ;
	JumpNext() ;
}

static void PThreadIncCallCounter(void)
{
	callCount++ ;
	JumpNext() ;
}

static void PActiveThreadSetSuccess()
{
	ThreadResult(active) = true ;
	JumpNext() ; ;
}

static void PActiveThreadReturn()
{
	ActiveThreadFinish(ThreadResult(active) ? tCompletedAtom : tFailedAtom) ;
	InternalError("PActiveThreadFailed") ;
}

static void PStatistics()
{
	StatisticsShow() ;
	JumpNext() ;
}

static void PStatistics2()
{
	register Str name = XTestAtomName(X0) ;
	if( StrEqual(name, "runtime") ) {
		static long past = 0 ;
		long curr = (long)(CpuTime() * 1000) ;
		if( Unify(X1,
			MakeList(MakeInt(curr),
				MakeList(MakeInt(curr - past), tNilAtom))) ) {
			past = curr ;
			JumpNext() ;
		}
	}
	DoFail() ;
}

static void PMallocStats()
{
	MallocStats() ;
	JumpNext() ;
}

static void PMaxMem()
{
	ZEnsureFreeSpaceOnStacks(128 K, -1, false) ; /* stacks grow now */
	JumpNext() ;
}

static void PNDCurrentThread()
{
	ExtraPNDCurrent(threadType, nil, 1, 0) ;
	JumpNext() ;
}

static void ThreadsShow(register ThreadPt th, int n)
{
	int i ;
	dotimes(i, n) Write("  ") ;
	Write("%16s", ExtraAsStr(th)) ;
	AliasedWithWrite(TagExtra(threadType, th)) ;
	IVarsWithWrite(TagExtra(threadType, th)) ;
	Write("\n") ;
	doseq(th, ThreadSons(th), ThreadBrother(th))
		ThreadsShow(th, n+1) ;
}
static void PThreads()
{
	ShowVersion() ;
	Write("THREADS:\n") ;
	ThreadsShow(root, 1) ;
	JumpNext() ;
}

void ThreadsInit()
{
	threadType = ExtraTypeNew("THREAD", ThreadSizeFun, nil, nil, 1) ;

	NotRunningAddr = Allocate(1, false) ;
	NotRunningAddr[0] = Fail ;
	
	InstallCBuiltinPred("thread", 1, PThreadCheck) ;
	InstallCBuiltinPred("thread_new", 3, PThreadNew) ;
	InstallCBuiltinPred("thread_transfer", 2, PThreadTransfer) ;
	InstallCBuiltinPred("thread_kill", 1, PThreadKill) ;
	InstallCBuiltinPred("active_thread", 1, PActiveThread) ;
	InstallCBuiltinPred("thread_run_a_bit", 1, PThreadRunABit) ;
	InstallCBuiltinPred("threads_concurrency", 1, PThreadsConcurrency) ;
	InstallCBuiltinPred("$$_active_thread_set_success", 0, PActiveThreadSetSuccess) ;
	InstallCBuiltinPred("$$_active_thread_return", 0, PActiveThreadReturn) ;
	InstallCBuiltinPred("$$_thread_inc_call_counter", 0, PThreadIncCallCounter) ;
	InstallCBuiltinPred("statistics", 0, PStatistics) ;
	InstallCBuiltinPred("statistics", 2, PStatistics2) ;
	InstallCBuiltinPred("malloc_stats", 0, PMallocStats) ;
	InstallCBuiltinPred("$$_max_mem", 0, PMaxMem) ;
	InstallGNDeterCBuiltinPred("current_thread", 1, 2, PNDCurrentThread) ;
	InstallCBuiltinPred("threads", 0, PThreads) ;
}
