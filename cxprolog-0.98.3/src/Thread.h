/*
 *   This file is part of the CxProlog system

 *   Thread.h
 *   by A.Miguel Dias - 1993/07/15
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Thread_
#define _Thread_

typedef struct Thread *ThreadPt ;
typedef struct Thread *PrologInstancePt ;

#define endOfChainMark			tNilAtom
#define IsEndOfChain(pt)		(*cHdl(pt) == endOfChainMark)

void ThreadRootCreate(Pt startGoal, Pt restartGoal) ;
void ActiveThreadReplace(Pt startGoal, Pt restartGoal) ;
void ActiveThreadReset(void) ;
void ActiveThreadStart(void) ;
void ActiveThreadRestart(void) ;
void ThreadSwitchToRoot(void) ;

ThreadPt ThreadByName(Str alias) ;
Bool ThreadActiveIsRoot(void) ;

void ThreadConcurrencyHandle(void) ;
void ThreadConcurrencyEnable(void) ;
void ThreadConcurrencyDisable(void) ;

void ThreadSetAutoOutput(ThreadPt th, Bool ao) ;
void ThreadRunABit(ThreadPt th) ;
void ThreadInput(ThreadPt th, Str line) ;
Str ThreadOutputText(ThreadPt th) ;
void ThreadInputGetCharReset(void);
WChar ThreadInputGetChar(void);
WChar ThreadInputPeekChar(void);
void ThreadRestart(ThreadPt th);

Bool ThreadWaitingInput(ThreadPt th) ;

void StatisticsShow(void) ;
void ThreadsInit(void) ;

#endif
