/*
 *   This file is part of the NanoProlog system

 *   Thread.h
 *   by A.Miguel Dias - 93/7/15
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931117: release of version 0.5

*/

#ifndef _Thread_
#define _Thread_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif
							
#define GrowGlobal(w)		GrowArea(H, stacksEnd, w, "Stacks")
#define TestTrailOverflow()	if( TR >= trailEnd) NoSpace("Trail") ;
#define TestOverflow(lTop)	if( cHdl(lTop) - 512 < H ) NoSpace("Stack")

typedef struct Thread
{
	AtomPt name, module, goal, rgoal ;
	Hdl stacksBegin, stacksEnd, trailBegin, trailEnd ;
	Hdl P, CP, H, TR ;
	EnvironmentPt E ;
	ChoicePointPt B ;
	Pt X[maxX] ;
	struct Thread *father, *sons, *brother ;
} Thread, *ThreadPt ;

#define	cThreadPt(t)				((ThreadPt)(t))

ThreadPt CreateThread(AtomPt name, long size,
							AtomPt module, AtomPt goal, AtomPt rgoal) ;
ThreadPt LookupThread(AtomPt name) ;
void TransferToThread(ThreadPt th) ;
void RestartCurrThread(void) ;
void KillThread(ThreadPt th) ;
void FinishCurrThread(char *res) ;
AtomPt GetCurrThreadName(void) ;
void Statistics(void) ;

#endif