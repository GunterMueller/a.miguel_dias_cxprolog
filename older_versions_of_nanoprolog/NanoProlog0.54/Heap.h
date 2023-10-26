/*
 *   This file is part of the NanoProlog system

 *   Heap.h
 *   by A.Miguel Dias - 89/11/14
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

#ifndef _Heap_
#define _Heap_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif

#define NoSpace(n)			Error("%s overflow", n)

#define GrowArea(c,e,w,n)	{	if( ((c) += (w)) > (e) )	\
								{	(c) -= (w) ; NoSpace(n) ; } }

#define GrowAreaR(c,b,w,n) {	if( ((c) -= (w)) < (b) )	\
								{	(c) += (w) ; NoSpace(n) ; }	}

void AreaStatistics(char *pre, char *name, long space, long used) ;

void CreateHeap(long size) ;
void *HeapAlloc(int nWords, Bool permanent) ;
void HeapFree(void *ptr) ;
long HeapUsed(void) ;
void HeapStatistics(void) ;

#endif