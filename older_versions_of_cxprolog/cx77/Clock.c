/*
 *   This file is part of the CxProlog system

 *   Clock.c
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"

#define clockDef		1

#if clockDef == 1
	#include <time.h>
	#define ClockRate	(double)CLOCKS_PER_SEC
	static clock_t startClock ;
#endif
#if clockDef == 2
	#include <sys/types.h>
	#include <sys/times.h>
	#include <sys/stat.h>
	#define ClockRate	60.0
#endif

void InitTime()
{
#if clockDef == 1
	startClock = clock() ;
#endif
}
	
double CurrTime()
{
#if clockDef == 1
	return ( clock() - startClock ) / (double)CLOCKS_PER_SEC ;
#endif
#if clockDef == 2
	struct tms theTime ;

	times(&theTime) ;
	return ((double)theTime.tms_utime)/ClockRate ;
#endif
}

