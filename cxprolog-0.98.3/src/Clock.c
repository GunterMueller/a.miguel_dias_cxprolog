/*
 *   This file is part of the CxProlog system

 *   Clock.c
 *   by A.Miguel Dias - 2000/04/02
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
#include <time.h>


/* RANDOM */

PFloat FloatRandom(void)	/* 0.0 < res < 1.0 */
{
	return (cPFloat(rand()) + 1) / (cPFloat(RAND_MAX) + 2);
}

PInt IntRandom(PInt range)	/* pre: range > 0 */
{
	return rand() % range ;
}


/* CLOCK */

static clock_t startClock ;

long AbsoluteTime(void)
{
	return time(nil) ;
}

double CpuTime(void)
{
	return (clock() - startClock) / (double)CLOCKS_PER_SEC ;
}

void ClockInit(void)
{
	startClock = clock() ;
	srand(time(nil)) ;
}
