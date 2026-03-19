/*
 *   This file is part of the CxProlog system

 *   Clock.c
 *   by A.Miguel Dias - 2000/04/02
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
#include <time.h>

static clock_t startClock ;

void ClockInit()
{
	startClock = clock() ;
}

double CpuTime()
{
	return ( clock() - startClock ) / (double)CLOCKS_PER_SEC ;
}

#if 0
static void DayTime()
{
	time_t t ;
	struct tm *tm ;
		
	time(&t) ;
	tm = localtime(&t) ;

            struct tm {
                      int     tm_sec;         /* seconds */
                      int     tm_min;         /* minutes */
                      int     tm_hour;        /* hours */
                      int     tm_mday;        /* day of the month */
                      int     tm_mon;         /* month */
                      int     tm_year;        /* year */
                      int     tm_wday;        /* day of the week */
                      int     tm_yday;        /* day in the year */
                      int     tm_isdst;       /* daylight saving time */
              };

	
}
#endif
