/*
 *   This file is part of the NanoProlog system

 *   Util.c
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

#include "Util.h"


void *Align(void *pt) ;
static void *Align(void *pt)
{
	CharPt p = pt ;
	
	while( cInt(p) % sizeof(Word) != 0 ) p++ ;
	return( p ) ;
}

void *AllocAligned(long bytes)
{
	CharPt mem ;

	if( (mem = malloc(bytes + sizeof(Word))) == nil ) {
		fprintf(stderr, "Error: Not enough memory!\n") ;
		exit( 1 ) ;
	}
	return( Align(mem) ) ;
}
