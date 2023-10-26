/*
 *   This file is part of the NanoProlog system

 *   PlStrings.c
 *   by A.Miguel Dias - 97/05/10
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990,...,1997 A.Miguel Dias, GLOC, DI/FCT/UNL

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
 970520: first version
*/

#include "NanoProlog.h"

Pt StrRead(void)
{
	if( fgets(cCharPt(codeBuff), maxCodePerClause Wd, stdin) == nil )
		Error("reading string") ;
	return( BufferToString(cCharPt(codeBuff)) ) ;
}

void StrWrite(Pt t)
{
	StringToBuffer(t, cCharPt(codeBuff), maxCodePerClause Wd) ;
	printf("%s", cCharPt(codeBuff)) ;
}
