/*
 *   This file is part of the CxProlog system

 *   OSUnknown.c
 *   by A.Miguel Dias - 2001/06/04
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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

#ifdef unknownOS

/* INTERRUPT */

#include <signal.h>

static void IHandler(int a)
{
	signal(SIGINT, SIG_IGN) ;
	Interrupted() = true ;
	Attention() = true ;
	signal(SIGINT, IHandler) ;
}

void InterruptOff()
{
	signal(SIGINT, SIG_IGN) ;
}

void InterruptOn()
{
	signal(SIGINT, IHandler) ;
}


/* FILESYS */

/* FSExists: Also requires that the file is readable, which is
    too big a requirement. However, this is the best that we can
    do using "stdio.h" alone */ 
Bool FSExists(CharPt fname)
{
	FILE *f;
	if( (f = fopen(fname, "r")) != nil ) {
		fclose(f) ;
		return true ;
	}
	return false ;
}

Bool FSRen(CharPt oname, CharPt nname)
{
	return rename(oname, nname) == 0 ;
}

Bool FSDel(CharPt fname)
{
	return remove(fname) == 0 ;
}

Pt FSPropType(CharPt fname)
{
	return FSExists(fname) ? tNilAtom : nil ;
}

Pt FSPropReadable(CharPt fname)
{
	return FSExists(fname) ? tTrueAtom : tFalseAtom ;
}

Pt FSGetCurrDir()
{
	return tNilAtom ;
}

Bool FSSetCurrDir(Pt t)
{
	return true ;
}

void FSGoHome()
{
}

Pt FSFiles()
{
	return tNilAtom ;
}


/* PROCESSES */

CharPt OSName()
{
	return "unknown" ;
}

Bool OSRun(CharPt fname)
{
	return false ;
}


/* INIT */

void OSDependentInit()
{
}

#endif
