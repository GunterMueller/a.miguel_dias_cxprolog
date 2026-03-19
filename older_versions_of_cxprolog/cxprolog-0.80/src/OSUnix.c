/*
 *   This file is part of the CxProlog system

 *   OSUnix.c
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

#ifdef unix


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
	siginterrupt(SIGINT, 1) ;
}


/* FILESYS */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

static CharPt homePath ;

static CharPt GetElem(CharPt str)
{
	if( *str == '\0' ) return nil ;
	for( ; *str != '/' && *str != '\0' ; *str++ ) ;
	if( *str == '/' ) *str++ = '\0' ;
	return str ;
}

static Pt UnixPathToList(CharPt str)
{
	CharPt b, a ;
	Pt list = tNilAtom ;
	if( str[0] != '/' ) return nil ;
	for( a = b = str + 1 ; (b = GetElem(a)) != nil ; a = b ) {
		if( *a == '\0' ) return nil ;
		list = MakeBinStruct(listFunctor, MakeTempAtom(a), list) ;
	}
	return list ;
}

static CharPt ListToUnixPath(Pt list, CharPt str, Size size)
{
	Size len ;
	Hdl h = ListToArray(list, &len) ;
	if( size < 4 ) return nil ;
	strcpy(str, len == 0 ? "/" : "") ;
	size-- ;
	while( len-- ) {
		CharPt s = XTestAtomName(h[len]) ;
		if( (size -= strlen(s) + 1) < 1 ) return nil ;
		strcat(str, "/") ;
		strcat(str, s) ;
	}
	return str ;
}

Bool FSExists(CharPt fname)
{
	return access(fname, 0) == 0 ;
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
	struct stat statbuf ;
	if( stat(fname, &statbuf) != 0 ) return nil ;
	return statbuf.st_mode & S_IFDIR
				? MakeAtom("dir")
				: MakeAtom("file") ;
}

Pt FSPropReadable(CharPt fname)
{
	struct stat statbuf ;
	if( stat(fname, &statbuf) != 0 ) return nil ;
	if( statbuf.st_mode & S_IFDIR ) return tFalseAtom ;
	return access(fname, 0) == 0
				? tTrueAtom
				: tFalseAtom ;
}

Pt FSGetCurrDir()
{
	Str2000 str ;
	if( getcwd(str, 2000) == nil ) return nil ;
	return UnixPathToList(str) ;
}

Bool FSSetCurrDir(Pt t)
{
	Str2000 str ;
	if( ListToUnixPath(t, str, 2000) == nil ) return false ;
	return chdir(str) == 0 ;
}

void FSGoHome()
{
	chdir(homePath) ;
}

Pt FSFiles()
{
	DIR *dir ;
	struct direct *dp ;
	Pt list = tNilAtom ;
	Hdl h = &list + 1 ;
	if( (dir = opendir(".")) == nil ) return nil ;
   	while( (dp = readdir(dir)) != nil ) {
		h[-1] = MakeBinStruct(listFunctor,
						MakeTempAtom(dp->d_name), tNilAtom) ;
		h = H ;
   	}
	closedir(dir) ;
	return list ;
}


/* PROCESSES */

CharPt OSName()
{
	return "unix" ;
}

Bool OSRun(CharPt fname)
{
	return system(fname) == 0 ;
}


/* INIT */

void OSDependentInit()
{
	Str2000 str ;
	if( getcwd(str, 2000) == nil )
		FileError("Couldn't get current directory") ;
	homePath = PermBlockAllocate(Words(strlen(str) + 1)) ;
	strcpy(homePath, str) ;
}

#endif
