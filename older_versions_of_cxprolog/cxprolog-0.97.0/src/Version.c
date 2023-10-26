/*
 *   This file is part of the CxProlog system

 *   Version.c
 *   by A.Miguel Dias - 2004/06/28
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL

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

static Str32 applName = "XXXX" ;
static Str32 version = "?.?" ;
static Str32 subVersion = "" ;
static Str32 revision = "" ;
static int variant = -1 ;

void SetVersion(CharPt a, CharPt v, CharPt s, CharPt r, int z)
{
	if( a != nil ) strncpy(applName, a, 30) ;
	if( v != nil ) strncpy(version, v, 30) ;
	if( s != nil ) strncpy(subVersion, s, 30) ;
	if( r != nil ) strncpy(revision, r, 30) ;
	if( z != -1 ) variant = z ;
}

void Version(CharPt *a, CharPt *v, CharPt *s, CharPt *r, int *z)
{
	if( a != nil ) *a = applName ;
	if( v != nil ) *v = version ;
	if( s != nil ) *s = subVersion ;
	if( r != nil ) *r = revision ;
	if( z != nil ) *z = variant ;
}

CharPt VersionString()
{
	CharPt s = GStrFormat("%s version %s", applName, version) ;
	if( !StrEqual(subVersion, "") )
		s = GStrFormat("%s.%s", s, subVersion) ;
	if( !StrEqual(revision, "") )
		s = GStrFormat("%s (revision: %s)", s, revision) ;
	if( variant != -1 && variant != 1 )
		s = GStrFormat("%s [variant: %d]", s, variant) ;
	return s ;
}

void ShowVersion()
{
	Write("%s\n", VersionString()) ;
}


/* CXPROLOG C'BUILTINS */

static void PVersion()
{
	Write("%s\n", VersionString()) ;
	JumpNext() ;
}

static void PVersion1()
{
	MustBe( UnifyWithAtomic(X0, MakeAtom(VersionString()))) ;
	JumpNext() ;
}

void VersionInit()
{
	InstallCBuiltinPred("version", 0, PVersion) ;
	InstallCBuiltinPred("version", 1, PVersion1) ;
}
