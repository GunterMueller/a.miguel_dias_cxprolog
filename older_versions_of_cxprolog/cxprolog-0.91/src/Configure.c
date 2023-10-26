/*
 *   This file is part of the CxProlog system

 *   Configure.c
 *   by A.Miguel Dias - 2004/07/24
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

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#if __APPLE__
#include <malloc/malloc.h>
#else
#include <malloc.h>
#endif
#include "Util.h"
#include "Atom.h"
#include "Number.h"
#include "Extra.h"
#define BACKWARDS 0
#include "Term.h"

static void Bye(void) {
	exit(1) ;
}

static void HandleWarning(FILE *out)
{
	fprintf(out, "/*\n") ;
	fprintf(out, " * This is an automatically generated file\n") ;
	fprintf(out, " * belonging to the CxProlog system\n") ;
	fprintf(out, " */\n") ;
	fprintf(out, "\n") ;
}

static void HandleBackwards(FILE *out)
{
	CharPt mem ;
	if( (mem = malloc(WordsAsBytes(256 K))) == nil ) Bye() ;
	fprintf(out, "#define BACKWARDS\t%d\n\n", GetTag(mem) != tagVar) ;
}

int main(int argc, char **argv)
{
	FILE *out ;
	if( (out = fopen("Configure.h", "w")) == nil ) Bye() ;
	HandleWarning(out) ;
	HandleBackwards(out) ;
	fclose(out) ;
	return 0 ;
}
