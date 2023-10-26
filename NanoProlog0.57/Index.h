/*
 *   This file is part of the NanoProlog system

 *   Index.h
 *   by A.Miguel Dias - 90/1/26
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

#ifndef _Index_
#define _Index_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif

#define clauseHeaderSize		3

ClausePt InstallClause(PredicatePt pr, Hdl code, int codeSize,
					Pt term, Bool end) ;
void DeleteCode(ClausePt cl) ;
void PatchPredicates(void) ;

void InitIndex(void) ;
void NoIndex(PredicatePt pr) ;
void InvalidIndex(PredicatePt pr) ;
void DoIndex(PredicatePt pr) ;

#endif