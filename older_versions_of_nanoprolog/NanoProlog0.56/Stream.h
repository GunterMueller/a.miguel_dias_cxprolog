/*
 *   This file is part of the NanoProlog system

 *   Stream.h
 *   by A.Miguel Dias - 89/12/2
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

#ifndef _Stream_
#define _Stream_

#ifndef _Util_
#include "Util.h"
#endif

#define dflInputStream	0
#define dflOutputStream 1

void InitStreams(void) ;
void RecoverStreams(void) ;
void CloseAllStreams(void) ;
void Flush(char *name) ;
void Seen(void) ;
void Told(void) ;
char *Seeing(void) ;
char *Telling(void) ;
void See(char *name) ;
void Tell(char *name) ;
void Put(int c) ;
void PutString(char *s) ;
void Nl(void) ;
void Tab(int n) ;
void Prompt(char *s) ;
int Get0(void) ;
int Get(void) ;
void GetString(char *buff, char *sw) ;
void Skip(int c) ;
int Peek0(void) ;
void SetTempOutput(int id) ;
void RestoreOutput(void) ;

#endif
