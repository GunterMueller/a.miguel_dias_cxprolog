/*
 *   This file is part of the NanoProlog system

 *   ThreadedCode.h
 *   by A.Miguel Dias - 93/11/20
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1993 A.Miguel Dias and DI/FCT/UNL

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

 931120: file introduced in version 0.51

*/


#if threadedCode == 1

#ifdef Mac
#define	JumpNext()	{ asm{Move.l P,A0}; asm{Add.l #4,P}\
					  asm{Movea.l (A0),A0}; asm{Jmp (A0)}}
#define	Jump(p)		{ asm{Movea.l p,A0}; asm{Jmp (A0)} }
#define Run()		(*cProc(*P++))()
#define Z(p)		cPt(*cPt(cWord(p) + 2))
#endif

#ifdef Vax
#define JumpNext()	asm("jmp *(r8)+") ;
#define Jump(p)		asm("movl %0, r0; jmp (r0)"::"g"(p):"r0") ;
#define Run()		(*cProc(cWord(*P++)-2))()
#define Z(p)		cPt(cWord(p)+2)
#endif

#endif



/* Default: no threaded-code */
#ifndef JumpNext
#define	JumpNext()	return ;
#define Jump(v)		{ (*cProc(v))() ; JumpNext() }
#define Run()		for(;;) (*cProc(*P++))()
#define Z(p)		cPt(p)
#endif
