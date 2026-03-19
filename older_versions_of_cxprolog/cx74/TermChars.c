/*
 *   This file is part of the CxProlog system

 *   TermChars.c
 *   by A.Miguel Dias - 1997/08/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"

/* WARNING: Forbiden changes:
		- changes involving _DG
		- changes involving chars that represent themselves
		- '.' and '/' must remain _SY 
		- 'sub' must remain _EF 
		
	Any other change seems to be safe (albeit probably unnecessary.)
*/

CharCategories allChars[256] = {

/* nul  soh  stx  etx  eot  enq  ack  bel   bs   ht   nl   vt   np   cr   so   si */
   __I, __I, __I, __I, __I, __I, __I, __I, _BL, _BL, _BL, __I, __I, _BL, __I, __I,

/* dle  dc1  dc2  dc3  dc4  nak  syn  etb  can   em  sub  esc   fs   gs   rs   us */
   __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, _EF, __I, __I, __I, __I, __I,

/*  sp    !   "     #   $    %     &   '    (    )     *    +   ,     -    .    / */
   _BL, _SO,'\"', _SY, _SY, '%', _SY,'\'', '(', ')', _SY, _SY, ',', _SY, _SY, _SY,

/*   0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ? */
   _DG, _DG, _DG, _DG, _DG, _DG, _DG, _DG, _DG, _DG, _SY, _SY, _SY, _SY, _SY, _SY,

/*   @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O */
   _SY, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC,

/*   P    Q    R    S    T    U    V    W    X    Y    Z   [     \   ]    ^     _ */
   _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, '[', _SY, ']', _SY, _UC,

/*   `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o */
   '`', _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC,

/*   p    q    r    s    t    u    v    w    x    y    z   {    |    }     ~  del */
   _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, '{', '|', '}', _SY, __I,
  
/* 128       130                                               140                */
   __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I,

/* 144                           150                                              */
   __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I, __I,

/* 160                                               170                           */
   _BL, _SY, _SY, _SY, _SY, _SY, _SY, _SY, _SY, _SY, _LC, _SY, _SY, _SY, _SY, _SY,

/* 176                 180                                               190      */
   _SY, _SY, _SY, _SY, _SY, _SY, _SY, _SY, _SY, _SY, _LC, _SY, _SY, _SY, _SY, _SY,

/* 192                                     200                                    */
   _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC, _UC,

/* 208       210                                               220                */
   _SY, _UC, _UC, _UC, _UC, _UC, _UC, _SY, _SY, _UC, _UC, _UC, _UC, _SY, _SY, _SY,

/* 224                           230                                              */
   _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC, _LC,

/* 240                                               250                      255 */
   _SY, _LC, _LC, _LC, _LC, _LC, _LC, _SY, _SY, _LC, _LC, _LC, _LC, _SY, _SY, _LC
  
} ;

void NullIsEof()
{
	allChars[0] = _EF ;
}


void NullIsNotEof()
{
	allChars[0] = __I ;
}

