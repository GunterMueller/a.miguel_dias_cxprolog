/*
 *   This file is part of the CxProlog system

 *   XAppl.h
 *   by A.Miguel Dias - 1997/05/10
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

void XSetPrompt(CharPt pr) ;
void XSetTimeOut(int to) ;

void XSend(CharPt command) ;
void XReceive(CharPt buf, int bufSize) ;
void XReceiveP(CharPt buf, int bufSize, char *tempPrompt, int timeout) ;

Bool XLaunch(CharPt xappl_path) ;
void XClose(void) ;
