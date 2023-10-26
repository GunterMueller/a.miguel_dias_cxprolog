/*
 *   This file is part of the CxProlog system

 *   UnixServices.h
 *   by A.Miguel Dias - 2000/08/03
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _UnixServices_
#define _UnixServices_

void InitArgs(int ac, CharPt av[]) ;
CharPt GetArg(CharPt sw) ;
CharPt GetEnv(CharPt var) ;
Bool UnixRun(CharPt command) ;
void InitUnixServices(void) ;

#endif
