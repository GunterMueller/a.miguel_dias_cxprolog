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

void UnixArgsInit(int ac, CharPt av[]) ;
void SpecifyBootFile(CharPt boot) ;
CharPt UnixGetArg(CharPt sw) ;
CharPt UnixGetEnv(CharPt var) ;
Bool UnixRun(CharPt command) ;
void UnixServicesInit(void) ;

#endif
