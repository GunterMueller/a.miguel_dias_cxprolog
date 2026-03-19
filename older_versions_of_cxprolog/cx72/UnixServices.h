/*
 *   This file is part of the CxProlog system

 *   UnixServices.h
 *   by A.Miguel Dias - 2000/08/03
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _UnixServices_
#define _UnixServices_

Bool GetArgvSwitch(int argc, CharPt argv[], CharPt sw) ;
CharPt GetArgvArg(int argc, CharPt argv[], CharPt sw) ;
void PostArgvCheck(int argc, CharPt argv[]) ;
Bool UnixRun(CharPt command) ;
void InitUnixServices(void) ;

#endif
