/*
 *   This file is part of the CxProlog system

 *   OSDependencies.h
 *   by A.Miguel Dias - 2001/06/04
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _OS_
#define _OS_

#ifndef unix
#define mac		1
#endif


/* INTERRUPT */

void InterruptOff(void) ;
void InterruptOn(void) ;


/* FILESYS */

Bool FSExists(CharPt fname) ;
Bool FSRen(CharPt oname, CharPt nname) ;
Bool FSDel(CharPt fname) ;
Pt FSPropType(CharPt fname) ;
Pt FSPropReadable(CharPt fname) ;
Pt FSGetCurrDir(void) ;
Bool FSSetCurrDir(Pt t) ;
void FSGoHome(void) ;
Pt FSFiles(void) ;


/* PROCESSES */

CharPt OSName(void) ;
Bool OSRun(CharPt fname) ;
CharPt OSGetEnv(CharPt var) ;
void SpecifyBootFile(CharPt boot) ;
void OSArgsInit(int ac, CharPt av[]) ;
CharPt OSGetArg(CharPt sw) ;


/* INIT */

void OSDependentInit(void) ;
void OSInit(void) ;

#endif
