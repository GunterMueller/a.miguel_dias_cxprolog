/*
 *   This file is part of the CxProlog system

 *   OS.h
 *   by A.Miguel Dias - 2002/01/19
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
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
Bool OSExists(CharPt fname) ;
Bool OSRen(CharPt oname, CharPt nname) ;
Bool OSDel(CharPt fname) ;
Pt OSPropType(CharPt fname) ;
Pt OSPropReadable(CharPt fname) ;
Pt OSGetCurrDir(void) ;
Bool OSSetCurrDir(Pt t) ;
void OSGoHome(void) ;
Pt OSFiles(void) ;
void OSFileSysInit(void) ;

/* PROCESSES */
int OSFork(void) ;
void OSKill(int pid) ;
int OSGetPid(void) ;
void OSSleep(Size secs) ;
Bool OSRun(CharPt fname) ;
void OSPipe(int *fd) ;
int OSPipeBufferSize(void) ;
void OSWrite(int fd, VoidPt buf, Size size) ;
Bool OSRead(int fd, VoidPt buf, Size size, Bool blocking) ;
CharPt OSGetEnv(CharPt var) ;

/* OTHER */
CharPt OSName(void) ;

#endif
