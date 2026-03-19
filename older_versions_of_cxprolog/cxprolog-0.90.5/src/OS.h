/*
 *   This file is part of the CxProlog system

 *   OS.h
 *   by A.Miguel Dias - 2002/01/19
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _OS_
#define _OS_

/* INTERRUPT */
void InterruptOff(void) ;
void InterruptOn(void) ;
void AllowInterruptibleSysCalls(void) ;
void DisallowInterruptibleSysCalls(void) ;

/* FILESYS */
Pt OSPathNameFromStr(CharPt str) ;
CharPt OSPathNameToStr(Pt list) ;
Bool OSExists(CharPt fname) ;
Bool OSRen(CharPt oname, CharPt nname) ;
Bool OSDel(CharPt fname) ;
Pt OSPropType(CharPt fname) ;
Pt OSPropSize(CharPt fname) ;
Pt OSPropReadable(CharPt fname) ;
Pt OSPropTime(CharPt fname) ;
Pt OSGetCurrDir(void) ;
Bool OSSetCurrDir(Pt t) ;
void OSGoHome(void) ;
Pt OSFiles(void) ;
void OSFileSysInit(void) ;

/* RAW INPUT */
Bool SetRawInput(StreamPt srm) ;
void UnsetRawInput(void) ;

/* SOCKETS */
int OSInstallServer(int port, int queueLen)	;
void OSAccept(int server, FILE **r, FILE **w) ;
void OSUninstallServer(int server) ;
void OSConnect(CharPt host, int port, FILE **r, FILE **w) ;
PInt OSEncodeInt(PInt i) ;
PInt OSDecodeInt(PInt i) ;

/* PROCESSES */
int OSFork(void) ;
void OSWait(void) ;
void OSKill(int pid) ;
int OSGetPid(void) ;
void OSSleep(Size secs) ;
Bool OSRun(CharPt command) ;
void OSPipe(int *fd) ;
int OSPipeBufferSize(void) ;
void OSWrite(int fd, VoidPt buf, Size size) ;
Bool OSRead(int fd, VoidPt buf, Size size, Bool blocking) ;
CharPt OSGetEnv(CharPt envVarName) ;

/* OTHER */
CharPt OSName(void) ;

#endif
