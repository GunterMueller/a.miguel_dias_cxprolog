/*
 *   This file is part of the CxProlog system

 *   OS.h
 *   by A.Miguel Dias - 2002/01/19
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _OS_
#define _OS_

/* INTERRUPT */
void AllowInterruptibleSysCalls(void) ;
void DisallowInterruptibleSysCalls(void) ;

/* FILESYS */
Bool OSExists(CharPt fname) ;
Bool OSMkdir(CharPt fname) ;
Pt OSPropType(CharPt fname) ;
Pt OSPropSize(CharPt fname) ;
Pt OSPropReadable(CharPt fname) ;
Pt OSPropTime(CharPt fname) ;
CharPt OSGetCurrDir(void) ;
Bool OSSetCurrDir(CharPt s) ;
Pt OSFiles(void) ;
CharPt OSApplDir(void) ;
void OSFileSysInit(void) ;

/* SPECIAL I/O INPUT */
Bool SetRawInput(FILE *file) ;
void UnsetRawInput(void) ;
Bool OSIsATty(int fd) ;

/* READLINE */
CharPt OSReadline(CharPt prompt) ;	

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
CharPt OSGetEnv(CharPt envVarName, Bool err) ;
void OSSetEnv(CharPt envVarName, CharPt newValue, Bool err) ;
CharPt OSPathSeparator(void) ;
CharPt OSGetUserHome(CharPt username, Bool err) ;

/* OTHER */
Bool CreateConsole(void) ;
void DeleteConsole(void) ;
CharPt OSName(void) ;

#endif
