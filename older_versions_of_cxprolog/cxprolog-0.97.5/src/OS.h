/*
 *   This file is part of the CxProlog system

 *   OS.h
 *   by A.Miguel Dias - 2002/01/19
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2010 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _OS_
#define _OS_

/* INTERRUPT */
void OSAllowInterruptibleSysCalls(void) ;
void OSDisallowInterruptibleSysCalls(void) ;
void OSHandleRunInterruptibleFun(void) ;
void OSRunInterruptibleFun(Fun fun, FunI InterrHandler, BFun PostInterrHandler) ;

/* FILESYS */
Bool OSExists(Str fname) ;
Bool OSMkdir(Str fname) ;
Pt OSPropType(Str fname) ;
Pt OSPropSize(Str fname) ;
Bool OSPropReadable(Str fname) ;
Bool OSPropWritable(Str fname) ;
Pt OSPropTime(Str fname) ;
CharPt OSGetCurrDir(void) ;
Bool OSSetCurrDir(Str s) ;
Pt OSFiles(void) ;
CharPt OSApplDir(void) ;
CharPt OSPrefixDir(void) ;
void OSFileSysInit(void) ;

/* SPECIAL I/O INPUT */
Bool SetRawInput(FILE *file) ;
void UnsetRawInput(void) ;
Bool OSIsATty(FILE *file) ;

/* SOCKETS */
int OSInstallServer(int port, int queueLen) ;
void OSAccept(int server, FILE **r, FILE **w) ;
void OSUninstallServer(int server) ;
void OSConnect(Str host, int port, FILE **r, FILE **w) ;
PInt OSEncodeInt(PInt i) ;
PInt OSDecodeInt(PInt i) ;

/* PROCESSES/THREADS */
long OSGetPid(void) ;
long OSGetPPid(void) ;
long OSGetTid(void) ;
void OSBlockSignals(void) ;
long OSFork(void) ;
void OSWait(void) ;
void OSKill(int pid) ;
void OSSleep(Size secs) ;
Bool OSRun(Str command) ;
void OSPipe(int *fd) ;
long OSPipeBufferSize(void) ;
void OSWrite(int fd, VoidPt buf, Size size) ;
Bool OSRead(int fd, VoidPt buf, Size size, Bool blocking) ;
CharPt OSGetEnv(Str envVarName, Bool err) ;
void OSSetEnv(Str envVarName, Str newValue, Bool err) ;
CharPt OSPathSeparator(void) ;
CharPt OSGetUserPath(Str username, Bool err) ;
CharPt OSGetCachePath(Bool err) ;
CharPt OSGetPreferencesPath(Bool err) ;
CharPt OSGetTmpPath(Bool err) ;

/* OTHER */
Bool CreateConsole(void) ;
void DeleteConsole(void) ;
CharPt OSName(void) ;

#endif
