/*
 *   This file is part of the CxProlog system

 *   OSMac.c
 *   by A.Miguel Dias - 2001/06/04
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "CxProlog.h"

#ifdef mac


/* INTERRUPT */

#include <signal.h>

static void IHandler(int a)
{
	signal(SIGINT, SIG_IGN) ;
	Interrupted() = true ;
	Attention() = true ;
	signal(SIGINT, IHandler) ;
}

void InterruptOff()
{
	signal(SIGINT, SIG_IGN) ;
}

void InterruptOn()
{
	signal(SIGINT, IHandler) ;
}


/* FILESYS */

#undef nil
#undef false
#undef true
#include <Files.h>

static Bool atVirtualRoot ;
static short vRefNum, vRefNumHome ;
static long dirID, dirIDHome ;

static void C2P(CharPt str, Str255 buff)
{
	strncpy(cCharPt(buff+1), str, 255) ;
	buff[0] = strlen(str) ;
}

static Bool GetVolByName(CharPt vname, short *r)
{
	HParamBlockRec pb ;
	Str255 buff ;
	int i ;
	pb.volumeParam.ioNamePtr = buff ;
	for( i = 1 ; ; i++ ) {
		pb.volumeParam.ioVolIndex = i ;
		if( PBHGetVInfoSync(&pb) != noErr ) return false ;
		buff[buff[0]+1] = '\0' ;
		if( r != nil )
			*r = pb.volumeParam.ioVRefNum ;
		if( SimilarStr(vname, cCharPt(buff+1), 9999, true) )
			return true ;
	}
	return false ;
}

static Pt GetVolByIdx(int i)
{
	HParamBlockRec pb ;
	Str255 buff ;
	pb.volumeParam.ioNamePtr = buff ;
	pb.volumeParam.ioVolIndex = i ;
	if( PBHGetVInfoSync(&pb) != noErr ) return nil ;
	buff[buff[0]+1] = '\0' ;
	return MakeTempAtom(cCharPt(buff+1)) ;
}

static Bool VolRename(CharPt oname, CharPt nname)
{
	HParamBlockRec pb ;
	short ref ;
	Str255 buff ;
	if( !GetVolByName(oname, &ref) ) return false ;
	pb.volumeParam.ioNamePtr = nil ;
	pb.volumeParam.ioVolIndex = 0 ;
	pb.volumeParam.ioVRefNum = ref ;
	if( PBHGetVInfoSync(&pb) != noErr ) return false ;
	pb.volumeParam.ioNamePtr = buff ;
	C2P(nname, buff) ;
	return PBSetVInfoSync(&pb) == noErr ;
}

static Bool GetFileByName(CharPt name, short r, long d, Bool *isDir)
{
	CInfoPBRec pInfo ;
	Str255 buff ;
	pInfo.hFileInfo.ioNamePtr = buff ;
	pInfo.hFileInfo.ioVRefNum = r ;
	pInfo.hFileInfo.ioDirID = d ;
	pInfo.hFileInfo.ioFDirIndex = 0 ;
	C2P(name, buff) ;
	if( PBGetCatInfoSync(&pInfo) != noErr ) return false ;
	if( isDir != nil )
		*isDir = (pInfo.hFileInfo.ioFlAttrib & ioDirMask) != 0 ;
	return true ;
}

static Pt GetFileByIdx(int i, short r, long d)
{
	CInfoPBRec pInfo ;
	Str255 buff ;
	pInfo.hFileInfo.ioNamePtr = buff ;
	pInfo.hFileInfo.ioVRefNum = r ;
	pInfo.hFileInfo.ioDirID = d ;
	pInfo.hFileInfo.ioFDirIndex = i ;
	if( PBGetCatInfoSync(&pInfo) != noErr ) return nil ;
	buff[buff[0]+1] = '\0' ;
	return MakeTempAtom(cCharPt(buff+1)) ;
}

static Bool GetDirByName(CharPt name, short r, long d, long *nd, Word *attr)
{
	CInfoPBRec pInfo ;
	Str255 buff ;
	pInfo.dirInfo.ioNamePtr = buff ;
	pInfo.dirInfo.ioVRefNum = r ;
	pInfo.dirInfo.ioDrDirID = d ;
	pInfo.dirInfo.ioFDirIndex = 0 ;
	C2P(name, buff) ;
	if( PBGetCatInfoSync(&pInfo) != noErr ) return false ;
	if( nd != nil )
		*nd = pInfo.dirInfo.ioDrDirID ;
	if( attr != nil )
		*attr = pInfo.dirInfo.ioFlAttrib ;
	return true ;
}

static Pt GetDirById(short r, long d, long *dPar)
{
	CInfoPBRec pInfo ;
	Str255 buff ;
	pInfo.dirInfo.ioNamePtr = buff ;
	pInfo.dirInfo.ioVRefNum = r ;
	pInfo.dirInfo.ioDrDirID = d ;
	pInfo.dirInfo.ioFDirIndex = -1 ;
	if( PBGetCatInfoSync(&pInfo) != noErr ) return nil ;
	buff[buff[0]+1] = '\0' ;
	*dPar = pInfo.dirInfo.ioDrParID ;
	return MakeTempAtom(cCharPt(buff+1)) ;
}

Bool OSExists(CharPt fname)
{
	if( atVirtualRoot )
		return GetVolByName(fname, nil) ;
	else
		return GetFileByName(fname, vRefNum, dirID, nil) ;
}

Bool OSRen(CharPt oname, CharPt nname)
{
	Str255 oBuff, nBuff ;
	if( atVirtualRoot )
		return VolRename(oname, nname) ;
	else {
		C2P(oname, oBuff) ;
		C2P(nname, nBuff) ;
		return HRename(vRefNum, dirID, oBuff, nBuff) == noErr ;
	}
}

Bool OSDel(CharPt fname)
{
	Str255 buff ;
	if( atVirtualRoot )
		return false ;
	else {
		C2P(fname, buff) ;
		return HDelete(vRefNum, dirID, buff) == noErr ;
	}
}

Pt OSPropType(CharPt fname)
{
	Bool isDir ;
	if( atVirtualRoot )
		return GetVolByName(fname, nil) ? MakeAtom("dir") : nil ;
	else {
		if( !GetFileByName(fname, vRefNum, dirID, &isDir) ) return nil ;
		return MakeAtom(isDir ? "dir" : "file") ;
	}
}

Pt OSPropReadable(CharPt fname)
{
	Bool isDir ;
	if( atVirtualRoot )
		return GetVolByName(fname, nil) ? tFalseAtom : nil ;
	else {
		if( !GetFileByName(fname, vRefNum, dirID, &isDir) ) return nil ;
		return isDir ? tFalseAtom : tTrueAtom ;
	}
}

Pt OSGetCurrDir()
{
	long dID, dParID ;
	Pt t, list = tNilAtom ;
	Hdl h = &list + 1 ;
	if( atVirtualRoot )
		return tNilAtom ;
	else {
		dID = dirID ;
		for(;;) {
			if( (t = GetDirById(vRefNum, dID, &dParID)) == nil )
				return nil ;
			h[-1] = MakeList(t, tNilAtom) ;
			h = H ;
			if( dID == fsRtDirID ) break ;
			dID = dParID ;
		}
		return list ;
	}
}

Bool OSSetCurrDir(Pt t)
{
	short ref ;
	long dID, dNewID ;
	Word flags ;
	Size len ;
	Hdl h = ListToArray(t, &len) ;
	if( len == 0 ) {
		atVirtualRoot = true ;
		return true ;
	}
	if( !GetVolByName(XTestAtomName(h[--len]), &ref) )
		return false ;
	dID = 0 ;
	while( len-- ) {
		if( !GetDirByName(XTestAtomName(h[len]), ref,
							dID, &dNewID, &flags) ) return false ;
		if( (flags & ioDirMask) == 0 ) return false ;
		dID = dNewID ;
	}
	if( HSetVol(nil, ref, dID) != noErr ) return false ;
	atVirtualRoot = false ;
	vRefNum = ref ;
	dirID = dID ;
	return true ;
}

void OSGoHome()
{
	atVirtualRoot = false ;
	vRefNum = vRefNumHome ;
	dirID = dirIDHome ;
}

Pt OSFiles()
{
	Pt t, list = tNilAtom ;
	Hdl h = &list + 1 ;
	int i ;
	if( atVirtualRoot ) {
		for( i = 1 ; t = GetVolByIdx(i) ; i++ ) {
			h[-1] = MakeList(t, tNilAtom) ;
			h = H ;
		}
	}
	else {
		for( i = 1 ; t = GetFileByIdx(i, vRefNum, dirID) ; i++ ) {
			h[-1] = MakeList(t, tNilAtom) ;
			h = H ;
		}
	}
	return list ;
}

void OSFileSysInit()
{
	if( HGetVol(nil, &vRefNum, &dirID) != noErr )
		FileError("Couldn't get current directory") ;
	atVirtualRoot = false ;
	vRefNumHome = vRefNum ;
	dirIDHome = dirID ;
}


/* SOCKETS */

int OSInstallServer(int port, int queueLen)
{
	Error("Sockets not supported on the Classic Mac.") ;
}
	
void OSAccept(int server, FILE **r, FILE **w)
{
	Error("Sockets not supported on the Classic Mac.") ;
}

void OSUninstallServer(int server)
{
	Error("Sockets not supported on the Classic Mac.") ;
}

void OSConnect(CharPt host, int port, FILE **r, FILE **w)
{
	Error("Sockets not supported on the Classic Mac.") ;
}

PInt OSEncodeInt(PInt i)
{
	return i ;
}

PInt OSDecodeInt(PInt i)
{
	return i ;
}


/* PROCESSES */

#include <Processes.h>

int OSFork()
{
	Error("Cannot fork") ;
}

void OSWait()
{
	Error("Cannot wait") ;
}

void OSKill(int pid)
{
}

int OSGetPid()
{
	return 0 ;
}

void OSSleep(Size secs)
{
	long m;
	Delay(60L * secs, &m);
}

Bool OSRun(CharPt fname)
{
	LaunchParamBlockRec lb ;
	FSSpec fs ;
	fs.vRefNum = vRefNum ;
	fs.parID = dirID ;
	C2P(fname, fs.name) ;
	lb.launchBlockID = extendedBlock ;
	lb.launchEPBLength = extendedBlockLen ;
	lb.launchFileFlags = 0 ;
	lb.launchControlFlags = launchContinue + launchNoFileFlags ;
	lb.launchAppSpec = &fs ;
	lb.launchAppParameters = nil ;
	return LaunchApplication(&lb) == noErr ;
}

void OSPipe(int *fd)
{
	Error("Cannot create pipe") ;
}

int OSPipeBufferSize()
{
	return 0 ;
}

void OSWrite(int fd, VoidPt buf, Size size)
{
	Error("Inter-process communication") ;
}

Bool OSRead(int fd, VoidPt buf, Size size, Bool blocking)
{
	Error("Inter-process communication") ;
}

CharPt OSGetEnv(CharPt var)
{
	return getenv(var) ;
}


/* OTHER */

CharPt OSName()
{
	return "mac" ;
}


#endif
