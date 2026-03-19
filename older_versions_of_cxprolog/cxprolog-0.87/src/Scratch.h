/*
 *   This file is part of the CxProlog system

 *   Scratch.h
 *   by A.Miguel Dias - 2001/02/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Scratch_
#define _Scratch_

/* The Scratch Pad is a memory buffer that is used for several purposes */

extern CharPt scratchBegin, scratchEnd, saveScratchPt, originalScratch ;
extern Mix scratchMix ;

#define scratchChPt			scratchMix.asCharPt
#define scratchHPt			scratchMix.asHdl


/* BUFFER HANDLING */

#define ScratchIsFree()		(scratchChPt == nil)
#define UseScratch()			cVoidPt( ScratchIsFree()			\
									? (scratchChPt = scratchBegin)	\
									: FatalError("Scratch pad in use") )
#define FreeScratch()		cVoidPt(((scratchChPt = nil), scratchBegin))
#define ScratchCheck()		((scratchChPt>=scratchEnd)?ScratchExpand():0)
#define ScratchRegister()	(originalScratch = scratchBegin)
#define ScratchHasMoved()	(originalScratch != scratchBegin)

int ScratchExpand(void) ;
void ScratchMakeRoom(Size nWords) ;
Size ScratchTotalSize(void) ;
void ScratchInit(void) ;


/* BUFFER SAVE */

#define ScratchSave()		(saveScratchPt = scratchChPt)
#define ScratchRestore()	(scratchChPt = saveScratchPt)


/* BUFFER AS PT-STACK */

#define _BP(pt)				( Push(scratchHPt, pt) )
#define ScratchPush(pt)		( ScratchCheck(), _BP(pt) )
#define ScratchPop()		( Pop(scratchHPt) )
#define ScratchBack(n)		( Grow(scratchHPt, -(n)) )
#define ScratchReset()		( cHdl(scratchChPt = scratchBegin) )
#define ScratchBegin()		( cHdl(scratchBegin) )
#define ScratchCurr()		( scratchHPt )
#define ScratchTop()			( Top(scratchHPt) )
#define ScratchXTop(n)		( XTop(scratchHPt, n) )
#define ScratchUsed()		( Df(scratchHPt, scratchBegin) )
#define ScratchDistToSaved()	( Df(scratchHPt, saveScratchPt) )


/* BUFFER AS CODE-BUFFER */

#define Gen0(c)				( ScratchPush(c) )
#define Gen1(c1,c2)			(Gen0(c1), _BP(c2))
#define Gen2(c1,c2,c3)		(Gen1(c1, c2), _BP(c3))
#define Gen3(c1,c2,c3,c4)	(Gen2(c1, c2, c3), _BP(c4))
#define Gen4(c1,c2,c3,c4,c5) (Gen3(c1, c2, c3, c4), _BP(c5))


/* BUFFER AS TEXT-BUFFER */

#define ScratchAddCh(c)	( ScratchCheck(), *scratchChPt++ = (c) )
void ScratchAddNStr(CharPt s, Size len) ;
void ScratchAddStr(CharPt s) ;
void ScratchAddPString(Pt l) ;
void ScratchAddTerm(Pt t) ;
void ScratchWriteV(CharPt fmt, VoidPt v) ;
void ScratchWrite(CharPt fmt, ...) ;

#endif
