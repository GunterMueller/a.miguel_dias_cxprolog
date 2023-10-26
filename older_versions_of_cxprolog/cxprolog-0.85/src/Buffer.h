/*
 *   This file is part of the CxProlog system

 *   Buffer.h
 *   by A.Miguel Dias - 2001/02/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Buffer_
#define _Buffer_


/* BUFFER */

extern CharPt bufferBegin, bufferEnd, saveBufferPt, originalBuffer ;
extern Mix bufferMix ;

#define bufferChPt			bufferMix.asCharPt
#define bufferHPt			bufferMix.asHdl


/* BUFFER HANDLING */

#define BufferIsFree()		(bufferChPt == nil)
#define UseBuffer()			cVoidPt( BufferIsFree()					\
									? (bufferChPt = bufferBegin)	\
									: Error("Buffer in use") )
#define FreeBuffer()		cVoidPt(((bufferChPt = nil), bufferBegin))
#define BufferCheck()		((bufferChPt>=bufferEnd)?BufferExpand():0)
#define BufferRegister()	(originalBuffer = bufferBegin)
#define BufferHasMoved()	(originalBuffer != bufferBegin)

int BufferExpand(void) ;
void BufferMakeRoom(Size nWords) ;
Size BufferTotalSize(void) ;
void BufferInit(void) ;


/* BUFFER SAVE */

#define BufferSave()		(saveBufferPt = bufferChPt)
#define BufferRestore()		(bufferChPt = saveBufferPt)


/* BUFFER AS PT-STACK */

#define _BP(pt)				( Push(bufferHPt, pt) )
#define BufferPush(pt)		( BufferCheck(), _BP(pt) )
#define BufferPop()			( Pop(bufferHPt) )
#define BufferBack(n)		( Grow(bufferHPt, -(n)) )
#define BufferReset()		( cHdl(bufferChPt = bufferBegin) )
#define BufferBegin()		( cHdl(bufferBegin) )
#define BufferCurr()		( bufferHPt )
#define BufferTop()			( Top(bufferHPt) )
#define BufferXTop(n)		( XTop(bufferHPt, n) )
#define BufferUsed()		( Df(bufferHPt, bufferBegin) )
#define BufferDistToSaved()	( Df(bufferHPt, saveBufferPt) )


/* BUFFER AS CODE-BUFFER */

#define Gen0(c)				( BufferPush(c) )
#define Gen1(c1,c2)			(Gen0(c1), _BP(c2))
#define Gen2(c1,c2,c3)		(Gen1(c1, c2), _BP(c3))
#define Gen3(c1,c2,c3,c4)	(Gen2(c1, c2, c3), _BP(c4))
#define Gen4(c1,c2,c3,c4,c5) (Gen3(c1, c2, c3, c4), _BP(c5))


/* BUFFER AS TEXT-BUFFER */

#define BufferAddCh(c)	( BufferCheck(), *bufferChPt++ = (c) )
void BufferAddNStr(CharPt s, Size len) ;
void BufferAddStr(CharPt s) ;
void BufferAddQStr(CharPt s, Bool dirty) ;
void BufferAddPString(Pt l) ;
void BufferAddTerm(Pt t) ;
void BufferWrite(CharPt fmt, ...) ;

#endif
