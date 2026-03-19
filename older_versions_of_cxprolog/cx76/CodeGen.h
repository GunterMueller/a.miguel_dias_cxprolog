/*
 *   This file is part of the CxProlog system

 *   CodeGen.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _CodeGen_
#define _CodeGen_

/* CODE GEN */
#define Gen0(c)					(*codePt++ = cPt(c))
#define Gen1(c1,c2)				(Gen0(c1), Gen0(c2))
#define Gen2(c1,c2,c3)			(Gen1(c1, c2), Gen0(c3))
#define Gen3(c1,c2,c3,c4)		(Gen2(c1, c2, c3), Gen0(c4))
#define Gen4(c1,c2,c3,c4,c5)	(Gen3(c1, c2, c3, c4), Gen0(c5))
#define CodeOverflow()			( Ge(codePt, bufferEnd) )
#define CodeStart()				( cHdl(buffer) )
#define CodeReset()				( codePt = cHdl(UseBuffer()) )
#define CodeFinish()			FreeBuffer()
#define CodeSkip(n)				( codePt += (n) )
#define CodeSize()				( codePt - cHdl(buffer) )
#define CodeCurr()				( codePt )


/* CODE READ */
#define CodeStartReading(c)		( codePt = (c) )
#define GetCode()				( *codePt++ )

extern Hdl codePt ;

#endif
