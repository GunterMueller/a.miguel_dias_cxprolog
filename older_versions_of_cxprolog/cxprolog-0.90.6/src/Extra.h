/*
 *   This file is part of the CxProlog system

 *   Extra.h
 *   by A.Miguel Dias - 2002/01/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Extra_
#define _Extra_

#define ExtraDef(Type)	\
	short tag ;			\
	short inUse ;		\
	struct Type *next

typedef struct ExtraType
{
	Word tag ;
	CharPt name ;
	Size size ;
	CFunV namingFun ;
	VoidPt first, last ;
	VoidPt freeList ;
} ExtraType, *ExtraTypePt ;

typedef Size (*ExtraFun)(VoidPt) ;
typedef Bool (*ExtraCond)(VoidPt, VoidPt) ;

ExtraTypePt ExtraTypeNew(CharPt name, Size size, CFunV namingFun) ;

Bool IsThisExtra(ExtraTypePt e, Pt t) ;
VoidPt ExtraNew(ExtraTypePt e) ;
void ExtraDelete(ExtraTypePt e, VoidPt x) ;
Size ForEachExtra(ExtraTypePt e, ExtraFun proc) ;
VoidPt ExtraFindFirst(ExtraTypePt e, ExtraCond cond, VoidPt arg) ;
void PNDCurrentExtra(ExtraTypePt e, BFunV bfun, int arity) ;

VoidPt XTestExtra(ExtraTypePt e, Pt t) ;
Bool XExtraCheck(ExtraTypePt e, Pt t) ;
CharPt XExtraTypeName(Pt t) ;
CharPt XExtraAsStr(Pt t) ;
Pt MakeExtraFromStr(CharPt s) ;
void BindVarWithExtra(Pt t, VoidPt ref) ;
VoidPt ExtraTypeError(ExtraTypePt e, CharPt alt, Pt t) ;

#endif
