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
	Bool inUse : 1 ;	\
	Bool isHidden : 1 ;	\
	struct Type *next

typedef struct Extra
{
	ExtraDef(Extra) ;
} Extra, *ExtraPt ;

typedef struct ExtraType
{
	Word tag ;
	CharPt name ;
	Size size ;
	CFunV namingFun ;
	VoidPt first, last ;
	VoidPt freeList ;
} ExtraType, *ExtraTypePt ;

#define cExtraPt(x)				((ExtraPt)(x))
#define ExtraIsHidden(x)		(cExtraPt(x)->isHidden)

typedef Size (*ExtraFun)(VoidPt) ;
typedef Bool (*ExtraCond)(VoidPt, VoidPt) ;

ExtraTypePt ExtraTypeNew(CharPt name, Size size, CFunV namingFun) ;

Bool IsThisExtra(ExtraTypePt e, Pt t) ;
VoidPt ExtraNew(ExtraTypePt e) ;
void ExtraDelete(ExtraTypePt e, VoidPt x) ;
Size ExtraForEach(ExtraTypePt e, ExtraFun proc) ;
VoidPt ExtraFindFirst(ExtraTypePt e, ExtraCond cond, VoidPt arg) ;
void ExtraPNDCurrent(ExtraTypePt e, BFunV bfun, int arity, int resPos) ;
void ExtraShow(ExtraTypePt e, ExtraFun fun) ;

VoidPt XTestExtra(ExtraTypePt e, Pt t) ;
Bool XExtraCheck(ExtraTypePt e, Pt t) ;
CharPt XExtraTypeName(Pt t) ;
CharPt XExtraAsStr(Pt t) ;
Pt MakeExtraFromStr(CharPt s) ;
void BindVarWithExtra(Pt t, VoidPt ref) ;
VoidPt ExtraTypeError(ExtraTypePt e, CharPt alt, Pt t) ;

#endif
