/*
 *   This file is part of the CxProlog system

 *   Extra.h
 *   by A.Miguel Dias - 2002/01/01
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Extra_
#define _Extra_

typedef Size (*ExtraFun)(VoidPt) ;
typedef Bool (*ExtraCond)(VoidPt, VoidPt) ;

typedef struct ExtraType {
	Word tag ;
	CharPt name ;
	ExtraFun sizeFun ;
	FunV gcMarkFun ;
	BFunV gcDeleteFun ;
	VoidPt *hashTable ;
	Size hashTableSize ;
} ExtraType, *ExtraTypePt ;

#define ExtraDef(Type)		\
	unsigned int tag : 8 ;	\
	Bool disabled : 1 ;		\
	Bool hidden : 1 ;		\
	Bool permanent : 1 ;	\
	Bool gcMarked : 1 ;		\
	struct Type *next ;

typedef struct Extra {
	ExtraDef(Extra) ;
} Extra, *ExtraPt ;

#define cExtraPt(x)					((ExtraPt)(x))

#define ExtraTag(x)					(cExtraPt(x)->tag)

#define XExtraTag(t)				ExtraTag(XExtra(t))

ExtraTypePt ExtraTypeNew(CharPt name, ExtraFun sizeFun,
							FunV gcMarkFun, BFunV gcDeleteFun, Size htSize) ;
Bool IsThisExtra(ExtraTypePt e, Pt t) ;
VoidPt ExtraNewWithSize(ExtraTypePt e, Size size, int slot) ;
VoidPt ExtraNew(ExtraTypePt e, int slot) ;
Pt TagExtraAuto(VoidPt x) ;
Bool ExtraIsAlive(VoidPt x) ;
void ExtraSetDisabled(VoidPt x) ;
void ExtraSetHidden(VoidPt x) ;
void ExtraSetPermanent(VoidPt x) ;
CharPt ExtraAsStr(VoidPt x) ;
VoidPt ExtraGetFirst(ExtraTypePt e) ;	/* pre: only one slot */
VoidPt ExtraGetNext(VoidPt e) ;
Size ExtraForEach(ExtraTypePt e, ExtraFun proc) ;
VoidPt ExtraFindFirst(ExtraTypePt e, int slot, ExtraCond cond, VoidPt arg) ;
void ExtraPNDCurrent(ExtraTypePt e, BFunV bfun, int arity, int resPos) ;
void ExtraShow(ExtraTypePt e, ExtraFun fun) ;

/* EXTRAS GARBAGE COLLECTION */
void ExtraGCHandlerInstall(CharPt name, Fun p) ;
void ExtraGCAddDelta(Size size) ;
void ExtraGCMark(VoidPt x) ;
void ExtraGCMarkRange(Hdl a, Hdl z) ;
int ExtraGCClearNotMarked(ExtraTypePt e) ;
void ExtraGC(void) ;

/* PT OPERATIONS */
VoidPt XTestExtra(ExtraTypePt e, Pt t) ;
Bool XExtraCheck(ExtraTypePt e, Pt t) ;
CharPt XExtraTypeName(Pt t) ;
CharPt XExtraAsStr(Pt t) ;
Pt MakeExtraFromStr(CharPt s) ;
void BindVarWithExtra(Pt t, VoidPt x) ;
VoidPt ExtraTypeError(ExtraTypePt e, CharPt alt, Pt found) ;
void ExtraInit(void) ;
void ExtraInit2(void) ;

#endif
