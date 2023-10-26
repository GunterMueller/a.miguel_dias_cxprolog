/*
 *   This file is part of the CxProlog system

 *   Dict.h
 *   by A.Miguel Dias - 2000/09/29
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Dict_
#define _Dict_

/* DICT */
typedef struct DEntry {
	Pt key ;
	Pt value ;
} DEntry, *DEntryPt ;

typedef struct Dict {
	ExtraDef(Dict) ;
	DEntryPt begin, end, first, last  ;
} Dict, *DictPt ;

#define cDictPt(x)				((DictPt)(x))

#define DictBegin(d)			((d)->begin)
#define DictEnd(d)				((d)->end)
#define DictFirst(d)			((d)->first)
#define DictLast(d)				((d)->last)

/* MAIN OPERATIONS */
Size DictSize(DictPt d) ;
DictPt DictNew(void) ;
void DictClear(DictPt d) ;
void DictDelete(DictPt d) ;
void DictFilter(DictPt d, BFunVV filter, VoidPt x) ;
Bool DictGet(DictPt d, Pt k, Pt *t) ;
void DictSet(DictPt d, Pt k, Pt t) ;
Bool DictDeleteItem(DictPt d, Pt k) ;

/* TEST, EXTRACT & INIT */
Bool IsDict(Pt t) ;
DictPt XTestDict(Pt t) ;
void DictsInit(void) ;

#endif
