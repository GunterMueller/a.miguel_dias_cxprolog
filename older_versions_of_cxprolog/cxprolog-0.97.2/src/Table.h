/*
 *   This file is part of the CxProlog system

 *   Table.h
 *   by A.Miguel Dias - 2008/08/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Table_
#define _Table_

typedef struct {
	CharPt name ;
	int itemSize ;	
	int capacity ;	
	Hdl begin, last, end ;
} Table, *TablePt ;

#define TableName(t)			( (t).name )
#define TableItemSize(t)		( (t).itemSize )
#define TableCapacity(t)		( (t).capacity )
#define TableBegin(t)			( (t).begin )
#define TableLast(t)			( (t).last )
#define TableEnd(t)				( (t).end )

#define TableFor(t, h)			for( (h) = TableBegin(t) ;					\
										(h) < TableLast(t) ;				\
										(h) += TableItemSize(t) )

#define TableForRev(t, h)		for( (h) = TableLast(t) - TableItemSize(t) ;\
										(h) >= TableBegin(t) ;				\
										(h) -= TableItemSize(t) )
	
void TableInit(TablePt t, CharPt name, int itemSize, int initialCapacity) ;
int TableNItems(TablePt t) ;
void TableReset(TablePt t) ;
Hdl TableNewItem(TablePt t, VoidPt v) ;

#endif
