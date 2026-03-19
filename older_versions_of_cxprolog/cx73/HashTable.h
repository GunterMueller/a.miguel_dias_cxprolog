/*
 *   This file is part of the CxProlog system

 *   HashTable.h
 *   by A.Miguel Dias - 2000/04/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _HashTable_
#define _HashTable_

typedef VoidPt (*GetNextFun)(VoidPt) ;
typedef AtomPt (*GetAtomFun)(VoidPt) ;
typedef int (*FilterFun)(VoidPt) ;

typedef struct HashTable
{
	Int size ;						/* Table size. Must be a power of 2 */
	GetNextFun getNextFun ;
	GetAtomFun getAtomFun ;
	FilterFun filterFun ;
/*	Hdl entries ; */				/* Starts of hash chains */
} HashTable, *HashTablePt ;

HashTablePt NewHashTable(int tableNEntries, GetNextFun getNextFun,
							GetAtomFun getAtomFun, FilterFun filterFun) ;
VoidPt HashTableFind(HashTablePt h, AtomPt atom) ;
VoidPt HashTableAdd(HashTablePt h, VoidPt pt) ;
VoidPt HashTableFirst(HashTablePt h) ;
VoidPt HashTableNext(HashTablePt h, VoidPt pt) ;

#endif
