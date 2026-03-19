/*
 *   This file is part of the CxProlog system

 *   HashTable.c
 *   by A.Miguel Dias - 2000/04/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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

#define HashTableSize(h)		(h)->size
#define HashTableGetNext(h,t)	(((h)->getNextFun)(t))
#define HashTableGetAtom(h,t)	(((h)->getAtomFun)(t))
#define HashTableFilter(h,t)	(((h)->filterFun)(t))
#define HashTableEntries(h)		cHdl((h) + 1)

#define HashFun(h,pt)			( ( cWord(pt)>>3 ) & (HashTableSize(h) - 1) )

HashTablePt NewHashTable(int tableNEntries, GetNextFun getNextFun,
										GetAtomFun getAtomFun, FilterFun filterFun)
{
	HashTablePt h = PermBlockAllocate(WordsOf(HashTable) + tableNEntries) ;
	register int i ;
	HashTableSize(h) = tableNEntries ;
	h->getNextFun = getNextFun ;
	h->getAtomFun = getAtomFun ;
	h->filterFun = filterFun ;
	dotimes(i, tableNEntries)
		HashTableEntries(h)[i] = nil ;
	return h ;
}

VoidPt HashTableFind(HashTablePt h, AtomPt atom)
{
	register VoidPt pt ;
	int slot = HashFun(h, atom) ;
	dolist(pt, HashTableEntries(h)[slot], HashTableGetNext(h,pt))
		if( HashTableGetAtom(h,pt) == atom && HashTableFilter(h,pt) > 0 )
			return pt ;
	return nil ;
}

VoidPt HashTableAdd(HashTablePt h, VoidPt pt)
{
	int slot = HashFun(h, HashTableGetAtom(h,pt)) ;
	VoidPt old = HashTableEntries(h)[slot] ;
	HashTableEntries(h)[slot] = pt ;
	return old ;
}

static VoidPt HashTableFirstNF(HashTablePt h)
{
	register int i ;
	dotimes(i, HashTableSize(h))
		if( HashTableEntries(h)[i] != nil )
			return HashTableEntries(h)[i] ;
	return nil ;
}

static VoidPt HashTableNextNF(HashTablePt h, VoidPt pt)
{
	if( HashTableGetNext(h,pt) != nil )
		return HashTableGetNext(h,pt) ;
	else {
		register int i ;
		int slot = HashFun(h, HashTableGetAtom(h,pt)) ;
		for( i = slot + 1 ; i < HashTableSize(h) ; i++ )
			if( HashTableEntries(h)[i] != nil )
				return HashTableEntries(h)[i] ;
	}
	return nil ;
}

static void HashTableList(HashTablePt h)
{
	Pt pt ;
	Write("\tHashTable:\n") ;
	dolist(pt, HashTableFirst(h), HashTableNext(h, pt))
		Write("\t\t%s\n", AtomName(HashTableGetAtom(h,pt))) ;
}

VoidPt HashTableFirst(HashTablePt h)
{
	VoidPt pt ;
	for( pt = HashTableFirstNF(h) ;
				pt != nil && HashTableFilter(h,pt) != 1 ;
					pt = HashTableNextNF(h, pt) ) ;
	return pt ;
}

VoidPt HashTableNext(HashTablePt h, VoidPt pt)
{
	for( pt = HashTableNextNF(h, pt) ;
				pt != nil && HashTableFilter(h,pt) != 1 ;
					pt = HashTableNextNF(h, pt) ) ;
	return pt ;
}
