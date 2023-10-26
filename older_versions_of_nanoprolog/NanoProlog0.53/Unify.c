/*
 *   This file is part of the NanoProlog system

 *   Unify.c
 *   by A.Miguel Dias - 89/11/14
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931117: release of version 0.5

*/

#include "NanoProlog.h"


Bool UnifyWithAtomic(a, c)
Pt a, c ;
{
	VarValue(a) ;
	if( IsVar(a) ) Assign(a, c) ;
	elif( a != c ) return( false ) ;
	return( true ) ;
}

Bool Unify(t1, t2)
register Pt t1, t2 ;
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return( true ) ;
	
	if( IsVar(t1) )
	{
		if( IsVar(t2) ) Bind(t1, t2) ;
		else Assign(t1, t2) ;
		return( true ) ;
	}
	
	if( IsVar(t2) )
	{
		Assign(t2, t1) ;
		return( true ) ;
	}

	if( IsStruct(t1) )
	{
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) )
		{
			register n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
		
			do
			{
				if( not Unify(*arg1++, *arg2++) ) return( false ) ;
			}
			while( --n ) ;
			return( true ) ;
		}
		else return( false ) ;
	}
	
	if( IsList(t1) && IsList(t2) )
		return( Unify(XListHead(t1), XListHead(t2))
				& Unify(XListTail(t1), XListTail(t2)) ) ;
	
	return( false ) ;
}

Bool Equal(t1, t2)
register Pt t1, t2 ;
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return( true ) ;
	
	if( IsStruct(t1) )
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) )
		{
			register n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
		
			do
			{
				if( not Equal(*arg1++, *arg2++) ) return( false ) ;
			}
			while( --n ) ;
			return( true ) ;
		}
		else return( false ) ;
	
	if( IsList(t1) && IsList(t2) )
		return( Equal(XListHead(t1), XListHead(t2))
				& Equal(XListTail(t1), XListTail(t2)) ) ;
	
	return( false ) ;
}

Bool RawUnify(t1, t2)
register Pt t1, t2 ;
{
	VarValue(t1) ;
	VarValue(t2) ;

	if( t1 == t2 ) return( true ) ;

	if( IsVar(t1) || IsVar(t2) ) return( true ) ;

	if( IsStruct(t1) )
		if( IsStruct(t2) && XStructFunctor(t1) == XStructFunctor(t2) )
		{
			register n = XStructArity(t1) ;
			Hdl arg1 = XStructArgs(t1),
				arg2 = XStructArgs(t2) ;
		
			do
			{
				if( not RawUnify(*arg1++, *arg2++) ) return( false ) ;
			}
			while( --n ) ;
			return( true ) ;
		}
		else return( false ) ;
	
	if( IsList(t1) && IsList(t2) )
		return( RawUnify(XListHead(t1), XListHead(t2))
				& RawUnify(XListTail(t1), XListTail(t2)) ) ;
		
	return( false ) ;
}