 /*
 *   This file is part of the CxProlog system

 *   StreamProperty.c
 *   by A.Miguel Dias - 2006/05/31
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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

OpenStreamOptionsPt StreamPropertyGetDefaultOpenOptions(OpenStreamOptionsPt so)
{
	so->type = tTextAtom ;
	so->reposition = tTrueAtom ;
	so->alias = nil ;
	so->eofAction = tEofCodeAtom ;
	so->encoding = tTextAtom ;
	return so ;
}

static void StreamPropertyHandleOpenOption(OpenStreamOptionsPt so, CharPt opt, Pt arg)
{
	switch( opt[0] )
	{
		case 'a':
			if( StrEqual(opt, "alias") ) {
				if( so->alias && so->alias != arg )
					FileError("Duplicate 'alias' options") ;
				else so->alias = arg ;
				break ;
			}
			else goto invalidOption ;
		case 't':					
			if( StrEqual(opt, "type") ) {
				if( arg != tTextAtom && arg != tBinaryAtom )
					FileError("'type' argument should be 'text/binary'") ;
				elif( so->type && so->type != arg )
					FileError("Duplicate 'type' options") ;
				else so->type = arg ;
				break ;
			}
			else goto invalidOption ;
		case 'e':					
			if( StrEqual(opt, "eof_action") ) {
				if( arg != tErrorAtom && arg != tEofCodeAtom && arg != tResetAtom )
					FileError("'eof_action' argument should be 'error/eof_code/reset'") ;
				elif( so->eofAction && so->eofAction != arg )
					FileError("Duplicate 'eof_action' options") ;
				else so->eofAction = arg ;
				break ;						
			}
			elif( StrEqual(opt, "encoding") ) {
				if( so->encoding && so->encoding != arg )
					FileError("Duplicate 'encoding' options") ;
				else
					so->encoding = arg ;
				break ;
			}
			else goto invalidOption ;
		case 'r':					
			if( StrEqual(opt, "reposition") ) {
				if( arg != tTrueAtom && arg != tFalseAtom )
					FileError("'reposition' argument should be a boolean") ;
				elif( so->reposition && so->reposition != arg )
					FileError("Duplicate 'reposition' options") ;
				else so->reposition = arg ;
				break ;
			}
			else goto invalidOption ;
		default:
invalidOption:
			FileError("Invalid open stream option '%s'", opt) ;
	}
}

OpenStreamOptionsPt StreamPropertyGetOpenOptions(register Pt t, OpenStreamOptionsPt so)
{
	t = Drf(t) ;
	if( !IsAtom(t) && !IsList(t) )
		TypeError("LIST", t) ;

/* Clear record */
	so->type = nil ;
	so->reposition = nil ;
	so->alias = nil ;
	so->eofAction = nil ;
	so->encoding = nil ;

/* Transverse list */
	if( t == tNilAtom || IsList(t) )
		for( ; IsList(t) ; t = Drf(XListTail(t)) ) {
			register Pt e = Drf(XListHead(t)) ;
			if( IsStruct(e) && XStructArity(e) == 1 && IsAtom(Drf(XStructArg(e,0))) )
				StreamPropertyHandleOpenOption(so, XStructName(e), Drf(XStructArg(e,0))) ;
			else FileError("Invalid open stream option") ;
		}
	else so->encoding = t ;

/* Set defaults */
	if( so->type == nil ) {
		if( so->encoding == nil )
			so->type = so->encoding = tTextAtom ;
		else
			so->type = so->encoding == tBinaryAtom ? tBinaryAtom : tTextAtom ;
	}
	elif( so->type == tTextAtom ) {
		if( so->encoding == nil )
			so->encoding = tTextAtom ;
		elif( so->encoding == tBinaryAtom )
			FileError("Incompatible 'type/encoding' combination") ;
	}
	elif( so->type == tBinaryAtom ) {
		if( so->encoding == nil )
			so->encoding = tBinaryAtom ;
		elif( so->encoding != tBinaryAtom )
			FileError("Incompatible 'type/encoding' combination") ;
	}
	else InternalError("StreamPropertyGetOpenOptions") ;
	
	if( so->reposition == nil )
		so->reposition = tTrueAtom ;
	if( so->eofAction == nil )
		so->eofAction = tEofCodeAtom ;

	return so ;
}

CloseStreamOptionsPt StreamPropertyGetDefaultCloseOptions(CloseStreamOptionsPt co)
{
	co->force = tFalseAtom ;
	return co ;
}

static void StreamPropertyHandleCloseOption(CloseStreamOptionsPt co, CharPt opt, Pt arg)
{
	switch( opt[0] )
	{
		case 'f':					
			if( StrEqual(opt, "force") ) {
				if( arg != tTrueAtom && arg != tFalseAtom )
					FileError("'force' argument should be a boolean") ;
				elif( co->force && co->force != arg )
					FileError("Duplicate 'type' options") ;
				else co->force = arg ;
				break ;
			}
			else goto invalidOption ;
		default:
invalidOption:
			FileError("Invalid close stream option '%s'", opt) ;
	}
}

CloseStreamOptionsPt StreamPropertyGetCloseOptions(Pt t, CloseStreamOptionsPt co)
{
	t = Drf(t) ;
	if( t != tNilAtom && !IsList(t) )
		TypeError("LIST", t) ;

/* Clear record */
	co->force = nil ;

	/* Transverse list */
	for( ; IsList(t) ; t = Drf(XListTail(t)) ) {
		register Pt e = Drf(XListHead(t)) ;
		if( IsStruct(e) && XStructArity(e) == 1 && IsAtom(Drf(XStructArg(e,0))) )
			StreamPropertyHandleCloseOption(co, XStructName(e), Drf(XStructArg(e,0))) ;
		else FileError("Invalid close stream option") ;
	}

	/* Set defaults */
	if( co->force == nil )
		co->force = tFalseAtom ;

	return co ;
}


/* CXPROLOG C'BUILTINS */

void StreamPropertyInit()
{
	/* Nothing */
}
