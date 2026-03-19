 /*
 *   This file is part of the CxProlog system

 *   StreamProperty.c
 *   by A.Miguel Dias - 2006/05/31
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2008 A.Miguel Dias, CITI, DI/FCT/UNL

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
	so->encoding = MakeAtom("default") ;
	so->bom = tAbsentAtom ;
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
		case 'b':
			if( StrEqual(opt, "bom") ) {
				if( arg != tTrueAtom && arg != tFalseAtom )
					FileError("'bom' argument should be a boolean") ;
				elif( so->bom && so->type != arg )
					FileError("Duplicate 'bom' options") ;
				else so->bom = arg ;
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
	so->bom = nil ;

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
		if( so->encoding == nil ) {
			so->type = tTextAtom ;
			so->encoding = MakeAtom("default") ;
		}
		else
			so->type = so->encoding == tOctetAtom ? tBinaryAtom : tTextAtom ;
	}
	elif( so->type == tTextAtom ) {
		if( so->encoding == nil )
			so->encoding = MakeAtom("default") ;
		elif( so->encoding == tOctetAtom )
			FileError("Incompatible 'type/encoding' combination") ;
	}
	elif( so->type == tBinaryAtom ) {
		if( so->encoding == nil )
			so->encoding = tOctetAtom ;
		elif( so->encoding != tOctetAtom )
			FileError("Incompatible 'type/encoding' combination") ;
	}
	else InternalError("StreamPropertyGetOpenOptions") ;
	
	if( so->reposition == nil )
		so->reposition = tTrueAtom ;
	if( so->eofAction == nil )
		so->eofAction = tEofCodeAtom ;
	if( so->bom == nil )
		so->bom = tAbsentAtom ;

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


/* Stream property hadlers

	- result == false means that the property does not apply
	- result == true means that the property applies. In this case, the
					arguments of the term-property (e.g. X in file_name(X))
					must be returned in the args array.
*/

static Bool HandleAlias(StreamPt srm, Hdl args)
{
	AtomPt a ;
	if( (a = AliasSearch(srm)) != nil ) {
		args[0] = TagAtom(a) ;
		return true ;
	}
	else return false ;
}

static Bool HandleEncoding(StreamPt srm, Hdl args)
{
	args[0] = MakeTempAtom(StreamEncodingName(srm)) ;
	return true ;
}

static Bool HandleBom(StreamPt srm, Hdl args)
{
	switch( StreamBom(srm) ) {
		case false: args[0] = tFalseAtom ; break ;
		case true: args[0] = tTrueAtom ; break ;
		default: return IInternalError("HandleBom") ;
	}
	return true ;
}

static Bool HandleEndOfStream(StreamPt srm, Hdl args)
{
	if( StreamMode(srm) != mRead )
		return false ;
	if( StreamEofSeen(srm) )
		args[0] = MakeAtom("past") ;
	elif( StreamAtEnd(srm) )
		args[0] = MakeAtom("at") ;
	else args[0] = MakeAtom("not") ;
	return true ;
}

static Bool HandleEofAction(StreamPt srm, Hdl args)
{
	switch( StreamEofAction(srm) ) {
		case eofError: args[0] = MakeAtom("error") ; break ;
		case eofCode: args[0] = MakeAtom("eof_code") ; break ;
		case eofReset: args[0] = MakeAtom("reset") ; break ;
		default: return IInternalError("HandleEofAction") ;
	}
	return true ;
}

static Bool HandleFileName(StreamPt srm, Hdl args)
{
	if( IsAbsoluteFileName(AtomName(StreamPath(srm))) ) {
		args[0] = TagAtom(StreamAtom(srm)) ;
		return true ;
	}
	else
		return false ;
}

static Bool HandleFilePath(StreamPt srm, Hdl args)
{
	if( IsAbsoluteFileName(AtomName(StreamPath(srm))) ) {
		args[0] = ZPushTerm(PathNameListFromAtom(StreamPath(srm))) ; /* stacks may grow */
		return true ;
	}
	else
		return false ;
}

static Bool HandleInput(StreamPt srm, Hdl args)
{
	return StreamMode(srm) == mRead ;
}

static Bool HandleMode(StreamPt srm, Hdl args)
{
	switch( StreamMode(srm) ) {
		case mRead: args[0] = MakeAtom("read") ; break ;
		case mWrite: args[0] = MakeAtom("write") ; break ;
		case mAppend: args[0] = MakeAtom("append") ; break ;
		default: return IInternalError("HandleMode") ;
	}
	return true ;
}

static Bool HandleOutput(StreamPt srm, Hdl args)
{
	return StreamMode(srm) == mWrite || StreamMode(srm) == mAppend ;
}

static Bool HandleReposition(StreamPt srm, Hdl args)
{
	args[0] = MakeBool(StreamAllowReposition(srm)) ;
	return true ;
}

static Bool HandleStreamTty(StreamPt srm, Hdl args)
{
	args[0] = MakeBool(StreamIsATty(srm)) ;
	return true ;
}

static Bool HandleStreamType(StreamPt srm, Hdl args)
{
	args[0] = MakeAtom(StreamIsBinary(srm) ? "binary" : "text") ;
	return true ;
}

typedef struct {
	CharPt name ;
	int arity ;
	Bool (*fun)(StreamPt, Hdl) ;
	FunctorPt func ;
} StreamProperty, *StreamPropertyPt ;

static StreamProperty streamProperties[] = {
	{ "alias",				1, HandleAlias },
	{ "encoding",			1, HandleEncoding },
	{ "bom",				1, HandleBom },
	{ "end_of_stream",		1, HandleEndOfStream },
	{ "eof_action",			1, HandleEofAction },
	{ "file_name",			1, HandleFileName },
	{ "file_path",			1, HandleFilePath },
	{ "input",				0, HandleInput },
	{ "mode",				1, HandleMode },
	{ "output",				0, HandleOutput },
	{ "reposition",			1, HandleReposition },
	{ "tty",				1, HandleStreamTty },
	{ "type",				1, HandleStreamType },
	{ nil }
} ;

/*
Missing:

    * alias(A) -- If the stream has an alias then A shall be that alias.
    * position(P) -- If the stream has a a reposition property, P shall be the current stream position,
*/


static StreamPropertyPt GetProperty(Pt t)
{
	FunctorPt f ;
	StreamPropertyPt prop ;
	if( (f = XTermFunctor(t)) == nil ) return nil ;
	for( prop = streamProperties ; prop->name != nil ; prop++ )
		if( prop->func == f ) return prop ;
	return nil ;
}

static Bool CallProperty(StreamPropertyPt prop, StreamPt srm, Hdl args)
{
	return prop->fun(srm, args) ;
}

static void InitStreamProperties(void)
{
	StreamPropertyPt prop ;
	for( prop = streamProperties ; prop->name != nil ; prop++ )
		prop->func = LookupFunctorByName(prop->name, prop->arity) ;
}


/* CXPROLOG C'BUILTINS */

static void PNDStreamProperty()
{
	if( A(2) == tNilAtom ) {			/* init */
		X0 = Drf(X0) ;
		X1 = Drf(X1) ;
		if( IsVar(X0) && IsVar(X1) ) {	/* VAR-VAR init */
			A(2) = zeroIntPt ;
			A(3) = cPt(ExtraGetFirst(streamType)) ;
			A(4) = cPt(cC99Fix(streamProperties)) ;
		}
		elif( IsVar(X0) ) {				/* VAR-PROP init */
			A(2) = oneIntPt ;
			A(3) = cPt(ExtraGetFirst(streamType)) ;
			A(4) = cPt(GetProperty(X1)) ;
			if( A(4) == nil ) Jump(DiscardAndFail) ;
		}
		elif( IsVar(X1) ) {				/* STREAM-VAR init */
			A(2) = twoIntPt ;
			A(3) = cPt(XTestStream(X0, mNone)) ;
			if( A(3) == nil ) Jump(DiscardAndFail) ;
			A(4) = cPt(cC99Fix(streamProperties)) ;
		}
		else {											/* STREAM-PROP handle */
			StreamPt srm = XTestStream(X0, mNone) ;
			StreamPropertyPt prop = GetProperty(X1) ;
			Pt arg ;
			Discard() ;
			MustBe( prop != nil && CallProperty(prop, srm, &arg)
									&& UnifyStruct(X1, prop->func, &arg) ) ;
		}
	}

	if( A(2) == twoIntPt ) {							/* STREAM-VAR handle */
		StreamPt srm = cStreamPt(A(3)) ;
		StreamPropertyPt prop = (StreamPropertyPt)(A(4)) ;
		Pt arg ;
		for( ; prop->name != nil ; prop++ )
			if( CallProperty(prop, srm, &arg)
			  && UnifyVar(X1, MakeStruct(prop->func, &arg)) ) {
				A(4) = cPt(prop+1) ;
				JumpNext() ;
			}
		Jump(DiscardAndFail) ;
	}

	if( A(2) == oneIntPt ) {							/* VAR-PROP handle */
		StreamPt srm = cStreamPt(A(3)) ;
		StreamPropertyPt prop = (StreamPropertyPt)(A(4)) ;
		Pt arg ;
		doseq(srm, srm, ExtraGetNext(srm))
			if( CallProperty(prop, srm, &arg)
			  && UnifyStruct(X1, prop->func, &arg) )
					break ;
		if( srm == nil )
			Jump(DiscardAndFail) ;
		else {
			A(3) = cPt(ExtraGetNext(srm)) ;
			MustBe( UnifyVar(X0, TagExtra(streamType, srm)) ) ;
		}
	}

	if( A(2) == zeroIntPt ) {							/* VAR-VAR handle */
		StreamPt srm = cStreamPt(A(3)) ;
		StreamPropertyPt prop = (StreamPropertyPt)(A(4)) ;
		Pt arg ;
		doseq(srm, srm, ExtraGetNext(srm)) {
			for( ; prop->name != nil ; prop++ ) {
				if( CallProperty(prop, srm, &arg) 
			 	 && UnifyVar(X1, MakeStruct(prop->func, &arg)) ) {
					A(3) = cPt(srm) ;
					A(4) = cPt(prop+1) ;
					MustBe( UnifyVar(X0, TagExtra(streamType, srm)) ) ;
				}
			}
			prop = streamProperties ;
		}
		Jump(DiscardAndFail) ;
	}
}

void StreamPropertyInit()
{
	InitStreamProperties() ;

	InstallGNDeterCBuiltinPred("stream_property", 2, 3, PNDStreamProperty) ;
}
