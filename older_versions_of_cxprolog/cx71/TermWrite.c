/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL

 *   CxProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   CxProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with CxProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "CxProlog.h"

static WriteMode wMode ;
static int termMaxDepth = 30, listMaxLength = 999 ;
static int currTermDepth ;
static Bool silent = false ;

/* (a.b) -> [a|b]	[x] == .(x,nil). */

CharPt Quote(CharPt a)
{
	CharPt z = strBuffer ;

	*z++ = '\'' ;
	while( *a ) {
		if( *a == '\'' )
			*z++ = *a ;
		*z++ = *a++ ;
	}
	*z++ = '\'' ;
	*z++ = '\0' ;
	return strBuffer ;
}

static void PutQString(CharPt s, Bool dirty)
{
	PutString(dirty ? Quote(s) : s) ;
}

static void PutAtom(AtomPt at)
{
	register CharPt s = AtomName(at) ;

	if( wMode != display && wMode != quoted ) PutString(s) ;
	else switch( allChars[s[0]] ) {
		case _SO:
		case ',': {
			PutQString(s, s[1] != '\0') ;
			break ;
		}
		case '[': {
			PutQString(s, s[1] != ']' || s[2] != '\0') ;
			break ;
		}
		case '{': {
			PutQString(s, s[1] != '}' || s[2] != '\0') ;
			break ;
		}
		case _LC: {
			Bool dirty ;

			for( dirty = false ; *s != '\0' ; s++ )
				if( not cx_isalnum(*s) ) {
					dirty = true ;
					break ;
				}
			PutQString(AtomName(at), dirty) ;
			break ;
		}
		default: {
			Bool dirty ;

			for( dirty = false ; *s != '\0' ; s++ )
				if( not cx_issymbol(*s) ) {
					dirty = true ;
					break ;
				}
			PutQString(AtomName(at), dirty) ;
			break ;
		}
	}
}

static void WriteTerm2(register Pt term, int p) ;

static void WriteStruct(Pt term, int m)
{
	register StructPt st = XStruct(term) ;
	register FunctorPt func = StructFunctor(st) ;
	register AtomPt atom = FunctorAtom(func) ;
	int p, lp, rp ;
	
	switch( FunctorArity(func) ) {
		case 0: InternalError("WriteStruct") ;
		case 1: {
			if( wMode == display ) goto displayL ; 
			elif( func == varFunctor ) {
				Pt t = Drf(XStructArg(term, 0)) ;
				if( IsAtom(t) )
					PutAtom(XAtom(t)) ;
				elif( IsInt(t) && (p = XInt(t)) >= 0 ) {
					char s[256] ;
					s[0] = 'A' + p % 26 ;
					s[1] = '\0' ;
					if( p > 25 ) sprintf(s + 1, "%d", p / 26) ;
					PutString(s) ;
				}
				else goto displayL ;
			}
			elif( atom == XAtom(tBracketsAtom) ) {
				Put('{') ;
				WriteTerm2(StructArg(st, 0), maxPrec) ;
				Put('}') ;
			}
	/*		elif( func == unitParamFunctor ) {
				p = XUnitParam(term) - 1 ;
				WriteTerm2(TagAtom(UnitParam(CurrUnit(), p)), maxPrec) ;
			}*/
			elif( p = Prefix(atom, &rp) ) {
				if( p > m ) Put('(') ;
				PutAtom(atom) ;
				if( cx_isalpha(AtomName(atom)[0]) ) Put(' ') ;
				WriteTerm2(StructArg(st, 0), rp) ;
				if( p > m ) Put(')') ;
			}
			elif( p = Postfix(atom, &lp) ) {
				if( p > m ) Put('(') ;
				WriteTerm2(StructArg(st, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) Put(' ') ;
				PutAtom(atom) ;
				if( p > m ) Put(')') ;
			}
			else goto displayL ;
			break ;
		}
		case 2: {
			if( wMode == display ) goto displayL ; 
			elif( (p = Infix(atom, &lp, &rp)) != 0 ) {
				if( p > m ) Put('(') ;
				WriteTerm2(XStructArg(term, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) Put(' ') ;
				PutAtom(atom) ;
				if( cx_isalpha(AtomName(atom)[0]) ) Put(' ') ;
				WriteTerm2(XStructArg(term, 1), rp) ;
				if( p > m ) Put(')') ;
			}
			else goto displayL ;
			break ;
		}
		default: {
displayL:	PutAtom(atom) ;
			Put('(') ;
			dotimes(p, StructArity(st)) {
				if( p != 0 ) Put(',') ;
				WriteTerm2(StructArg(st, p), subPrec) ;
			}
			Put(')') ;
			break ;
		}
	}
}

static void WriteList(Pt term)
{
	register int i ;
	
	Put('[') ;
	dotimes(i, listMaxLength) {
		WriteTerm2(XListHead(term), subPrec) ;
		term = Drf(XListTail(term)) ;
		if( not IsList(term) ) break ;
		Put(',') ;
	}
	if( i == listMaxLength )
		WriteTerm2(MakeAtom("..."), subPrec) ;
	elif( term != tNilAtom ) {
		Put('|') ;
		WriteTerm2(term, subPrec) ;
	}
	Put(']') ;
}

static Bool IPortray(Pt term)
{
	return false ;
}

static void WriteTerm2(register Pt term, int p)
{	
	term = Drf(term) ;
	if( IsAtom(term) )
		PutAtom(XAtom(term));
	elif( IsVar(term) )
		PutString(VarName(term)) ;
	elif( IsInt(term) )
		{ sprintf(strBuffer, "%ld", XInt(term)) ; PutString(strBuffer) ; }
	elif( IsReal(term) )
		{ sprintf(strBuffer, "%.5g", XReal(term)) ; PutString(strBuffer) ; }
	elif( IsExtra(term) )
		WriteExtra(term, wMode == quoted || wMode == display) ;
	elif( IsList(term) ) {
		if( currTermDepth > termMaxDepth ) PutAtom(LookupAtom("...")) ;
		elif( wMode == print && IPortray(term) ) ;
		else { currTermDepth++ ; WriteList(term) ; currTermDepth-- ; }
	}
	elif( IsStruct(term) ) {
		if( currTermDepth > termMaxDepth ) PutAtom(LookupAtom("...")) ;
		elif( wMode == print && IPortray(term) ) ;
		else { currTermDepth++ ; WriteStruct(term, p) ; currTermDepth--;}
	}
	else
		{ sprintf(strBuffer, "<UNKNOWN TAG::%lx>", GetTag(term)) ; PutString(strBuffer) ; }
}

void GWriteTerm(Pt term, register WriteMode mode, Bool stdOutput)
{
	wMode = mode ;
	if( stdOutput ) SetTempOutput(dflOutputStream) ;
	WriteTerm2(term, maxPrec) ;
	if( stdOutput ) RestoreOutput() ;
}

void WriteTerm(Pt term)
{
	wMode = wNormal ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
}

void WritelnTerm(Pt term)
{
	wMode = wNormal ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
	Nl() ;
}

void WriteTermStd(Pt term)
{
	wMode = wNormal ;
	SetTempOutput(dflOutputStream) ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
	RestoreOutput() ;
}

void WriteDepth(int termDepth, int listLength)
{
	termMaxDepth = termDepth == 0 ? 32000 : termDepth ;
	listMaxLength = listLength == 0 ? 32000 : listLength ;
}

void SetSilent(Bool b)
{
    silent = b ;
}

Bool CheckSilent(void)
{
   return silent ;
}

