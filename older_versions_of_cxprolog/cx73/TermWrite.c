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

typedef enum { wNormal, display, quoted, print } WriteMode ;

static WriteMode wMode ;
static int termMaxDepth = 30, listMaxLength = 999 ;
static int currTermDepth ;

static CharPt strPut ;
#define AddCh(c)			(*strPut++ = (c))
#define AddStr(s)			(strcpy(strPut,s), strPut += strlen(s))
#define StrOverflow()		( strPut >= bufferEnd )


/* (a.b) -> [a|b]	[x] == .(x,nil). */

static void AddQStr(CharPt s, Bool dirty)
{
	if( dirty ) {
		AddCh('\'') ;
		while( *s && not StrOverflow() ) {
			if( *s == '\'' )
				AddCh('\'') ;
			AddCh(*s++) ;
		}
		AddCh('\'') ;
	}
	else AddStr(s) ;
}

static void AddName(CharPt name)
{	
	if( wMode != display && wMode != quoted )
		AddStr(name) ;
	else
	  switch( allChars[name[0]] ) {
		case _SO:
		case ',': {
			AddQStr(name, name[1] != '\0') ;
			break ;
		}
		case '[': {
			AddQStr(name, name[1] != ']' || name[2] != '\0') ;
			break ;
		}
		case '{': {
			AddQStr(name, name[1] != '}' || name[2] != '\0') ;
			break ;
		}
		case _LC: {
			Bool dirty = false ;
			CharPt s = name ;
			for( ; *s != '\0' ; s++ )
				if( not cx_isalnum(*s) ) {
					dirty = true ;
					break ;
				}
			AddQStr(name, dirty) ;
			break ;
		}
		default: {
			Bool dirty = false ;
			CharPt s = name ;
			for( ; *s != '\0' ; s++ )
				if( not cx_issymbol(*s) ) {
					dirty = true ;
					break ;
				}
			AddQStr(name, dirty || name[0] == '\0') ;
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
				if( IsAtomOrText(t) )
					AddName(XAtomOrTextName(t)) ;
				elif( IsInt(t) && (p = XInt(t)) >= 0 ) {
					char s[256] ;
					s[0] = 'A' + p % 26 ;
					s[1] = '\0' ;
					if( p > 25 ) sprintf(s + 1, "%d", p / 26) ;
					AddStr(s) ;
				}
				else goto displayL ;
			}
			elif( atom == XAtom(tBracketsAtom) ) {
				AddCh('{') ;
				WriteTerm2(StructArg(st, 0), maxPrec) ;
				AddCh('}') ;
			}
	/*		elif( func == unitParamFunctor ) {
				p = XUnitParam(term) - 1 ;
				WriteTerm2(TagAtom(UnitParam(CurrUnit(), p)), maxPrec) ;
			}*/
			elif( p = Prefix(atom, &rp) ) {
				if( p > m ) AddCh('(') ;
				AddName(AtomName(atom)) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddCh(' ') ;
				WriteTerm2(StructArg(st, 0), rp) ;
				if( p > m ) AddCh(')') ;
			}
			elif( p = Postfix(atom, &lp) ) {
				if( p > m ) AddCh('(') ;
				WriteTerm2(StructArg(st, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddCh(' ') ;
				AddName(AtomName(atom)) ;
				if( p > m ) AddCh(')') ;
			}
			else goto displayL ;
			break ;
		}
		case 2: {
			if( wMode == display ) goto displayL ; 
			elif( (p = Infix(atom, &lp, &rp)) != 0 ) {
				if( p > m ) AddCh('(') ;
				WriteTerm2(XStructArg(term, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddCh(' ') ;
				AddName(AtomName(atom)) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddCh(' ') ;
				WriteTerm2(XStructArg(term, 1), rp) ;
				if( p > m ) AddCh(')') ;
			}
			else goto displayL ;
			break ;
		}
		default: {
displayL:	AddName(AtomName(atom)) ;
			AddCh('(') ;
			dotimes(p, StructArity(st)) {
				if( p != 0 ) AddCh(',') ;
				WriteTerm2(StructArg(st, p), subPrec) ;
			}
			AddCh(')') ;
			break ;
		}
	}
}

static void WriteList(Pt term)
{
	register int i ;
	
	AddCh('[') ;
	dotimes(i, listMaxLength) {
		WriteTerm2(XListHead(term), subPrec) ;
		term = Drf(XListTail(term)) ;
		if( not IsList(term) ) break ;
		AddCh(',') ;
	}
	if( i == listMaxLength )
		WriteTerm2(tEllispisAtom, subPrec) ;
	elif( term != tNilAtom ) {
		AddCh('|') ;
		WriteTerm2(term, subPrec) ;
	}
	AddCh(']') ;
}

static Bool IPortray(Pt term)
{
	return false ;
}

static void WriteTerm2(register Pt term, int p)
{
			/* @@ StrOverflow overflow not properly handled yet */
	if( StrOverflow() ) Error("Term too large") ;
	term = Drf(term) ;
	if( IsAtomOrText(term) ) {
		if( IsText(term) && showText_flag ) AddCh('@') ; 
		AddName(XAtomOrTextName(term)) ;
	}
	elif( IsVar(term) )
		AddStr(VarName(term)) ;
	elif( IsInt(term) )
		{ sprintf(strPut, "%ld", XInt(term)) ; AddStr(strPut) ; }
	elif( IsReal(term) )
		{ sprintf(strPut, "%.5g", XReal(term)) ; AddStr(strPut) ; }
	elif( IsExtra(term) )
		AddStr(XExtraAsStr(term)) ;
	elif( IsList(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( wMode == print && IPortray(term) ) ;
		else { currTermDepth++ ; WriteList(term) ; currTermDepth-- ; }
	}
	elif( IsStruct(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( wMode == print && IPortray(term) ) ;
		else { currTermDepth++ ; WriteStruct(term, p) ; currTermDepth--;}
	}
	else
		{ sprintf(strPut, "<UNKNOWN TAG::%lx>", GetTag(term)) ; AddStr(strPut) ; }
}

static CharPt GWriteTermToStr(Pt term, WriteMode mode)
{
redo:
	strPut = buffer ;
	wMode = mode ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
	if( StrOverflow() ) {
		GrowBuffer() ;
		goto redo ;
	}
	*strPut = '\0' ;
	return buffer ;
}

static void GWriteTerm(Pt term, WriteMode mode, Bool stdOutput)
{
	CharPt s = GWriteTermToStr(term, mode) ;
	if( stdOutput ) SetTempOutput(dflOutputStream) ;
	PutString(s) ;
	if( stdOutput ) RestoreOutput() ;
}

CharPt WriteTermToStr(Pt term)
{
	return GWriteTermToStr(term, quoted) ;
}

void WriteTerm(Pt term)
{
	GWriteTerm(term, quoted, false) ;
}

void WriteTermStd(Pt term)
{
	GWriteTerm(term, quoted, true) ;
}

void WriteDepth(int termDepth, int listLength)
{
	termMaxDepth = termDepth == 0 ? 32000 : termDepth ;
	listMaxLength = listLength == 0 ? 32000 : listLength ;
}


/* CXPROLOG C'BUILTINS */

static void PTell()
{
	Tell(XTestAtomName(X0)) ;
	JumpNext()
}

static void PTelling()
{
	if( UnifyWithAtomic(X0, MakeAtomOrText(Telling())) ) JumpNext()
	DoFail()
}

static void PTold()
{
	Told() ;
	JumpNext()
}

static void PPut()
{
	Put(XTestInt(X0)) ;
	JumpNext()
}

static void PNl()
{
	Nl() ;
	JumpNext()
}

static void PTab()
{
	Tab(XTestInt(X0)) ;
	JumpNext()
}

static void PWrite()
{
	GWriteTerm(X0, wNormal, false) ;
	JumpNext()
}

static void PWriteQ()
{
	GWriteTerm(X0, quoted, false) ;
	JumpNext()
}

static void PPrint()
{
	GWriteTerm(X0, print, false) ;
	JumpNext()
}

static void PDisplay()
{
	GWriteTerm(X0, display, true) ;
	JumpNext()
}

static void PAtomTerm()
{
	Pt t, t0 = Drf(X0) ;
	if( IsAtomOrText(t0) ) {
		if( (t = ReadTermFromStr(XAtomOrTextName(t0), nil)) != nil && Unify(X1, t) )
			JumpNext()
		DoFail()
	}
	elif( IsVar(t0) ) {
		if( UnifyWithAtomic(t0, MakeAtomOrText(WriteTermToStr(X1))) )
			JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PQuote()
{
	CharPt n ;
	n = XTestAtomName(X0) ;
redo:
	strPut = buffer ;
	AddQStr(n, true) ;
	if( StrOverflow() ) {
		GrowBuffer() ;
		goto redo ;
	}
	*strPut = '\0' ;
	if( Unify(X1, MakeAtomOrText(buffer)) ) JumpNext()
	DoFail()
}

void InitTermWrite()
{
	InstallCBuiltinPred("tell", 1, PTell) ;
	InstallCBuiltinPred("telling", 1, PTelling) ;
	InstallCBuiltinPred("told", 0, PTold) ;

	InstallCBuiltinPred("put", 1, PPut) ;
	InstallCBuiltinPred("nl", 0, PNl) ;
	InstallCBuiltinPred("tab", 1, PTab) ;
	InstallCBuiltinPred("write", 1, PWrite) ;
	InstallCBuiltinPred("writeq", 1, PWriteQ) ;
	InstallCBuiltinPred("print", 1, PPrint) ;
	InstallCBuiltinPred("display", 1, PDisplay) ;

	InstallCBuiltinPred("atom_term", 2, PAtomTerm) ;
	InstallCBuiltinPred("quote", 2, PQuote) ;
}
