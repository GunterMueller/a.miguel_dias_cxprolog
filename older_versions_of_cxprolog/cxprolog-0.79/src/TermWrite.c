/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL

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

typedef enum { mNormal, mDisplay, mQuoted, mPrint } WriteMode ;

static WriteMode wMode ;
static Size termMaxDepth, listMaxLength ;
static Size currTermDepth ;

/* (a.b) -> [a|b]	[x] == .(x,nil). */

static void AddAtom(AtomPt atom)
{
	CharPt name = AtomName(atom) ;
	if( wMode != mDisplay && wMode != mQuoted )
		BufferAddStr(name) ;
	else
	  switch( CharType(name[0]) ) {
		case _SO:
		case ',': {
			BufferAddQStr(name, name[1] != '\0') ;
			break ;
		}
		case '[': {
			BufferAddQStr(name, name[1] != ']' || name[2] != '\0') ;
			break ;
		}
		case '{': {
			BufferAddQStr(name, name[1] != '}' || name[2] != '\0') ;
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
			BufferAddQStr(name, dirty) ;
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
			BufferAddQStr(name, dirty || name[0] == '\0') ;
			break ;
		}
	}
}

static void WriteTerm2(register Pt term, int p) ;

static void WriteStruct(register Pt term, int m)
{
	register FunctorPt func = XStructFunctor(term) ;
	register AtomPt atom = FunctorAtom(func) ;
	int p, lp, rp ;	
	switch( FunctorArity(func) ) {
		case 0: InternalError("WriteStruct") ;
		case 1: {
			if( wMode == mDisplay ) goto displayL ; 
			elif( func == varFunctor ) {
				Pt t = Drf(XStructArg(term, 0)) ;
				if( IsAtom(t) )
					AddAtom(XAtom(t)) ;
				elif( IsInt(t) && (p = XInt(t)) >= 0 ) {
					Char s[256] ;
					s[0] = 'A' + p % 26 ;
					s[1] = '\0' ;
					if( p > 25 ) sprintf(s + 1, "%d", p / 26) ;
					BufferAddStr(s) ;
				}
				else goto displayL ;
			}
			elif( atom == XAtom(tBracketsAtom) ) {
				BufferAddCh('{') ;
				WriteTerm2(XStructArg(term, 0), maxPrec) ;
				BufferAddCh('}') ;
			}
			elif( atom == XAtom(tStringAtom) ) {
				BufferAddCh('"') ;
				WriteTerm2(XStructArg(term, 0), maxPrec) ;
				BufferAddCh('"') ;
			}
	/*		elif( func == unitParamFunctor ) {
				p = XUnitParam(term) - 1 ;
				WriteTerm2(TagAtom(UnitParam(CurrUnit(), p)), maxPrec) ;
			}*/
			elif( (p = Prefix(atom, &rp)) != 0 ) {
				if( p > m ) BufferAddCh('(') ;
				AddAtom(atom) ;
				if( cx_isalpha(AtomName(atom)[0]) ) BufferAddCh(' ') ;
				WriteTerm2(XStructArg(term, 0), rp) ;
				if( p > m ) BufferAddCh(')') ;
			}
			elif( (p = Postfix(atom, &lp)) != 0 ) {
				if( p > m ) BufferAddCh('(') ;
				WriteTerm2(XStructArg(term, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) BufferAddCh(' ') ;
				AddAtom(atom) ;
				if( p > m ) BufferAddCh(')') ;
			}
			else goto displayL ;
			break ;
		}
		case 2: {
			if( wMode == mDisplay ) goto displayL ; 
			elif( (p = Infix(atom, &lp, &rp)) != 0 ) {
				if( p > m ) BufferAddCh('(') ;
				WriteTerm2(XStructArg(term, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) BufferAddCh(' ') ;
				AddAtom(atom) ;
				if( cx_isalpha(AtomName(atom)[0]) ) BufferAddCh(' ') ;
				WriteTerm2(XStructArg(term, 1), rp) ;
				if( p > m ) BufferAddCh(')') ;
			}
			else goto displayL ;
			break ;
		}
		default: {
displayL:	AddAtom(atom) ;
			BufferAddCh('(') ;
			dotimes(p, XStructArity(term)) {
				if( p != 0 ) BufferAddCh(',') ;
				WriteTerm2(XStructArg(term, p), subPrec) ;
			}
			BufferAddCh(')') ;
			break ;
		}
	}
}

static void WriteList(Pt term)
{
	register Size i ;	
	BufferAddCh('[') ;
	dotimes(i, listMaxLength) {
		WriteTerm2(XListHead(term), subPrec) ;
		term = Drf(XListTail(term)) ;
		if( not IsList(term) ) break ;
		BufferAddCh(',') ;
	}
	if( i == listMaxLength )
		BufferAddStr("...") ;
	elif( term != tNilAtom ) {
		BufferAddCh('|') ;
		WriteTerm2(term, subPrec) ;
	}
	BufferAddCh(']') ;
}

static Bool IPortray(Pt term)
{
	return false ;
}

static void WriteTerm2(register Pt term, int p)
{
	term = Drf(term) ;
	if( IsAtom(term) )
		AddAtom(XAtom(term)) ;
	elif( IsVar(term) )
		BufferAddStr(VarName(term)) ;
	elif( IsNumber(term) )
		BufferAddStr(XNumberAsStr(term)) ;
	elif( IsExtra(term) )
		BufferAddStr(XExtraAsStr(term)) ;
	elif( IsList(term) ) {
		if( currTermDepth > termMaxDepth ) BufferAddStr("...") ;
		elif( wMode == mPrint && IPortray(term) ) ;
		else { currTermDepth++ ; WriteList(term) ; currTermDepth-- ; }
	}
	elif( IsStruct(term) ) {
		if( currTermDepth > termMaxDepth ) BufferAddStr("...") ;
		elif( wMode == mPrint && IPortray(term) ) ;
		else { currTermDepth++ ; WriteStruct(term, p) ; currTermDepth--;}
	}
	else
		BufferWrite("<UNKNOWN TAG::%lx>", GetTag(term)) ;
}

static CharPt TermAsStrMode(Pt term, WriteMode mode)
{
	UseBuffer() ;
	wMode = mode ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
	BufferAddCh('\0') ;
	return FreeBuffer() ;
}

/* PUBLIC */

CharPt TermAsStr(Pt term)
{
	return TermAsStrMode(term, mNormal) ;
}

void SetWriteDepth(Size termDepth, Size listLength)
{
	termMaxDepth = termDepth == 0 ? LONG_MAX : termDepth ;
	listMaxLength = listLength == 0 ? LONG_MAX : listLength ;
}


/* CXPROLOG C'BUILTINS */

static void PTell()
{
	Tell(XTestAtomName(X0)) ;
	JumpNext()
}

static void PTelling()
{
	if( UnifyWithAtomic(X0, MakeTempAtom(StreamName(currOut))) ) JumpNext()
	DoFail()
}

static void PTold()
{
	StreamClose(currOut) ;
	JumpNext()
}

static void PFlush()
{
	StreamFlush(currOut) ;
	JumpNext()
}

static void PSFlush()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamFlush(srm) ;
	JumpNext()
}

static void PPut()
{
	StreamPut(currOut, XTestInt(X0)) ;
	JumpNext()
}

static void PSPut()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamPut(srm, XTestInt(X1)) ;
	JumpNext()
}

static void PNl()
{
	StreamPut(currOut, '\n') ;
	JumpNext()
}

static void PSNl()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamPut(srm, '\n') ;
	JumpNext()
}

static void PTab()
{
	Size n = XTestInt(X0) ; ;
	while( n-- )
		StreamPut(currOut, ' ') ;
}

static void PSTab()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	Size n = XTestInt(X1) ; ;
	while( n-- )
		StreamPut(srm, ' ') ;
}

static void PWrite()
{
	StreamWrite(currOut, "%s", TermAsStrMode(X0, mNormal)) ;
	JumpNext()
}

static void PSWrite()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamWrite(srm, "%s", TermAsStrMode(X1, mNormal)) ;
	JumpNext()
}

static void PWriteln()
{
	StreamWrite(currOut, "%s\n", TermAsStrMode(X0, mNormal)) ;
	JumpNext()
}

static void PSWriteln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamWrite(srm, "%s\n", TermAsStrMode(X1, mNormal)) ;
	JumpNext()
}

static void PWriteQ()
{
	StreamWrite(currOut, "%s", TermAsStrMode(X0, mQuoted)) ;
	JumpNext()
}

static void PSWriteQ()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamWrite(srm, "%s", TermAsStrMode(X1, mQuoted)) ;
	JumpNext()
}

static void PWriteQln()
{
	StreamWrite(currOut, "%s\n", TermAsStrMode(X0, mQuoted)) ;
	JumpNext()
}

static void PSWriteQln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamWrite(srm, "%s\n", TermAsStrMode(X1, mQuoted)) ;
	JumpNext()
}

static void PPrint()
{
	StreamWrite(currOut, "%s", TermAsStrMode(X0, mPrint)) ;
	JumpNext()
}

static void PSPrint()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	StreamWrite(srm, "%s", TermAsStrMode(X1, mPrint)) ;
	JumpNext()
}

static void PDisplay()
{
	StreamWrite(userOut, "%s", TermAsStrMode(X0, mDisplay)) ;
	JumpNext()
}

static void PDisplayln()
{
	StreamWrite(userOut, "%s\n", TermAsStrMode(X0, mDisplay)) ;
	JumpNext()
}

static void PAtomTerm()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = ZReadTermFromStr(XAtomName(t0)) ;
		if( t == eofPt ) t = tEmptyAtom ;
		if( t != nil && Unify(X1, t) )
			JumpNext()
		DoFail()
	}
	elif( IsVar(t0) ) {
		if( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrMode(X1, mNormal))) )
			JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PAtomTermQ()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = ZReadTermFromStr(XAtomName(t0)) ;
		if( t == eofPt ) t = tEmptyAtom ;
		if( t != nil && Unify(X1, t) )
			JumpNext()
		DoFail()
	}
	elif( IsVar(t0) ) {
		if( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrMode(X1, mQuoted))) )
			JumpNext()
		DoFail()
	}
	else Error("Invalid arguments") ;
}

static void PQuote()
{
	CharPt n ;
	n = XTestAtomName(X0) ;
	UseBuffer() ;
	BufferAddQStr(n, true) ;
	BufferAddCh('\0') ;
	if( Unify(X1, MakeTempAtom(FreeBuffer())) ) JumpNext()
	DoFail()
}

static void PWriteDepth(void)
{
	SetWriteDepth(XTestNat(X0), XTestNat(X1)) ;
	JumpNext()
}


void TermWriteInit()
{
	SetWriteDepth(30,999) ;

	InstallCBuiltinPred("tell", 1, PTell) ;
	InstallCBuiltinPred("telling", 1, PTelling) ;
	InstallCBuiltinPred("told", 0, PTold) ;
	InstallCBuiltinPred("flush", 0, PFlush) ;
	InstallCBuiltinPred("flush", 1, PSFlush) ;

	InstallCBuiltinPred("put", 1, PPut) ;
	InstallCBuiltinPred("put", 2, PSPut) ;
	InstallCBuiltinPred("nl", 0, PNl) ;
	InstallCBuiltinPred("nl", 1, PSNl) ;
	InstallCBuiltinPred("tab", 1, PTab) ;
	InstallCBuiltinPred("tab", 2, PSTab) ;

	InstallCBuiltinPred("write", 1, PWrite) ;
	InstallCBuiltinPred("write", 2, PSWrite) ;
	InstallCBuiltinPred("writeln", 1, PWriteln) ;
	InstallCBuiltinPred("writeln", 2, PSWriteln) ;
	InstallCBuiltinPred("writeq", 1, PWriteQ) ;
	InstallCBuiltinPred("writeq", 2, PSWriteQ) ;
	InstallCBuiltinPred("writeqln", 1, PWriteQln) ;
	InstallCBuiltinPred("writeqln", 2, PSWriteQln) ;
	InstallCBuiltinPred("print", 1, PPrint) ;
	InstallCBuiltinPred("print", 2, PSPrint) ;
	InstallCBuiltinPred("display", 1, PDisplay) ;
	InstallCBuiltinPred("displayln", 1, PDisplayln) ;

	InstallCBuiltinPred("atom_term", 2, PAtomTerm) ;
	InstallCBuiltinPred("atom_termq", 2, PAtomTermQ) ;
	InstallCBuiltinPred("quote", 2, PQuote) ;
	
	InstallCBuiltinPred("write_depth", 2, PWriteDepth) ;
}
