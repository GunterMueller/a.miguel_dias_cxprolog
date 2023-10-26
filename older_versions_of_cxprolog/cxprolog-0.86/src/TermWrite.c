/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

typedef enum { mNormal, mDisplay, mQuoted, mPrint } WriteMode ;

static WriteMode wMode ;
static Size termMaxDepth, listMaxLength ;
static Size currTermDepth ;
static PredicatePt portrayPred ;

/* (a.b) -> [a|b]	[x] == .(x,nil). */

static StreamPt buffer, output ;

#define AddChar(c)		(StreamPutChar(output, c))
#define AddStr(s)		(StreamPutStr(output, s))

static void AddQStr(CharPt s, Bool dirty)
{
	if( dirty ) {
		AddChar('\'') ;
		while( *s ) {
			if( *s == '\'' )
				AddChar('\'') ;
			AddChar(*s++) ;
		}
		AddChar('\'') ;
	}
	else AddStr(s) ;
}

static void AddAtom(AtomPt atom)
{
	CharPt name = AtomName(atom) ;
	if( wMode != mDisplay && wMode != mQuoted )
		AddStr(name) ;
	else
	  switch( CharType(name[0]) ) {
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
				if( !cx_isalnum(*s) ) {
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
				if( !cx_issymbol(*s) ) {
					dirty = true ;
					break ;
				}
			AddQStr(name, dirty || name[0] == '\0') ;
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
					Str16 s ;
					s[0] = 'A' + p % 26 ;
					s[1] = '\0' ;
					if( p > 25 ) sprintf(s + 1, "%d", p / 26) ;
					AddStr(s) ;
				}
				else goto displayL ;
			}
			elif( atom == XAtom(tBracketsAtom) ) {
				AddChar('{') ;
				WriteTerm2(XStructArg(term, 0), maxPrec) ;
				AddChar('}') ;
			}
			elif( atom == XAtom(tStringAtom) ) {
				AddChar('"') ;
				WriteTerm2(XStructArg(term, 0), maxPrec) ;
				AddChar('"') ;
			}
			elif( func == unitParamFunctor ) {
				AddStr(XAtomName(UnitParam(CurrUnit(), XUnitParam(term)))) ;
			}
			elif( func == metaCutFunctor ) {
				AddChar('!') ;
			}
            elif( (p = Prefix(atom, &rp)) != 0 ) {
				if( p > m ) AddChar('(') ;
				AddAtom(atom) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddChar(' ') ;
				WriteTerm2(XStructArg(term, 0), rp) ;
				if( p > m ) AddChar(')') ;
			}
			elif( (p = Postfix(atom, &lp)) != 0 ) {
				if( p > m ) AddChar('(') ;
				WriteTerm2(XStructArg(term, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddChar(' ') ;
				AddAtom(atom) ;
				if( p > m ) AddChar(')') ;
			}
			else goto displayL ;
			break ;
		}
		case 2: {
			if( wMode == mDisplay ) goto displayL ; 
			elif( (p = Infix(atom, &lp, &rp)) != 0 ) {
				if( p > m ) AddChar('(') ;
				WriteTerm2(XStructArg(term, 0), lp) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddChar(' ') ;
				AddAtom(atom) ;
				if( cx_isalpha(AtomName(atom)[0]) ) AddChar(' ') ;
				WriteTerm2(XStructArg(term, 1), rp) ;
				if( p > m ) AddChar(')') ;
			}
			else goto displayL ;
			break ;
		}
		default: {
displayL:	AddAtom(atom) ;
			AddChar('(') ;
			dotimes(p, XStructArity(term)) {
				if( p != 0 ) AddChar(',') ;
				WriteTerm2(XStructArg(term, p), subPrec) ;
			}
			AddChar(')') ;
			break ;
		}
	}
}

static void WriteList(Pt term)
{
	register Size i ;	
	AddChar('[') ;
	dotimes(i, listMaxLength) {
		WriteTerm2(XListHead(term), subPrec) ;
		term = Drf(XListTail(term)) ;
		if( !IsList(term) ) break ;
		AddChar(',') ;
	}
	if( i == listMaxLength )
		AddStr("...") ;
	elif( term != tNilAtom ) {
		AddChar('|') ;
		WriteTerm2(term, subPrec) ;
	}
	AddChar(']') ;
}

static Bool Portray(Pt term)
{
	Bool saveWMode, res ;
	Size saveCurrTermDepth ;
	StreamPt saveOutput ;
	if( PredIsUndefined(portrayPred) ) return false  ;
	saveWMode = wMode ;
	saveCurrTermDepth = currTermDepth ;
	saveOutput = output ;
	res = CallProlog(MakeUnStruct(PredFunctor(portrayPred),term)) ;
	output = saveOutput ;
	currTermDepth = saveCurrTermDepth ;
	wMode = saveWMode ;
	return res  ;
}

static void WriteTerm2(register Pt term, int p)
{
	term = Drf(term) ;
	if( IsAtom(term) )
		if( wMode == mPrint && Portray(term) ) ;
		else AddAtom(XAtom(term)) ;
	elif( IsVar(term) )
		AddStr(VarName(term)) ;
	elif( IsNumber(term) )
		if( wMode == mPrint && Portray(term) ) ;
		else AddStr(XNumberAsStr(term)) ;
	elif( IsExtra(term) )
		if( wMode == mPrint && Portray(term) ) ;
		else AddStr(XExtraAsStr(term)) ;
	elif( IsStruct(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( wMode == mPrint && Portray(term) ) ;
		else { currTermDepth++ ; WriteStruct(term, p) ; currTermDepth--;}
	}
	elif( IsList(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( wMode == mPrint && Portray(term) ) ;
		else { currTermDepth++ ; WriteList(term) ; currTermDepth-- ; }
	}
	else
		StreamWrite(output, "<UNKNOWN TAG::%lx>", GetTag(term)) ;
}

static void WriteTermMode(StreamPt srm, Pt term, WriteMode mode)
{
	output = srm ;
	wMode = mode ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
}

static CharPt TermAsStrMode(Pt term, WriteMode mode)
{
	WriteTermMode(BufferStreamOpen(), term, mode) ;
	return StreamClose(output) ;
}


/* PUBLIC */

void WriteTerm(StreamPt srm, Pt term)
{
	WriteTermMode(srm, term, mNormal) ;
}

CharPt TermAsStr(Pt term)
{
	return TermAsStrMode(term, mNormal) ;
}

void TermToBuffer(Pt term)
{
	WriteTermMode(buffer, term, mNormal) ;
}

void SetWriteDepth(Size termDepth, Size listLength)
{
	termMaxDepth = termDepth == 0 ? LONG_MAX : termDepth ;
	listMaxLength = listLength == 0 ? LONG_MAX : listLength ;
}


/* CXPROLOG C'BUILTINS */

static void PWrite()
{
	WriteTermMode(currOut, X0, mNormal) ;
	JumpNext()
}

static void PSWrite()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mNormal) ;
	JumpNext()
}

static void PWriteln()
{
	WriteTermMode(currOut, X0, mNormal) ;
	StreamPutChar(currOut, '\n') ;
	JumpNext()
}

static void PSWriteln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mNormal) ;
	StreamPutChar(srm, '\n') ;
	JumpNext()
}

static void PWriteQ()
{
	WriteTermMode(currOut, X0, mQuoted) ;
	JumpNext()
}

static void PSWriteQ()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mQuoted) ;
	JumpNext()
}

static void PWriteQln()
{
	WriteTermMode(currOut, X0, mQuoted) ;
	StreamPutChar(currOut, '\n') ;
	JumpNext()
}

static void PSWriteQln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mQuoted) ;
	StreamPutChar(srm, '\n') ;
	JumpNext()
}

static void PPrint()
{
	WriteTermMode(currOut, X0, mPrint) ;
	JumpNext()
}

static void PSPrint()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mPrint) ;
	JumpNext()
}

static void PDisplay()
{
	WriteTermMode(userOut, X0, mDisplay) ;
	JumpNext()
}

static void PDisplayln()
{
	WriteTermMode(userOut, X0, mDisplay) ;
	StreamPutChar(userOut, '\n') ;
	JumpNext()
}

static void PAtomTerm()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
        Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
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
	else TypeError2("ATOM or VAR", t0) ;
}

static void PAtomTermQ()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
        Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
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
	else TypeError2("ATOM or VAR", t0) ;
}

static void PQuote()
{
	CharPt n = XTestAtomName(X0) ;
	output = BufferStreamOpen() ;
	AddQStr(n, true) ;
	if( Unify(X1, MakeTempAtom(StreamClose(output))) ) JumpNext()
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
	buffer = BufferStreamOpen() ;
	StreamClose(buffer) ;

	portrayPred =  LookupPredicateByName("portray", 1) ;
	PredIsMutableBuiltin(portrayPred) = true ;

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
