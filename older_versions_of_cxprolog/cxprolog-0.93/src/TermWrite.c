/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
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

typedef enum { mNormal, mDisplay, mQuoted, mPrint } WriteMode ;

static WriteMode wMode ;
static Size termMaxDepth, listMaxLength ;
static Size currTermDepth ;
static PredicatePt portrayPred ;

/* (a.b) -> [a|b]	[x] == .(x,nil). */

static StreamPt output ;

#define AddChar(c)		(StreamPut(output, c))
#define AddStr(s)		(StreamPutStr(output, s))

static void AddQStr(CharPt s, Bool dirty, Bool opClashL, Bool opClashR)
{
	if( dirty ) {
		AddChar('\'') ;
		while( *s ) {
			if( *s == '\'' )
				AddChar('\'') ;
			AddChar(CharDecode(s)) ;
		}
		AddChar('\'') ;
	}
	else {
		if( opClashL && !cx_isalnum(CharFirst(s)) ) AddChar(' ') ;
		AddStr(s) ;
		if( opClashR && !cx_isalnum(CharLast(s)) ) AddChar(' ') ;
	}
}

static void AddAtom(AtomPt atom, Bool opClashL, Bool opClashR)
{
	CharPt name = AtomName(atom) ;
	Bool dirty = false ;
	if( wMode == mDisplay || wMode == mQuoted ) {
		CharPt s = name ;
		int first = CharDecode(s) ;
		switch( CharType(first) ) {
			case _SO: dirty = s[0] != '\0' ; break ;
			case '[': dirty = s[0] != ']' || s[1] != '\0' ; break ;
			case '{': dirty = s[0] != '}' || s[1] != '\0' ; break ;
			default:  dirty = true ; break ;
			case _LC: {
				while( *s != '\0' ) {
					int c = CharDecode(s) ;
					if( !cx_isalnum(c) ) {
						dirty = true ;
						break ;
					}
				}
				break ;
			}
			case _SY: {
				while( *s != '\0' ) {
					int c = CharDecode(s) ;
					if( !cx_issymbol(c) ) {
						dirty = true ;
						break ;
					}
				}
				break ;
			}
		}
	}
	AddQStr(name, dirty, opClashL, opClashR) ;
}

static void DoWriteTerm(register Pt term, int p, Bool opClashL, Bool opClashR, Bool parAtom) ;

static void WriteStruct(register Pt term, int m, Bool opClashL, Bool opClashR)
{
	register FunctorPt func = XStructFunctor(term) ;
	register AtomPt atom = FunctorAtom(func) ;
	int p, lp, rp ;	
	switch( FunctorArity(func) ) {
		case 0: InternalError("WriteStruct") ;
		case 1: {
			if( wMode == mDisplay ) goto displayL ; 
			elif( func == varFunctor ) {
				Pt t = DrfChecked(XStructArg(term, 0)) ;
				if( IsAtom(t) )
					AddAtom(XAtom(t), false, false) ;
				elif( IsInt(t) ) {
					PInt i = XInt(t) ;
					if( i >= 0 ) {
						AddChar('A' + i % 26) ;
						if( i >= 26 )
							AddStr(GStrFormat("%d", i / 26)) ;
					}
					else goto displayL ;
				}
				else goto displayL ;
			}
			elif( func == bracketsFunctor ) {
				AddChar('{') ;
				DoWriteTerm(XStructArg(term, 0), maxPrec, false, false, false) ;
				AddChar('}') ;
			}
			elif( func == stringFunctor ) {
				AddChar('"') ;
				DoWriteTerm(XStructArg(term, 0), maxPrec, false, false, false) ;
				AddChar('"') ;
			}
			elif( func == unitParamFunctor ) {
				AddStr(XAtomName(UnitParam(CurrUnit(), XUnitParam(term)))) ; /*@@@*/
			}
			elif( func == metaCutFunctor ) {
				AddChar('!') ;
			}
			elif( (p = Prefix(atom, &rp)) != 0 ) {
				if( p > m ) { AddChar('(') ; opClashL = opClashR = false ; }
				if( cx_isalpha(CharFirst(AtomName(atom))) ) {
					AddAtom(atom, opClashL, false) ;
					AddChar(' ') ;	/* ex: a->not not b */
					DoWriteTerm(XStructArg(term, 0), rp, false, opClashR, false) ;
				}
				else {
					AddAtom(atom, opClashL, false) ;	/* ex: a-> -b */
 					DoWriteTerm(XStructArg(term, 0), rp, true, opClashR, false) ;
				}
				if( p > m ) AddChar(')') ;
			}
			elif( (p = Postfix(atom, &lp)) != 0 ) {
				if( p > m ) { AddChar('(') ; opClashL = opClashR = false ; }
				if( cx_isalpha(CharFirst(AtomName(atom))) ) {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, false, false) ;
					AddChar(' ') ;
					AddAtom(atom, false, opClashR) ;
				}
				else {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, true, false) ;
					AddAtom(atom, false, opClashR) ; /* ex: a! ->b */
 				}
				if( p > m ) AddChar(')') ;
			}
			else goto displayL ;
			break ;
		}
		case 2: {
			if( wMode == mDisplay ) goto displayL ; 
			elif( (p = Infix(atom, &lp, &rp)) != 0 ) {
				if( p > m ) AddChar('(') ;
				if( cx_isalpha(CharFirst(AtomName(atom))) ) {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, false, true) ;
					AddChar(' ') ;
					AddAtom(atom, false, false) ;
					AddChar(' ') ;
					DoWriteTerm(XStructArg(term, 1), rp, false, opClashR, false) ;
				}
				else {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, true, true) ;
					if( func == commaFunctor ) {
						AddChar(',') ;
						if( extraSpacesInTerms_flag ) AddChar(' ') ;
					}
					else AddAtom(atom, false, false) ;
					DoWriteTerm(XStructArg(term, 1), rp, true, opClashR, false) ;
				}
				if( p > m ) AddChar(')') ;
			}
			else goto displayL ;
			break ;
		}
		default: {
			int i ;
displayL:	AddAtom(atom, false, false) ;
			AddChar('(') ;
			dotimes(i, XStructArity(term)) {
				if( i != 0 ) {
					AddChar(',') ;
					if( extraSpacesInTerms_flag ) AddChar(' ') ;
				}
				DoWriteTerm(XStructArg(term, i), subPrec, false, false, false) ;
			}
			AddChar(')') ;
			break ;
		}
	}
}

static void DisplayList(Pt term)
{
	AddChar('.') ;
	AddChar('(') ;
	DoWriteTerm(XListHead(term), subPrec, false, false, false) ;
	AddChar(',') ;
	if( extraSpacesInTerms_flag ) AddChar(' ') ;
	DoWriteTerm(XListTail(term), subPrec, false, false, false) ;
	AddChar(')') ;
}
	
static void WriteList(Pt term)
{
	register Size i ;	
	AddChar('[') ;
	dotimes(i, listMaxLength) {
		DoWriteTerm(XListHead(term), subPrec, false, false, false) ;
		term = DrfChecked(XListTail(term)) ;
		if( !IsList(term) ) break ;
		AddChar(',') ;
		if( extraSpacesInTerms_flag ) AddChar(' ') ;
	}
	if( i == listMaxLength )
		AddStr("...") ;
	elif( term != tNilAtom ) {
		AddChar('|') ;
		if( extraSpacesInTerms_flag ) AddChar(' ') ;
		DoWriteTerm(term, subPrec, false, false, false) ;
	}
	AddChar(']') ;
}

static Bool Portray(Pt term)
{
	Bool saveWMode, res ;
	Size saveCurrTermDepth ;
	StreamPt saveOutput ;
	if( !PredHasClauses(portrayPred) ) return false ;
	saveWMode = wMode ;
	saveCurrTermDepth = currTermDepth ;
	saveOutput = output ;
	res = CallProlog(MakeUnStruct(PredFunctor(portrayPred),term)) ;
	output = saveOutput ;
	currTermDepth = saveCurrTermDepth ;
	wMode = saveWMode ;
	return res ;
}

static void DoWriteTerm(register Pt term, int p, Bool opClashL, Bool opClashR, Bool parAtom)
{
	int q, rq ;
	term = DrfChecked(term) ;
	if( IsAtom(term) )
		if( wMode == mPrint && Portray(term) ) ;
		else {
			AtomPt a = XAtom(term) ;
			if( parAtom && (q = Prefix(a, &rq)) != 0 )
				{ AddChar('(') ; AddAtom(a, false, false) ; AddChar(')') ; }
			else AddAtom(a, opClashL, opClashR) ;
		}
	elif( IsVar(term) )
		AddStr(VarName(term)) ;
	elif( IsNumber(term) )
		if( wMode == mPrint && Portray(term) ) ;
		else {
			CharPt s = XNumberAsStr(term) ;
			if( opClashL && s[0] == '-' ) { /* ex: >(-1) */
				AddChar('(') ; AddStr(s) ; AddChar(')') ;
			}
			else AddStr(s) ;
		}
	elif( IsExtra(term) )
		if( wMode == mPrint && Portray(term) ) ;
		else AddStr(XExtraAsStr(term)) ;
	elif( IsStruct(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( wMode == mPrint && Portray(term) ) ;
		else { currTermDepth++ ;
			   WriteStruct(term, p, opClashL, opClashR) ;
			   currTermDepth--;
		}
	}
	elif( IsList(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( wMode == mPrint && Portray(term) ) ;
		elif( wMode == mDisplay ) {
			currTermDepth++ ;
			DisplayList(term) ;
			currTermDepth-- ;
		}	
		else {
			currTermDepth++ ;
			WriteList(term) ;
			currTermDepth-- ;
		}
	}
	else
		StreamWrite(output, "<UNKNOWN TAG::%lx>", GetTag(term)) ;
}

static void WriteSubtermMode(StreamPt srm, Pt subterm, Pt term, WriteMode mode)
{
	output = srm ;
	wMode = mode ;
	currTermDepth = 0 ;
	PrepareDrfChecked(term)	 ;

	/* Centralized call point */
	DoWriteTerm(subterm, maxPrec, false, false, false) ;
}

static void WriteTermMode(StreamPt srm, Pt term, WriteMode mode)
{
	WriteSubtermMode(srm, term, term, mode) ;
}

static void WriteTermsMode(StreamPt srm, Pt list, WriteMode mode)
{
	for( list = Drf(list) ; IsList(list) ; list = Drf(XListTail(list)) ) {
		Pt t = Drf(XListHead(list)) ;
		if( IsList(t) )
			WriteTermsMode(srm, t, mode) ;
		else
			WriteTermMode(srm, t, mode) ;
	}
	if( list != tNilAtom )
		TypeError2("PROPERLY-TERMINATED-LIST", nil) ;
}

static CharPt SubtermAsStrMode(Pt subterm, Pt term, WriteMode mode)
{
	StreamPt s = InnerStreamOpen() ; /* open inner stream afresh */
	WriteSubtermMode(s, subterm, term, mode) ;
	return StreamClose(s, nil) ;
}

static CharPt TermAsStrMode(Pt term, WriteMode mode)
{
	StreamPt s = InnerStreamOpen() ; /* open inner stream afresh */
	WriteTermMode(s, term, mode) ;
	return StreamClose(s, nil) ;
}

static CharPt TermsAsStrMode(Pt list, WriteMode mode)
{
	StreamPt s = InnerStreamOpen() ; /* open inner stream afresh */
	WriteTermsMode(s, list, mode) ;
	return StreamClose(s, nil) ;
}

static void WriteTermNormal(StreamPt srm, Pt term)
{
	WriteTermMode(srm, term, forceQuoted_flag ? mQuoted : mNormal) ;
}

static CharPt TermAsStrNormal(Pt term)
{
	return TermAsStrMode(term, forceQuoted_flag ? mQuoted : mNormal) ;
}


/* PUBLIC */

CharPt SubtermAsStr(Pt subterm, Pt term)
{
	return SubtermAsStrMode(subterm, term, mQuoted) ;
}

CharPt TermAsStr(Pt term)
{
	return TermAsStrMode(term, mQuoted) ;
}

CharPt TermsAsStr(Pt list)
{
	return TermsAsStrMode(list, mNormal) ;
}

void SetWriteDepth(Size termDepth, Size listLength)
{
	termMaxDepth = termDepth == 0 ? LONG_MAX : termDepth ;
	listMaxLength = listLength == 0 ? LONG_MAX : listLength ;
}


/* CXPROLOG C'BUILTINS */

static void PWrite()
{
	WriteTermNormal(currOut, X0) ;
	JumpNext() ;
}

static void PSWrite()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermNormal(srm, X1) ;
	JumpNext() ;
}

static void PWriteln()
{
	WriteTermNormal(currOut, X0) ;
	StreamPut(currOut, '\n') ;
	JumpNext() ;
}

static void PSWriteln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermNormal(srm, X1) ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PWriteQ()
{
	WriteTermMode(currOut, X0, mQuoted) ;
	JumpNext() ;
}

static void PSWriteQ()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mQuoted) ;
	JumpNext() ;
}

static void PWriteQln()
{
	WriteTermMode(currOut, X0, mQuoted) ;
	StreamPut(currOut, '\n') ;
	JumpNext() ;
}

static void PSWriteQln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mQuoted) ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PPrint()
{
	WriteTermMode(currOut, X0, mPrint) ;
	JumpNext() ;
}

static void PSPrint()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mPrint) ;
	JumpNext() ;
}

static void PDisplay()
{
	WriteTermMode(userOut, X0, mDisplay) ;
	JumpNext() ;
}

static void PDisplayln()
{
	WriteTermMode(userOut, X0, mDisplay) ;
	StreamPut(userOut, '\n') ;
	JumpNext() ;
}

static void PAtomTerm()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
		MustBe( t != nil && Unify(X1, t) ) ;
	}
	elif( IsVar(t0) )
		MustBe( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrNormal(X1))) ) ;
	else TypeError2("ATOM or VAR", t0) ;
}

static void PAtomTermQ()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
		MustBe( t != nil && Unify(X1, t) ) ;
	}
	elif( IsVar(t0) )
		MustBe( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrMode(X1, mQuoted))) ) ;
	else TypeError2("ATOM or VAR", t0) ;
}

static void PQuote()
{
	CharPt n = XTestAtomName(X0) ;
	output = InnerStreamOpen() ;
	AddQStr(n, true, false, false) ;
	MustBe( Unify(X1, MakeTempAtom(StreamClose(output, nil))) ) ;
}

static void PWriteDepth(void)
{
	SetWriteDepth(XTestNat(X0), XTestNat(X1)) ;
	JumpNext() ;
}

void TermWriteInit()
{
	SetWriteDepth(30,999) ;

/* create portray/1 descriptor (is a mutable builtin) */
	portrayPred = LookupPredicateByName("portray", 1) ;
	SetDynamic(portrayPred, true) ;

/* install builtins */
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
