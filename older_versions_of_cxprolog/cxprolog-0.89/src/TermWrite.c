/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL

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

static StreamPt scratch, output ;

#define AddChar(c)		(StreamPut(output, c))
#define AddStr(s)		(StreamPutStr(output, s))

static void AddQStr(CharPt s, Bool dirty, Bool opClashL, Bool opClashR)
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
	else {
		if( opClashL && !cx_isalnum(s[0]) ) AddChar(' ') ;
		AddStr(s) ;
		if( opClashR && !cx_isalnum(s[strlen(s)-1]) ) AddChar(' ') ;
	}
}

static void AddAtom(AtomPt atom, Bool opClashL, Bool opClashR)
{
	CharPt name = AtomName(atom) ;
	if( wMode != mDisplay && wMode != mQuoted )
		AddQStr(name, false, opClashL, opClashR) ;
	else
	  switch( CharType(name[0]) ) {
		case _SO: {
			AddQStr(name, name[1] != '\0', opClashL, opClashR) ;
			break ;
		}
		case '[': {
			AddQStr(name, name[1] != ']' || name[2] != '\0', opClashL, opClashR) ;
			break ;
		}
		case '{': {
			AddQStr(name, name[1] != '}' || name[2] != '\0', opClashL, opClashR) ;
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
			AddQStr(name, dirty, opClashL, opClashR) ;
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
			AddQStr(name, dirty || name[0] == '\0', opClashL, opClashR) ;
			break ;
		}
	}
}

static void DoWriteTerm(register Pt term, int p, Bool opClashL, Bool opClashR) ;

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
				elif( IsInt(t) && (p = XInt(t)) >= 0 ) {
					Str32 s ;
					s[0] = 'A' + p % 26 ;
					s[1] = '\0' ;
					if( p > 25 ) sprintf(s + 1, "%d", p / 26) ;
					AddStr(s) ;
				}
				else goto displayL ;
			}
			elif( func == bracketsFunctor ) {
				AddChar('{') ;
				DoWriteTerm(XStructArg(term, 0), maxPrec, false, false) ;
				AddChar('}') ;
			}
			elif( func == stringFunctor ) {
				AddChar('"') ;
				DoWriteTerm(XStructArg(term, 0), maxPrec, false, false) ;
				AddChar('"') ;
			}
			elif( func == unitParamFunctor ) {
				AddStr(XAtomName(UnitParam(CurrUnit(), XUnitParam(term)))) ; /*?*/
			}
			elif( func == metaCutFunctor ) {
				AddChar('!') ;
			}
            elif( (p = Prefix(atom, &rp)) != 0 ) {
				if( p > m ) { AddChar('(') ; opClashL = opClashR = false ; }
				if( cx_isalpha(AtomName(atom)[0]) ) {
					AddAtom(atom, opClashL, false) ;
					AddChar(' ') ;   /* ex: a->not not b */
					DoWriteTerm(XStructArg(term, 0), rp, false, opClashR) ;
				}
				else {
                    AddAtom(atom, opClashL, false)  ;/* ex: a-> -b */
 					DoWriteTerm(XStructArg(term, 0), rp, true, opClashR) ;
				}
				if( p > m ) AddChar(')') ;
			}
			elif( (p = Postfix(atom, &lp)) != 0 ) {
				if( p > m ) { AddChar('(') ; opClashL = opClashR = false ; }
				if( cx_isalpha(AtomName(atom)[0]) ) {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, false) ;
					AddChar(' ') ;
					AddAtom(atom, false, opClashR) ;
				}
				else {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, true) ;
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
				if( cx_isalpha(AtomName(atom)[0]) ) {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, false) ;
					AddChar(' ') ;
					AddAtom(atom, false, false) ;
					AddChar(' ') ;
					DoWriteTerm(XStructArg(term, 1), rp, false, opClashR) ;
				}
				else {
					DoWriteTerm(XStructArg(term, 0), lp, opClashL, true) ;
					AddAtom(atom, false, false) ;
					DoWriteTerm(XStructArg(term, 1), rp, true, opClashR) ;
				}
				if( p > m ) AddChar(')') ;
			}
			else goto displayL ;
			break ;
		}
		default: {
displayL:	AddAtom(atom, false, false) ;
			AddChar('(') ;
			dotimes(p, XStructArity(term)) {
				if( p != 0 ) AddChar(',') ;
				DoWriteTerm(XStructArg(term, p), subPrec, false, false) ;
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
		DoWriteTerm(XListHead(term), subPrec, false, false) ;
		term = DrfChecked(XListTail(term)) ;
		if( !IsList(term) ) break ;
		AddChar(',') ;
	}
	if( i == listMaxLength )
		AddStr("...") ;
	elif( term != tNilAtom ) {
		AddChar('|') ;
		DoWriteTerm(term, subPrec, false, false) ;
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

static void DoWriteTerm(register Pt term, int p, Bool opClashL, Bool opClashR)
{
	term = DrfChecked(term) ;
	if( IsAtom(term) )
		if( wMode == mPrint && Portray(term) ) ;
		else AddAtom(XAtom(term), opClashL, opClashR) ;
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
	PrepareDrfChecked(term)	 ;

	/* Centralized call point */
	DoWriteTerm(term, maxPrec, false, false) ;
}

static CharPt TermAsStrMode(Pt term, WriteMode mode)
{
	StreamPt s = ScratchStreamOpen() ; /* open scratch afresh */
	WriteTermMode(s, term, mode) ;
	return StreamClose(s) ;
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

CharPt TermAsStr(Pt term)
{
	return TermAsStrMode(term, mQuoted) ;
}

void TermToScratch(Pt term)  /* Append term to scratch, used by concat/1/ */
{
	WriteTermNormal(scratch, term) ;
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
	JumpNext()
}

static void PSWrite()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermNormal(srm, X1) ;
	JumpNext()
}

static void PWriteln()
{
	WriteTermNormal(currOut, X0) ;
	StreamPut(currOut, '\n') ;
	JumpNext()
}

static void PSWriteln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermNormal(srm, X1) ;
	StreamPut(srm, '\n') ;
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
	StreamPut(currOut, '\n') ;
	JumpNext()
}

static void PSWriteQln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermMode(srm, X1, mQuoted) ;
	StreamPut(srm, '\n') ;
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
	StreamPut(userOut, '\n') ;
	JumpNext()
}

static void PAtomTerm()
{
	Pt t0 = DrfChecked(X0) ;
	if( IsAtom(t0) ) {
        Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
		if( t == eofPt ) t = tEmptyAtom ;
		MustBe( t != nil && Unify(X1, t) )
	}
	elif( IsVar(t0) )
		MustBe( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrNormal(X1))) )
	else TypeError2("ATOM or VAR", t0) ;
}

static void PAtomTermQ()
{
	Pt t0 = DrfChecked(X0) ;
	if( IsAtom(t0) ) {
        Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
		if( t == eofPt ) t = tEmptyAtom ;
		MustBe( t != nil && Unify(X1, t) )
	}
	elif( IsVar(t0) )
		MustBe( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrMode(X1, mQuoted))) )
	else TypeError2("ATOM or VAR", t0) ;
}

static void PQuote()
{
	CharPt n = XTestAtomName(X0) ;
	output = ScratchStreamOpen() ;
	AddQStr(n, true, false, false) ;
	MustBe( Unify(X1, MakeTempAtom(StreamClose(output))) )
}

static void PWriteDepth(void)
{
	SetWriteDepth(XTestNat(X0), XTestNat(X1)) ;
	JumpNext()
}

void TermWriteInit()
{
	SetWriteDepth(30,999) ;

/* prepare for function TermToScratch */
	scratch = ScratchStreamOpen() ;
	StreamClose(scratch) ;

/* create portray/1 descriptor */
	portrayPred =  LookupPredicateByName("portray", 1) ;
	PredIsMutableBuiltin(portrayPred) = true ;

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
