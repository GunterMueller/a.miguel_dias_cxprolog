/*
 *   This file is part of the CxProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 1992/02/23
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

typedef enum { mNormal, mNonex } WriteMode ;

static int character_escapes ;
static int ignore_ops ;
static int numbervars ;
static int portray ;
static int quoted ;

static Size termMaxDepth, listMaxLength ;
static Size currTermDepth ;
static PredicatePt portrayPred ;

static StreamPt output ;


/* Write Options */

static void SetWriteOptions(Pt t)
{
	t = TestList(t) ;
	for( ; IsList(t) ; t = Drf(XListTail(t)) ) {
		Pt e = Drf(XListHead(t)) ;
		CharPt opt ;
		Pt arg ;
		if( IsStruct(e)
			&& XStructArity(e) == 1
			&& (opt = XStructName(e))
			&& (arg = Drf(XStructArg(e,0))) )
				switch( opt[0] ) {
					case 'c':
						if( StrEqual(opt, "character_escapes") && XAtomBool(&character_escapes, arg) )
							continue ;
						break ;
					case 'i':
						if( StrEqual(opt, "ignore_ops") && XAtomBool(&ignore_ops, arg) )
							continue ;
						break ;
					case 'n':
						if( StrEqual(opt, "numbervars") && XAtomBool(&numbervars, arg) )
							continue ;
						break ;
					case 'p':
						if( StrEqual(opt, "portray") && XAtomBool(&portray, arg) )
							continue ;
						break ;
					case 'q':
						if( StrEqual(opt, "quoted") && XAtomBool(&quoted, arg) )
							continue ;
						break ;
				}
		FileError("Invalid write_term option '%s'", TermAsStr(e)) ;
	}
}


/* Term Write */

#define AddChar(c)		(StreamPut(output, c))
#define AddStr(s)		(StreamPutStr(output, s))

static void AddEscapeSequence(WChar c)
{
	switch( c ) {
		case '\a':
			AddChar('\\') ; AddChar('a') ; break ;
		case '\b':
			AddChar('\\') ; AddChar('b') ; break ;
		case '\f':
			AddChar('\\') ; AddChar('f') ; break ;
		case '\n':
			AddChar('\\') ; AddChar('n') ; break ;
		case '\r':
			AddChar('\\') ; AddChar('r') ; break ;
		case '\t':
			AddChar('\\') ; AddChar('t') ; break ;
		case '\v':
			AddChar('\\') ; AddChar('v') ; break ;
		case '\'':
			AddChar('\\') ; AddChar('\'') ; break ;
		case '\\':
			AddChar('\\') ; AddChar('\\') ; break ;
		default:
			if( c < ' ' || CharType(c) == __I  )
				AddStr(CharAsNumericEscape(c)) ;
			else AddChar(c) ;
			break ;
	}
}

static void AddQStr(CharPt s, Bool dirty, Bool opClashL, Bool opClashR)
{
	if( dirty ) {
		WChar c ;
		AddChar('\'') ;
		if( character_escapes )
			while((c = CharDecode(s)) != '\0')
				AddEscapeSequence(c) ;
		else
			while((c = CharDecode(s)) != '\0')
				AddChar(c) ;
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
	if( quoted ) {
		CharPt s = name ;
		int first = CharDecode(s) ;
		switch( CharType(first) ) {
			case _SO: dirty = s[0] != '\0' ; break ;
			case '|': dirty = s[0] != '\0' ; break ;
			case '[': dirty = s[0] != ']' || s[1] != '\0' ; break ;
			case '{': dirty = s[0] != '}' || s[1] != '\0' ; break ;
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
				if( first == '.' )
					dirty = s[0] == '\0' ;
				else {
					while( *s != '\0' ) {
						int c = CharDecode(s) ;
						if( !cx_issymbol(c) ) {
							dirty = true ;
							break ;
						}
					}
				}
				break ;
			}
			default: dirty = true ; break ;
		}
	}
	if( name[0] == '[' && name[1] == ']' && name[2] == '\0' )
		AddQStr(name, dirty, false, false) ;
	else
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
			if( ignore_ops ) goto rawL ;
			elif( func == varFunctor ) {
				Pt t = DrfChecked(XStructArg(term, 0)) ;
				if( !numbervars )
					goto rawL ;
				elif( IsAtom(t) )
					AddAtom(XAtom(t), false, false) ;
				elif( IsInt(t) ) {
					PInt i = XInt(t) ;
					if( i >= 0 ) {
						AddChar('A' + i % 26) ;
						if( i >= 26 )
							AddStr(GStrFormat("%d", i / 26)) ;
					}
					else goto rawL ;
				}
				else goto rawL ;
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
			else goto rawL ;
			break ;
		}
		case 2: {
			if( ignore_ops ) goto rawL ;
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
			else goto rawL ;
			break ;
		}
		default: {
			int i ;
rawL:		AddAtom(atom, false, false) ;
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

static void WriteListNoOps(Pt term)
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
	int character_escapesSave ;
	int ignore_opsSave ;
	int numbervarsSave ;
	int portraySave ;
	int quotedSave ;
	Size currTermDepthSave ;
	StreamPt outputSave ;
	Bool res ;

	if( !PredHasClauses(portrayPred) ) return false ;

	character_escapesSave = character_escapes ;
	ignore_opsSave = ignore_ops ;
	numbervarsSave = numbervars ;
	portraySave = portray ;
	quotedSave = quoted ;
	currTermDepthSave = currTermDepth ;
	outputSave = output ;

	res = CallProlog(MakeUnStruct(PredFunctor(portrayPred),term), nil) == peSucc ;

	character_escapes = character_escapesSave ;
	ignore_ops = ignore_opsSave ;
	numbervars = numbervarsSave ;
	portray = portraySave ;
	quoted = quotedSave ;
	currTermDepth = currTermDepthSave ;
	output = outputSave ;

	return res ;
}

static void DoWriteTerm(register Pt term, int p, Bool opClashL, Bool opClashR, Bool parAtom)
{
	int q, rq ;
	term = DrfChecked(term) ;
	if( IsAtom(term) )
		if( portray && Portray(term) ) ;
		else {
			AtomPt a = XAtom(term) ;
			if( parAtom && (q = Prefix(a, &rq)) != 0 )
				{ AddChar('(') ; AddAtom(a, false, false) ; AddChar(')') ; }
			else AddAtom(a, opClashL, opClashR) ;
		}
	elif( IsVar(term) )
		AddStr(VarName(term)) ;
	elif( IsNumber(term) )
		if( portray && Portray(term) ) ;
		else {
			CharPt s = XNumberAsStr(term) ;
			if( opClashL && s[0] == '-' ) { /* ex: >(-1) */
				AddChar('(') ; AddStr(s) ; AddChar(')') ;
			}
			else AddStr(s) ;
		}
	elif( IsExtra(term) )
		if( portray && Portray(term) ) ;
		else AddStr(XExtraAsStr(term)) ;
	elif( IsStruct(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( portray && Portray(term) ) ;
		else { currTermDepth++ ;
			   WriteStruct(term, p, opClashL, opClashR) ;
			   currTermDepth--;
		}
	}
	elif( IsList(term) ) {
		if( currTermDepth > termMaxDepth ) AddStr("...") ;
		elif( portray && Portray(term) ) ;
		elif( ignore_ops ) {
			currTermDepth++ ;
			WriteListNoOps(term) ;
			currTermDepth-- ;
		}
		else {
			currTermDepth++ ;
			WriteList(term) ;
			currTermDepth-- ;
		}
	}
	else
		StreamWrite(output, "<INVALID TERM::%lx>", term) ;
}

static void WriteSubtermMode(StreamPt srm, Pt subterm, Pt term, WriteMode mode)
{
	output = srm ;
	currTermDepth = 0 ;
	PrepareDrfChecked(term) ;

	/* Centralized call point */
	DoWriteTerm(subterm, maxPrec, false, false, false) ;
}

static void WriteTermMode(StreamPt srm, Pt term, WriteMode mode)
{
	if( srm == userErr ) {
		StreamFlush(userOut) ;
		WriteSubtermMode(srm, term, term, mode) ;
		StreamFlush(userErr) ;
	}
	else
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
		TypeError("PROPERLY-TERMINATED-LIST", nil) ;
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

static void WriteTermWithOptions(StreamPt srm, Pt term, Pt options)
{
/* Set defaults */
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = forceQuoted_flag ;
/* Change defaults */
	SetWriteOptions(options) ;
/* Write the term */
	WriteTermMode(srm, term, mNonex) ;
}


/* PUBLIC */

void TermWriteN(StreamPt srm, Pt term)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = forceQuoted_flag ;
	WriteTermMode(srm, term, forceQuoted_flag ? mNonex : mNormal) ;
}

void TermWriteQ(StreamPt srm, Pt term) /* quoted */
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = 1 ;
	WriteTermMode(srm, term, mNonex) ;
}

void TermWriteP(StreamPt srm, Pt term) /* print */
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 0 ;
	portray = 1 ;
	quoted = forceQuoted_flag ;
	WriteTermMode(srm, term, mNonex) ;
}

void TermWriteD(StreamPt srm, Pt term) /* display */
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 1 ;
	numbervars = 0 ;
	portray = 0 ;
	quoted = forceQuoted_flag ;
	WriteTermMode(srm, term, mNonex) ;
}

void TermWriteC(StreamPt srm, Pt term)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 1 ;
	numbervars = 0 ;
	portray = 0 ;
	quoted = 1 ;
	WriteTermMode(srm, term, mNonex) ;
}

CharPt TermAsStrN(Pt term)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = forceQuoted_flag ;
	return TermAsStrMode(term, forceQuoted_flag ? mNonex : mNormal) ;
}

CharPt TermAsStrQ(Pt term)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = 1 ;
	return TermAsStrMode(term, mNonex) ;
}

CharPt SubtermAsStrN(Pt subterm, Pt term)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = forceQuoted_flag ;
	return SubtermAsStrMode(subterm, term, forceQuoted_flag ? mNonex : mNormal) ;
}

CharPt SubtermAsStrQ(Pt subterm, Pt term)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = 1 ;
	return SubtermAsStrMode(subterm, term, mNonex) ;
}

CharPt TermsAsStrN(Pt list)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = forceQuoted_flag ;
	return TermsAsStrMode(list, forceQuoted_flag ? mNonex : mNormal) ;
}

CharPt TermsAsStrQ(Pt list)
{
	character_escapes = characterEscapes_flag ;
	ignore_ops = 0 ;
	numbervars = 1 ;
	portray = 0 ;
	quoted = 1 ;
	return TermsAsStrMode(list, mNonex) ;
}

CharPt TermAsStr(Pt term)
{
	return TermAsStrQ(term) ;

}

void SetWriteDepth(Size termDepth, Size listLength)
{
	termMaxDepth = termDepth == 0 ? LONG_MAX : termDepth ;
	listMaxLength = listLength == 0 ? LONG_MAX : listLength ;
}



/* CXPROLOG C'BUILTINS */

static void PWriteTerm()
{
	WriteTermWithOptions(currOut, X0, X1) ;
	JumpNext() ;
}

static void PSWriteTerm()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	WriteTermWithOptions(srm, X1, X2) ;
	JumpNext() ;
}

static void PWrite()
{
	TermWriteN(currOut, X0) ;
	JumpNext() ;
}

static void PSWrite()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteN(srm, X1) ;
	JumpNext() ;
}

static void PWriteln()
{
	TermWriteN(currOut, X0) ;
	StreamPut(currOut, '\n') ;
	JumpNext() ;
}

static void PSWriteln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteN(srm, X1) ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PWriteQ()
{
	TermWriteQ(currOut, X0) ;
	JumpNext() ;
}

static void PSWriteQ()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteQ(srm, X1) ;
	JumpNext() ;
}

static void PWriteQln()
{
	TermWriteQ(currOut, X0) ;
	StreamPut(currOut, '\n') ;
	JumpNext() ;
}

static void PSWriteQln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteQ(srm, X1) ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PPrint()
{
	TermWriteP(currOut, X0) ;
	JumpNext() ;
}

static void PSPrint()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteP(srm, X1) ;
	JumpNext() ;
}

static void PDisplay()
{
	TermWriteD(userOut, X0) ;
	JumpNext() ;
}

static void PSDisplay()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteD(srm, X1) ;
	JumpNext() ;
}

static void PDisplayln()
{
	TermWriteD(userOut, X0) ;
	StreamPut(userOut, '\n') ;
	JumpNext() ;
}

static void PSDisplayln()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteD(srm, X1) ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PWriteCanonical()
{
	TermWriteC(userOut, X0) ;
	StreamPut(userOut, '\n') ;
	JumpNext() ;
}

static void PSWriteCanonical()
{
	StreamPt srm = XTestStream(X0, mWrite) ;
	TermWriteC(srm, X1) ;
	StreamPut(srm, '\n') ;
	JumpNext() ;
}

static void PAtomTermN()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
		MustBe( t != nil && Unify(X1, t) ) ;
	}
	elif( IsVar(t0) )
		MustBe( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrN(X1))) ) ;
	else TypeError("ATOM or VAR", t0) ;
}

static void PAtomTermQ()
{
	Pt t0 = Drf(X0) ;
	if( IsAtom(t0) ) {
		Pt t = ZTermFromStr(XAtomName(t0)) ; /* stacks may grow */
		MustBe( t != nil && Unify(X1, t) ) ;
	}
	elif( IsVar(t0) )
		MustBe( UnifyWithAtomic(t0, MakeTempAtom(TermAsStrQ(X1))) ) ;
	else TypeError("ATOM or VAR", t0) ;
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

/* create portray/1 descriptor (is a mutable built-in) */
	portrayPred = LookupPredicateByName("portray", 1) ;
	SetDynamic(portrayPred, true) ;

/* install builti-ns */
	InstallCBuiltinPred("write_term", 2, PWriteTerm) ;
	InstallCBuiltinPred("write_term", 3, PSWriteTerm) ;
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
	InstallCBuiltinPred("display", 2, PSDisplay) ;
	InstallCBuiltinPred("displayln", 1, PDisplayln) ;
	InstallCBuiltinPred("displayln", 2, PSDisplayln) ;
	InstallCBuiltinPred("write_canonical", 1, PWriteCanonical) ;
	InstallCBuiltinPred("write_canonical", 2, PSWriteCanonical) ;

	InstallCBuiltinPred("atom_term", 2, PAtomTermN) ;
	InstallCBuiltinPred("atom_termq", 2, PAtomTermQ) ;
	InstallCBuiltinPred("quote", 2, PQuote) ;

	InstallCBuiltinPred("write_depth", 2, PWriteDepth) ;
}
