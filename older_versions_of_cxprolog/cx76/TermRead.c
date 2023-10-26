/*
 *   This file is part of the CxProlog system

 *   TermRead.c
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
#include <math.h>


/* SCANNER */

typedef enum
{
	vnsTk,		atomTk,		textTk,
	openRTk,	openSTk,	openCTk,
	closeRTk,	closeSTk,	closeCTk,
	commaTk,	barTk,
	dotTk
} TokenType ;

static int cc ;
static TokenType tk ;
static Pt tkTerm ;
static int nTokens, tkPreBlanks ;
static jmp_buf readJmp ;
static Bool delayToken, noTexts ;
static CharPt inPt, tokenTextPt, peekTokenTextPt ;
static Hdl readStack ;
static struct { AtomPt name ; Pt var ; } vars[maxVarsPerTerm] ;
static int nVars ;

static char auxBuffer[2000] ;	/* @@ to remove */

static CharPt strGet ;
static StreamPt input ;
#define GetChar0()		(strGet ? *strGet++ : GetStream(input))
#define PeekChar0()		(strGet ? *strGet : PeekStream(input))

static Pt LookupVar(CharPt id)
{
	register AtomPt name ;
	register int i ;

	if( id[0] == '_' && id[1] == '\0' )
		return MakeVar() ;
	name = LookupAtom(id) ;
	dotimes(i, nVars)
		if( vars[i].name == name )
			return vars[i].var ;
	if( nVars == maxVarsPerTerm )
		Error("Too many variables in term") ;
	vars[nVars].name = name ;
	vars[nVars].var = MakeVar() ;
	return vars[nVars++].var ;
}

static int PeekStream(StreamPt srm)
{
	int c = GetStream(srm) ;
	UngetStream(srm, c) ;
	return c ;
}

static Bool IsFullStop(int cc)
{
	if( cc == '.' ) {
		int c = PeekChar0() ;
		return cx_isspace(c) || c == '%' || cx_iseof(c) ;
	}
	else return false ;
}

static void ErrorPointer(int n)
{
	int m ;
	for( m = 0 ; m < n ; m++ )
		PutStream(userErr, '-') ;
	WriteStream(userErr, n < 75 ? "^---\n" : "^ \n") ;
}

static void ReaderError(CharPt s)
{
	int n ;
	register CharPt ch ;
	Bool pointerDone = false ;

	WriteStream(userErr, "*** READER ERROR (%s) ***\n", s) ;
	for( ch = auxBuffer ; ch < tokenTextPt && *ch == '\n' ; ch++ ) ;
	for( n = 0 ; ch < tokenTextPt ; ch++ ) {
		n++ ;
		if( *ch == '\n' ) n = 0 ;
		PutStream(userErr, *ch) ;
	}
	for( ; ch < inPt - 1 ; ch++ ) {
		PutStream(userErr, *ch) ;
		if( *ch == '\n' && not pointerDone ) {
			ErrorPointer(n) ;
			pointerDone = true ;
		}
	}
	while( not cx_iseof(cc) ) {
		PutStream(userErr, cc) ;
		if( cc == '\n' && not pointerDone ) {
			ErrorPointer(n) ;
			pointerDone = true ;
		}
		if( IsFullStop(cc) ) break ;
		cc = GetChar0() ;
	}

	if( not pointerDone ) {
		PutStream(userErr, '\n') ;
		ErrorPointer(n) ;
	}
	longjmp(readJmp, 1) ;
}

static void NextChar()
{
	*inPt++ = cc = GetChar0() ;
}

static void NextCharEOF()
{
	if( cx_iseof(cc) )
		ReaderError("Premature end of file") ;
	*inPt++ = cc = GetChar0() ;
	if( inPt - auxBuffer > 1100 ) {
		inPt -= 50 ;
	}
}

static CharPt ReadId()
{
	CharPt s = cCharPt(readStack) ;
	register long n = 0 ;

	do {
		HandleBufferOverflowBytes(s, "ReadId")
		*s++ = cc ;
		n++ ;
		NextChar() ;
	} while( cx_isalnum(cc) ) ;
	*s = '\0' ;
	readStack = cHdl(s - n) ;	/* Buffer might have moved */
	return s - n ;
}

static CharPt ReadSymbol()
{
	CharPt s = cCharPt(readStack) ;
	register long n = 0 ;

	do {
		HandleBufferOverflowBytes(s, "ReadSymbol")
		*s++ = cc ;
		n++ ;
		NextChar() ;
	} while( cx_issymbol(cc) ) ;
	*s = '\0' ;
	readStack = cHdl(s - n) ;	/* Buffer might have moved */
	return s - n ;
}

static CharPt ReadQuoted()
{
	CharPt s = cCharPt(readStack) ;
	register long n = 0 ;
	int quote = cc ;

	NextChar() ;	/* Skip first quote */
	for(;;) {
		for( ; cc != quote ; NextCharEOF() ) {
			HandleBufferOverflowBytes(s, "ReadQuoted")
			*s++ = cc ;
			n++ ;
		}
		NextChar() ;
		if( cc == quote ) {	/* A quote again */
			*s++ = quote ;
			n++ ;
			NextChar() ;
		}
		else break ;
	}
	*s = '\0' ;
	readStack = cHdl(s - n) ;	/* Buffer might have moved */
	return s - n ;
}

static Int ReadInt10()
{
	register Int i = 0 ;

	do {
		i = i * 10 + (cc - '0') ;
		NextChar() ;
	} while( cx_isdigit(cc) ) ;
	return i ;
}

static Int ReadInt(Int base)
{
	register Int i = 0 ;
	register int n ;

	for(;;) {
		if( cx_isdigit(cc) ) n = cc - '0' ;
		elif( 'a' <= cc && cc <= 'z' ) n = 10 + cc - 'a' ;
		else break ;
		if( n >= base ) break ;
		i = i * base + n ;
		NextChar() ;
	}
	return i ;
}

static Real ReadReal(Int ipart)
{
	Int dpart = 0, epart = 0 ;
	int esig = 1 ;
	double d ;
	char s[512] ;

	if( cc == '.' ) {
		NextChar() ;	/* Skip '.' */
		if( cx_isdigit(cc) )
			dpart = ReadInt10() ;
		else ReaderError("Malformed decimal part of real") ;
	}
	if( cc == 'e' || cc == 'E' ) {
		NextChar() ;
		if( cc == '+' )
			NextChar() ;
		else if( cc == '-' ) {
			esig = -1 ;
			NextChar() ;
		}
		if( cx_isdigit(cc) )
			epart = ReadInt10() ;
		else ReaderError("Malformed exponent of real") ;
	}
	sprintf(s, "%ld.%lde%ld", ipart, dpart, esig*epart) ;
	if( sscanf(s, "%lf", &d) != 1 )
		ReaderError("Malformed real") ;
	return d ;
}

static void SkipComment(void)
{
	NextChar() ;		/* skip / */
	while( cc != '/' ) {	/* start with a '*' */
		do { NextCharEOF() ; inPt-- ; } while( cc != '*' ) ;
		do { NextChar() ; inPt-- ; } while( cc == '*' ) ;
	}
	*inPt++ = '*' ;		/* Remove inside of commentary for error reports */
	*inPt++ = '/' ;
	NextChar() ;	/* skip / */
}

static void SkipLineComment(void)
{
	do {
		NextChar() ;
		inPt-- ;
	} while( cc != '\n' && not cx_iseof(cc) ) ;
}

static void NextToken()
{
	CharPt s ;

	if( delayToken ) {
		tokenTextPt = peekTokenTextPt ;
		delayToken = false ;
		return ;
	}
redoL:
	for( tkPreBlanks = 0 ; cx_isspace(cc) ; NextChar(), tkPreBlanks++ ) ;
	if( inPt - auxBuffer > 1000 )
		inPt = tokenTextPt ;
	tokenTextPt = inPt - 1 ;
	switch( allChars[cc] ) {
		case _LC: {
			s = ReadId() ;
atomOrTextL:
			if( noTexts ) {
				tkTerm = MakeAtom(s) ;
				tk = atomTk ;
			}
			else {
				tkTerm = MakeAtomOrText(s) ;
				tk = IsText(tkTerm) ? textTk : atomTk ;
			}
			break ;
		}
		case _UC: {
			s = ReadId() ;
			tk = vnsTk ;
			tkTerm = LookupVar(s) ;
			break ;
		}
		case _SO: {
			s = cCharPt(readStack) ;
			s[0] = cc ;
			s[1] = '\0' ;
			NextChar() ;
			goto atomOrTextL ;
		}
		case _SY: {	/* '.' and '/' are symbols */
			if( IsFullStop(cc) )
				{ tk = dotTk ; NextChar() ; break ; }
			elif( cc == '/' && PeekChar0() == '*' )
				{ SkipComment() ; goto redoL ; }
			s = ReadSymbol() ;
			goto atomOrTextL ;
		}
		case _DG: {
			Int base = ReadInt10() ;
			tk = vnsTk ;	
			if( cc == '\'' || cc == '\"' ) {
				NextChar() ; /* Skip quote */
				switch( base ) {
					case 0: {
						tkTerm = MakeInt(cc) ;
						NextCharEOF() ;
						if( cx_isalnum(cc) )
							ReaderError("Too many chars in 0'x literal") ;
						break ;
					}
					case 1: {
						if( not cx_isalpha(cc) || (tkTerm = MakeExtraFromStr(ReadId())) == nil )
							ReaderError("Invalid 1'xxx_yyy literal") ;
						break ;
					}
					default: {
						if( base > 36 )
							ReaderError("Too big a base in base'xxx") ;
						tkTerm = MakeInt(ReadInt(base)) ;
						break ;
					}
				}
			}
			elif( (cc == '.' && not IsFullStop(cc)) || cc == 'e' || cc == 'E' )
				tkTerm = MakeReal(ReadReal(base)) ;
			else
				tkTerm = MakeInt(base) ;
			break ;
		}
		case '(': { tk = openRTk ; NextChar() ; break ; }
		case ')': { tk = closeRTk ; NextChar() ; break ; }
		case '[': { tk = openSTk ; NextChar() ; break ; }
		case ']': { tk = closeSTk ; NextChar() ; break ; }
		case '{': { tk = openCTk ; NextChar() ; break ; }
		case '}': { tk = closeCTk ; NextChar() ; break ; }
		case '|': { tk = barTk ; NextChar() ; break ; }
		case ',': { tk = commaTk ; NextChar() ; break ; }
		case '\'':{ s = ReadQuoted() ; goto atomOrTextL ; }
		case '\"':{ s = ReadQuoted() ; tk = vnsTk ; tkTerm = StringToPString(s) ; break ; }
		case '%': { SkipLineComment() ; goto redoL ; }
		case _EF: {
			if( nTokens == 0 ) longjmp(readJmp, 2) ;
			elif( strGet ) { tk = dotTk ; break ; }	/* fake that dotTk was seen */
			else NextCharEOF() ;	/* Generate an eof error */
			break ;
		}
		default: {
			WriteStream(userErr, "Ascii code: <%d>", cc) ;
			ReaderError("Invalid character") ;
			break ;
		}
	}
	nTokens++ ;
}

static void PeekToken()
{
	if( not delayToken ) {
		NextToken() ;
		peekTokenTextPt = tokenTextPt ;
		delayToken = true ;
	}
}

static void InitLex()
{
	nVars = 0 ;
	cc = ' ' ;
	nTokens = 0 ;
	tk = barTk ;
	tokenTextPt = inPt = auxBuffer ;
	delayToken = false ;
}


/* TEST */

static void PrintToken()
{
	switch( tk ) {
		case atomTk:	WriteStd("atom(%s) ", XAtomOrTextName(tkTerm)) ; break ;
		case textTk:	WriteStd("text(%s) ", XAtomOrTextName(tkTerm)) ; break ;
		case vnsTk:		WriteStd("vns(%d) ", TermAsStr(tkTerm)) ; break ;
		case dotTk:		WriteStd(".\n") ; break ;
		case openRTk:	WriteStd("( ") ; break ;
		case closeRTk:	WriteStd(") ") ; break ;
		case openSTk:	WriteStd("[ ") ; break ;
		case closeSTk:	WriteStd("] ") ; break ;
		case openCTk:	WriteStd("{ ") ; break ;
		case closeCTk:	WriteStd("} ") ; break ;
		case barTk:		WriteStd("| ") ; break ;
		default:		WriteStd("OTHER ") ; break ;
	}
}


/* PARSER

 Recursive descendent with the most frequent cases involving
 operators handled directly.

 The grammar:

	term		--> subterm(1200), dotTk.
	subterm(N)	--> term(MN), { M <= N }.
	term(N)		-->					op(N,fx).
				-->					op(N,fy).
				-->					op(N,fx),	subterm(N-1).
				-->					op(N,fy),	subterm(N).
				--> subterm(N-1),	op(N,xfx),	subterm(N-1).
				--> subterm(N-1),	op(N,xfy),	subterm(N).
				--> subterm(N),		op(N,yfx),	subterm(N-1).
				--> subterm(N-1),	op(N,xf).
				--> subterm(N),		op(N,yf).
	term(1000)	--> subterm(999), commaTk, subterm(1000).
	term(0)		--> atomTk, openRTk, args, closeRTk.
				--> openRTk, subterm(1200), closeRTk.
				--> openCTk, subterm(1200), closeCTk.
				--> list | atomTk | stringTk | numberTk | varTk.
*/

static Pt Term(int n) ;

static Pt Args(AtomPt atom)
{
	int arity = 0 ;
	NextToken() ;	/* skip ( */
	do {
		HandleBufferOverflowWords(readStack, "read term")
		Push(readStack, Term(subPrec)) ;
		if( ++arity > maxArity )
			Error("Highest arity (%d) exceeded on functor '%s/%d'",
								maxArity, AtomName(atom), arity) ;
		NextToken() ;
	} while( tk == commaTk ) ;
	if( tk != closeRTk ) ReaderError("')' expected") ;
	return MakeStruct(LookupFunctor(atom, arity), readStack -= arity) ;
}

static Pt RoundBrackets()
{
	Pt t = Term(maxPrec) ;
	NextToken() ;
	if( tk != closeRTk ) ReaderError("')' expected") ;
	return t ;
}

static Pt SquareBrackets()
{
	int n = 0 ;
	PeekToken() ;	/* peek after [ */
	if( tk == closeSTk ) {
		NextToken() ;	/* skip ] */
		return tNilAtom ;
	}
	do {
		HandleBufferOverflowWords(readStack, "read term")
		Push(readStack, Term(subPrec)) ;
		n++ ;
		NextToken() ;
	} while( tk == commaTk ) ;
	if( tk == barTk ) {
		Push(readStack, Term(subPrec)) ;
		NextToken() ;
	}
	else Push(readStack, tNilAtom) ;
	n++ ;
	if( tk != closeSTk ) ReaderError("']' expected") ;
	return ArrayToOpenList(readStack -= n, n) ;
}

static Pt CurlyBrackets()
{
	Pt t ;

	PeekToken() ;	/* peek after { */
	if( tk == closeCTk ) {
		NextToken() ;	/* skip } */
		return tBracketsAtom ;
	}
	t = Term(maxPrec) ;
	NextToken() ;
	if( tk != closeCTk )
		ReaderError("'}' expected") ;
	return MakeUnStruct(LookupFunctor(XAtom(tBracketsAtom), 1), t) ;
}

static Pt MinusOp(AtomPt atom)
{
	if( atom == XAtom(tMinusAtom) && tk == vnsTk && IsNumber(tkTerm) ) {
		Pt t = tkTerm ;
		NextToken() ;
		return IsInt(t) ? MakeInt(-XInt(t)) : MakeReal(-XReal(t)) ;
	}
	else return nil ;
}

static Pt Term(int n)
{
	register Pt res ;
	AtomPt atom ;
	int m = 0, p, lp, rp ;

	NextToken() ;
	switch( tk ) {
		case vnsTk:		res = tkTerm ; break ;
		case openRTk:	res = RoundBrackets() ; break ;
		case openSTk:	res = SquareBrackets() ; break ;
		case openCTk:	res = CurlyBrackets() ; break ;
		case closeRTk:	goto errorL ;
		case closeSTk:	goto errorL ;
		case closeCTk:	goto errorL ;
		case commaTk:	goto errorL ;
		case barTk:		goto errorL ;
		case dotTk:		goto errorL ;
		case textTk: {
			res = tkTerm ;
			PeekToken() ;
			if( tk == openRTk && tkPreBlanks == 0 )
				res = Args(XAtomOrTextAsAtom(res)) ;
			break ;
		}
		case atomTk: {
			res = tkTerm ;
			atom = XAtom(tkTerm) ;
			PeekToken() ;
			if( tk == openRTk && tkPreBlanks == 0 )
				res = Args(atom) ;
			elif( (p = Prefix(atom, &rp)) != 0 ) {
				if( (res = MinusOp(atom)) != nil ) break ;
				if( n < p ) goto errorL ;
				switch( tk ) {
					case textTk: case vnsTk: case openRTk:
					case openSTk: case openCTk:					goto prefixL ;
					case commaTk: case closeRTk: case closeSTk:
					case closeCTk: case dotTk: case barTk:		goto atomL ;
					case atomTk: {
						if( Infix(XAtom(tkTerm), &lp, &rp) && m <= lp ) goto atomL ;
						elif( Postfix(XAtom(tkTerm), &lp) && m <= lp ) goto atomL ;
						else goto prefixL ;
					}
				}
				InternalError("Term (1)") ;
			}
			break ;
		}
		default:
			InternalError("Term (2)") ;
	}

rightL:
	PeekToken() ;
	switch( tk ) {
		case dotTk: case closeRTk: case closeSTk: case closeCTk:
			goto doneL ;
		case textTk: case vnsTk: case openRTk: case openSTk: case openCTk:
			goto errorL ;
		case commaTk: {
			if( n >= commaPrec && m < commaPrec ) {
				m = commaPrec ;
				NextToken() ;
				res = MakeBinStruct(commaFunctor, res, Term(m)) ;
				if( n > m ) goto rightL ;
			}
			goto doneL ;
		}
		case barTk: {
			if( n >= barPrec && m < barPrec ) {
				m = barPrec ;
				NextToken() ;
				if( barOpDefined )
					res = MakeBinStruct(barFunctor, res, Term(m)) ;
				else
					res = MakeBinStruct(semicolonFunctor, res, Term(m)) ;
				if( n > m ) goto rightL ;
			}
			goto doneL ;
		}
		case atomTk: {
			atom = XAtom(tkTerm) ;
			if( (p = Postfix(atom, &lp)) != 0 && n >= p && m <= lp ) {
				NextToken() ;
				if( (p = Infix(atom, &lp, &rp)) != 0 && n >= p && m <= lp ) {
					PeekToken() ;
					switch( tk ) {
						case openRTk: case openSTk: case openCTk:
						case vnsTk: case textTk:
							goto infixL ;
						case commaTk: case barTk: case closeRTk:
						case closeSTk:
							goto posfixL ;
						case atomTk: {
							if( ExclusivelyPrefix(XAtom(tkTerm)) ) goto infixL ;
							else goto posfixL ;
						}
					}
					InternalError("Term (3)") ;
				}
				else goto posfixL ;
			}
			elif( (p = Infix(atom, &lp, &rp)) != 0 && n >= p && m <= lp ) {
				NextToken() ;
				goto infixL ;
			}
			else goto doneL ;	/* not an operator */
		}
	}
	InternalError("Term (4)") ;

errorL:
	ReaderError("Malformed term") ;
doneL:
	return res ;
atomL:
	if( n < m ) goto errorL ;
	res = TagAtom(atom) ;
	goto rightL ;
prefixL:
	res = MakeUnStruct(LookupFunctor(atom, 1), Term(rp)) ;
	m = p ;
	goto rightL ;
infixL:
	res = MakeBinStruct(LookupFunctor(atom, 2), res, Term(rp)) ;
	m = p ;
	goto rightL ;
posfixL:
	res = MakeUnStruct(LookupFunctor(atom, 1), res) ;
	m = p ;
	goto rightL ;
}

static Pt DoReadTerm()
{
	Hdl saveStructState ;

	InitLex() ;
	readStack = cHdl(UseBuffer()) ;
	saveStructState = H ;
	switch( setjmp(readJmp) ) {
		case 0: {
			Pt res = Term(maxPrec) ;
			NextToken() ;
			if( tk != dotTk ) ReaderError("'.' expected") ;
			FreeBuffer() ;
			return res ;
		}
		case 1: {
			H = saveStructState ;
			FreeBuffer() ;
			return nil ;
		}
		case 2: {
			H = saveStructState ;
			FreeBuffer() ;
			return tEofAtom ;
		}
	}
	InternalError("DoReadTerm") ;
	return nil ;
}

static Pt ReadTermStream(StreamPt srm)
{
	input = srm ;
	noTexts = false ;
	return DoReadTerm() ;
}

static Pt ReadTermStreamForAssert(StreamPt srm)
{
	input = srm ;
	noTexts = true ;
	return DoReadTerm() ;
}

/* PUBLIC */

Pt ReadTerm()
{
	input = currIn ;
	noTexts = false ;
	return DoReadTerm() ;
}

void SeeStr(CharPt s)
{
	strGet = s ;
}

void SeenStr()
{
	strGet = nil ;
}

Pt ReadTermFromStr(CharPt s)
{
	Pt t ;
	SeeStr(s) ;
	input = nil ;
	noTexts = false ;
	t = DoReadTerm() ;
	SeenStr() ;
	return t ;
}


/* CXPROLOG C'BUILTINS */

static void PStreamCheck()
{
	Pt t = Drf(X0) ;
	if( IsAtomOrText(t) ) {
		if( t == tUserAtom ) JumpNext()
		t = IVarGet(XAtomOrTextAsAtom(t)) ;
	}
	if( IsExtra(t) && XExtraSubTag(t) == streamSubTag && StreamCheck(XPt(t)) ) JumpNext()
	DoFail()
}

static void PSee()
{
	See(XTestAtomName(X0)) ;
	JumpNext()
}

static void PSeeing()
{
	if( UnifyWithAtomic(X0, MakeAtomOrText(NameOfStream(currIn))) ) JumpNext()
	DoFail()
}

static void PSeen()
{
	CloseStream(currIn) ;
	JumpNext()
}

static void PGet0()
{
	int c = GetStream(currIn) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSGet0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = GetStream(srm) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X1, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PGet()
{
	int c = GetStreamNonBlank(currIn) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSGet()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = GetStreamNonBlank(srm) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X1, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PPeek0()
{
	int c = GetStream(currIn) ;
	UngetStream(currIn, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSPeek0()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = GetStream(srm) ;
	UngetStream(srm, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X1, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PPeek()
{
	int c = GetStreamNonBlank(currIn) ;
	UngetStream(currIn, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSPeek()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = GetStreamNonBlank(srm) ;
	UngetStream(srm, c) ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSkip()
{
	int c = XTestInt(X0) ;
	if( c == 10 ) c = '\n' ;
	while( GetStream(currIn) != c ) ;
	JumpNext()
}

static void PSSkip()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	int c = XTestInt(X1) ;
	if( c == 10 ) c = '\n' ;
	while( GetStream(srm) != c ) ;
	JumpNext()
}

static void PGetLine()
{
	CharPt str = GetLineStream(currIn) ;
	if( str == nil ) {
		if( UnifyWithAtomic(X0, MakeInt(eofCode)) ) JumpNext()
		DoFail()
	}
	else {
		if( UnifyWithAtomic(X0, MakeAtomOrText(str)) ) JumpNext()
		DoFail()
	}
}

static void PSGetLine()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	CharPt str = GetLineStream(srm) ;
	if( str == nil ) {
		if( UnifyWithAtomic(X1, MakeInt(eofCode)) ) JumpNext()
		DoFail()
	}
	else {
		if( UnifyWithAtomic(X1, MakeAtomOrText(str)) ) JumpNext()
		DoFail()
	}
}

static void PRead()
{
	Pt t ;
	if( ( t = ReadTermStream(currIn) ) != nil && Unify(X0, t) ) JumpNext()
	DoFail()
}

static void PSRead()
{
	Pt t ;
	StreamPt srm = XTestStream(X0, mRead) ;
	if( ( t = ReadTermStream(srm) ) != nil && Unify(X1, t) ) JumpNext()
	DoFail()
}

static void PVarNames()
{
	register int i ;
	register Pt t = tNilAtom ;
	dotimes(i, nVars)
		t = MakeBinStruct(
				listFunctor,
				MakeBinStruct(eqFunctor, TagAtom(vars[i].name), vars[i].var),
				t) ;
	if( Unify(X0, t) ) JumpNext()
	DoFail()
}

static void PReadForAssert()
{
	Pt t ;
	if( (t = ReadTermStreamForAssert(currIn)) != nil && Unify(X0, t) ) JumpNext()
	DoFail()
}

static void POpen()
{
	CharPt name = XTestAtomName(X0) ;
	CharPt mode = XTestAtomName(X1) ;
	Pt t = Drf(X2) ;
	StreamMode m ;

	if( EqualStr(mode, "read") )
		m = mRead ;
	elif( EqualStr(mode, "write") )
		m = mWrite ;
	elif( EqualStr(mode, "append") )
		m = mAppend ;
	else Error("Invalid stream mode") ;

	if( IsVar(t) ) {
		StreamPt srm = OpenFileStream(name, m) ;
		if( UnifyWithAtomic(t, TagExtra(srm)) ) JumpNext()
		InternalError("POpen") ;
	}
	if( IsAtomOrText(t) ) {
		StreamPt srm = OpenFileStream(name, m) ;
		IVarSet(XAtomOrTextAsAtom(t), TagExtra(srm)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PClose()
{
	StreamPt srm = XTestStream(X0, mNone) ;
	CloseStream(srm) ;
	JumpNext()
}

static void PSetInput()
{
	currIn = XTestStream(X0, mRead) ;
	JumpNext()
}

static void PCurrentInput()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		if( UnifyWithAtomic(t, TagExtra(currIn)) ) JumpNext()
		InternalError("PCurrentInput") ;
	}
	if( IsAtomOrText(t) ) {
		IVarSet(XAtomOrTextAsAtom(t), TagExtra(currIn)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PSetOutput()
{
	currOut = XTestStream(X0, mWrite) ;
	JumpNext()
}

static void PCurrentOutput()
{
	Pt t = Drf(X0) ;
	if( IsVar(t) ) {
		if( UnifyWithAtomic(t, TagExtra(currOut)) ) JumpNext()
		InternalError("PCurrentInput") ;
	}
	if( IsAtomOrText(t) ) {
		IVarSet(XAtomOrTextAsAtom(t), TagExtra(currOut)) ;
		JumpNext()
	}
	TypeError("var or ivar", t) ;
}

static void PStreams()
{
	ShowStreams() ;
	JumpNext()
}

void InitTermRead()
{
	InstallCBuiltinPred("stream", 1, PStreamCheck) ;

	InstallCBuiltinPred("see", 1, PSee) ;
	InstallCBuiltinPred("seeing", 1, PSeeing) ;
	InstallCBuiltinPred("seen", 0, PSeen) ;

	InstallCBuiltinPred("get0", 1, PGet0) ;
	InstallCBuiltinPred("get0", 2, PSGet0) ;
	InstallCBuiltinPred("get", 1, PGet) ;
	InstallCBuiltinPred("get", 2, PSGet) ;
	InstallCBuiltinPred("peek0", 1, PPeek0) ;
	InstallCBuiltinPred("peek0", 2, PSPeek0) ;
	InstallCBuiltinPred("peek", 1, PPeek) ;
	InstallCBuiltinPred("peek", 2, PSPeek) ;
	InstallCBuiltinPred("skip", 1, PSkip) ;
	InstallCBuiltinPred("skip", 2, PSSkip) ;
	InstallCBuiltinPred("get_line", 1, PGetLine) ;
	InstallCBuiltinPred("get_line", 2, PSGetLine) ;

	InstallCBuiltinPred("read", 1, PRead) ;
	InstallCBuiltinPred("read", 2, PSRead) ;
	InstallCBuiltinPred("varnames", 1, PVarNames) ;
	InstallCBuiltinPred("$$_read_for_assert", 1, PReadForAssert) ;

	InstallCBuiltinPred("open", 3, POpen) ;
	InstallCBuiltinPred("close", 1, PClose) ;
	InstallCBuiltinPred("set_input", 1, PSetInput) ;
	InstallCBuiltinPred("current_input", 1, PCurrentInput) ;
	InstallCBuiltinPred("set_output", 1, PSetOutput) ;
	InstallCBuiltinPred("current_output", 1, PCurrentOutput) ;

	InstallCBuiltinPred("streams", 0, PStreams) ;
}
