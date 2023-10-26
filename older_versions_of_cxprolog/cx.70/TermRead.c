/*
 *   This file is part of the CxProlog system

 *   TermRead.c
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
#include <math.h>


/* SCANNER */

typedef enum
{
	vnsTk,		atomTk,
	openRTk,	openSTk,	openCTk,
	closeRTk,	closeSTk,	closeCTk,
	commaTk,	barTk,
	dotTk
} TokenType ;

#define shortStringSize	256

#define tkAtom				((AtomPt)(tkVal))
#define tkTerm				((Pt)(tkVal))

static int cc ;
static TokenType tk ;
static VoidPt tkVal ;
static int nTokens, tkPreBlanks ;
static jmp_buf readJmp ;
static Bool delayToken ;
static CharPt inPt, tokenTextPt, peekTokenTextPt ;

static Bool IsFullStopChar(int c)
{
	return cc == '.' && (isspace(c) || c == '%' || iseof(c)) ;
}

static void ErrorPointer(int *n)
{
	int m ;
	
	if( *n == -1 ) return ;
	for( m = 0 ; m < *n ; m++ ) Put('-') ;
	PutString(*n < 75 ? "^---\n" : "^ \n") ;
	*n = -1 ;
}

static void ReaderError(CharPt s)
{
	int n ;
	register CharPt ch ;

	Write("*** READER ERROR (%s) ***\n", s) ;

	for( ch = strBuffer ; ch < tokenTextPt && *ch == '\n' ; ch++ ) ;
	for( n = 0 ; ch < tokenTextPt ; ch++ ) {
		n++ ;
		if( *ch == '\n' ) n = 0 ;
		Put(*ch) ;
	}
	for( ; ch < inPt ; ch++ ) {
		Put(*ch) ;
		if( *ch == '\n' ) ErrorPointer(&n) ;
	}

	if( tk != dotTk ) {
		while( not IsFullStopChar(Peek0()) ) {
			Put(cc = Get0()) ;
			if( cc == '\n' ) ErrorPointer(&n) ;
		}
		Nl() ;
	}

	ErrorPointer(&n) ;

	longjmp(readJmp, 1) ;
}

static int NextChar()
{
	*inPt++ = cc = Get0() ;
}

static int NextCharEOF()
{
	if( iseof(cc) ) ReaderError("Premature end of file") ;
	*inPt++ = cc = Get0() ;
	if( inPt - strBuffer > 1100 ) {
		inPt -= 50 ;
	}
}

static void ReadId(CharPt s)
{
	CharPt send = s + shortStringSize - 2 ;
	do {
		if( s >= send ) ReaderError("Atom or var name too long") ;
		*s++ = cc ;
		NextChar() ;
	} while( isalnum(cc) ) ;
	*s = '\0' ;
}

static void ReadSymbol(CharPt s)
{
	CharPt send = s + shortStringSize - 2 ;
	do {
		if( s >= send ) ReaderError("Symbol too long") ;
		*s++ = cc ;
		NextChar() ;
	} while( issymbol(cc) ) ;
	*s = '\0' ;
}

static void ReadQuoted(CharPt s)
{
	CharPt send = s + shortStringSize - 2 ;
	int quote = cc ;

	NextChar() ;	/* Skip quote */
	for(;;) {
		for( ; cc != quote ; NextCharEOF() ) {
			if( s >= send ) ReaderError("Quoted text too long") ;
			else *s++ = cc ;
		}
		NextChar() ;
		if( cc == quote ) {	/* A quote again */
			*s++ = quote ;
			NextChar() ;
		}
		else break ;
	}
	*s = '\0' ;
}

static Int ReadInt10(CharPt s)
{
	CharPt send = s + shortStringSize - 2 ;
	do {
		if( s >= send ) ReaderError("Integer too long") ;
		*s++ = cc ;
		NextChar() ;
	} while( isdigit(cc) ) ;
	*s = '\0' ;
}

static Int ReadInt(Int base, int *nd)
{
	register Int i = 0 ;
	register int n ;
	int dummy ;

	if( nd == nil ) nd = &dummy ;
	*nd = 0 ;
	for(;;) {
		if( isdigit(cc) ) n = cc - '0' ;
		elif( 'a' <= cc && cc <= 'z' ) n = 10 + cc - 'a' ;
		else break ;
		if( n >= base ) break ;
		(*nd)++ ;
		i = i * base + n ;
		NextChar() ;
	}
	if( *nd == 0 ) ReaderError("Missing digits") ;
	return i ;
}

static Int ReadQuotedInt(CharPt s)
{
	int base ;
	if( strlen(s) > 2 )
		ReaderError("Base too big in base'xxx") ;
	sscanf(s, "%d", &base) ;
	if( base > 36 )
		ReaderError("Base too big in base'xxx") ;

	NextChar() ; /* Skip quote */
	if( base == 0 ) {
		int asciiCode = cc ;
		NextCharEOF() ;
		if( isalnum(cc) )
			ReaderError("Too many chars in 0'x constant") ;
		return asciiCode ;
	}
	else return ReadInt(base, nil) ;
}

static Real ReadReal(CharPt si)
{
	double d ;
	char s[shortStringSize], rr[shortStringSize * 4]  ;

	strcpy(rr, si) ;
	if( cc == '.' ) {
		NextChar() ;	/* Skip '.' */
		strcat(rr, ".") ;
		if( isdigit(cc) ) {
			ReadInt10(s) ;
			strcat(rr, s) ;
		}
		else ReaderError("Malformed decimal part of real") ;
	}
	if( cc == 'e' || cc == 'E' ) {
		NextChar() ;
		strcat(rr, "e") ;
		if( cc == '+' ) {
			NextChar() ;
			strcat(rr, "+") ;
		}
		else if( cc == '-' ) {
			NextChar() ;
			strcat(rr, "-") ;
		}
		if( isdigit(cc) ) {
			ReadInt10(s) ;
			strcat(rr, s) ;
		}
		else ReaderError("Malformed exponent of real") ;
	}
	if( sscanf(rr, "%lf", &d) != 1 )
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
	} while( cc != '\n' && not iseof(cc) ) ;
}

static void NextToken()
{
	char s[shortStringSize] ;

	if( delayToken ) {
		tokenTextPt = peekTokenTextPt ;
		delayToken = false ;
		return ;
	}
redoL:
	for( tkPreBlanks = 0 ; isspace(cc) ; NextChar(), tkPreBlanks++ ) ;
	if( inPt - strBuffer > 1000 )
		inPt = tokenTextPt ;
	tokenTextPt = inPt - 1 ;
	switch( allChars[cc] ) {
		case _LC: {
			ReadId(s) ;
atomL:		tk = atomTk ;
			tkVal = LookupAtom(s) ;
			break ;
		}
		case _UC: {
			ReadId(s) ;
			tk = vnsTk ;
			tkVal = LookupVar(s) ;
			break ;
		}
		case _SO: {
			s[0] = cc ;
			s[1] = '\0' ;
			NextChar() ;
			goto atomL ;
		}
		case _SY: {	/* '.' and '/' are symbols */
			if( IsFullStopChar(Peek0()) )
				{ tk = dotTk ; NextChar() ; break ; }
			elif( cc == '/' && Peek0() == '*' )
				{ SkipComment() ; goto redoL ; }
			ReadSymbol(s) ;
			goto atomL ;
		}
		case _DG: {
			ReadInt10(s) ;
			tk = vnsTk ;	
			if( cc == '\'' || cc == '\"' )
				tkVal = MakeInt(ReadQuotedInt(s)) ;
			elif( ( cc == '.' && not IsFullStopChar(Peek0()) ) || cc == 'e' || cc == 'E' )
				tkVal = MakeReal(ReadReal(s)) ;
			else {
				Int i ;
				if( sscanf(s, "%ld", &i) != 1 )
					Error("invalid integer") ;
				tkVal = MakeInt(i) ;
			}
			break ;
		}
		case '`': { ReadQuoted(s) ; tk = vnsTk ;
					tkVal = MakeExtra(s, textSubTag) ; break ; }
		case '(': { tk = openRTk ; NextChar() ; break ; }
		case ')': { tk = closeRTk ; NextChar() ; break ; }
		case '[': { tk = openSTk ; NextChar() ; break ; }
		case ']': { tk = closeSTk ; NextChar() ; break ; }
		case '{': { tk = openCTk ; NextChar() ; break ; }
		case '}': { tk = closeCTk ; NextChar() ; break ; }
		case '|': { tk = barTk ; NextChar() ; break ; }
		case ',': { tk = commaTk ; NextChar() ; break ; }
		case '\'':{ ReadQuoted(s) ; goto atomL ; }
		case '\"':{ ReadQuoted(s) ; tk = vnsTk ; tkVal = StringToPString(s) ; break ; }
		case '%': {
			SkipLineComment() ;
			if( iseof(cc) ) goto eofL ;
			else goto redoL ;
		}
		case _EF: {
eofL:		if( nTokens == 0 ) longjmp(readJmp, 2) ;
			else NextCharEOF() ;	/* Generate an eof error */
			break ;
		}
		default: {
			Write("Ascii code: <%d>", cc) ;
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
	ResetVarDic() ;
	cc = ' ' ;
	nTokens = 0 ;
	tk = barTk ;
	tokenTextPt = inPt = strBuffer ;
	delayToken = false ;
}


/* TEST */

static void PrintToken()
{
	switch( tk ) {
		case atomTk:	WriteStd("atom(%s) ", AtomName(tkAtom)) ; break ;
		case vnsTk:		WriteStd("vns(") ;
						WriteTermStd(tkTerm) ;
						WriteStd(") ") ;
						break ;
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

static Hdl readStack ;
static Pt Term(int n) ;

static Pt Args(AtomPt atom)
{
	Hdl save = readStack ;
	int arity ;

	NextToken() ;	/* skip ( */
	do {
		Push(readStack, Term(subPrec)) ;
		NextToken() ;
	}
	while( tk == commaTk ) ;
	if( tk != closeRTk ) ReaderError("')' expected") ;
	arity = readStack - save ;
	readStack = save ;
	return MakeStruct(LookupFunctor(atom, arity), save) ;
}

static Pt RoundBrackets()
{
	Pt t ;

	t = Term(maxPrec) ;
	NextToken() ;
	if( tk != closeRTk ) ReaderError("')' expected") ;
	return t ;
}

static Pt SquareBrackets()
{
	Hdl save = readStack ;
	int n ;

	PeekToken() ;	/* peek after [ */
	if( tk == closeSTk ) {
		NextToken() ;	/* skip ] */
		return tNilAtom ;
	}
	Push(readStack, Term(subPrec)) ;
	NextToken() ;
	while( tk == commaTk ) {
		Push(readStack, Term(subPrec)) ;
		NextToken() ;
	}	
	if( tk == barTk ) {
		Push(readStack, Term(subPrec)) ;
		NextToken() ;
	}
	else Push(readStack, tNilAtom) ;
	if( tk != closeSTk ) ReaderError("']' expected") ;
	n = readStack - save ;
	readStack = save ;
	return ArrayToList(n, save) ;
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
	if( atom == LookupAtom("-") && tk == vnsTk && IsNumber(tkTerm) ) {
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
		case closeRTk: case closeSTk: case closeCTk:
		case commaTk: case barTk: case dotTk:			goto errorL ;
		case atomTk: {
			atom = tkAtom ;
			PeekToken() ;
			if( tk == openRTk && tkPreBlanks == 0 ) {
				res = Args(atom) ;
				break ;
			}
			elif( p = Prefix(atom, &rp) ) {
				if( (res = MinusOp(atom)) != nil ) break ;
				if( n < p ) goto errorL ;
				switch( tk ) {
					case vnsTk: case openRTk:
					case openSTk: case openCTk:					goto prefixL ;
					case commaTk: case closeRTk: case closeSTk:
					case closeCTk: case dotTk: case barTk:		goto atomL ;
					case atomTk: {
						if( Infix(tkAtom, &lp, &rp) && m <= lp ) goto atomL ;
						elif( Postfix(tkAtom, &lp) && m <= lp ) goto atomL ;
						else goto prefixL ;
					}
				}
				InternalError("Term (1)") ;
			}
			else res = TagAtom(atom) ;
			break ;
		}
		default:
			InternalError("Term (2)") ;
	}

rightL:
	PeekToken() ;
	switch( tk ) {
		case dotTk: case closeRTk: case closeSTk: case closeCTk:	goto doneL ;
		case vnsTk: case openRTk: case openSTk: case openCTk:		goto errorL ;
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
			atom = tkAtom ;
			if( (p = Postfix(atom, &lp)) && n >= p && m <= lp ) {
				NextToken() ;
				if( (p = Infix(atom, &lp, &rp)) && n >= p && m <= lp ) {
					PeekToken() ;
					switch( tk ) {
						case openRTk: case openSTk:
						case openCTk: case vnsTk:		goto infixL ;
						case commaTk: case barTk:
						case closeRTk: case closeSTk:	goto posfixL ;
						case atomTk: {
							if( ExclusivelyPrefix(tkAtom) ) goto infixL ;
							else goto posfixL ;
						}
					}
					InternalError("Term (3)") ;
				}
				else goto posfixL ;
			}
			elif( (p = Infix(atom, &lp, &rp)) && n >= p && m <= lp ) {
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

Pt ReadTerm(Hdl vars)
{
	Hdl saveStructState ;

	InitLex() ;
	readStack = codeBuffer ;
	saveStructState = H ;
	switch( setjmp(readJmp) ) {
		case 0: {
			Pt res = Term(maxPrec) ;
			NextToken() ;
			if( tk != dotTk) ReaderError("'.' expected") ;
			if( vars != nil) *vars = VarDicToEqList() ;
			return res ;
		}
		case 1: {
			H = saveStructState ;
			return nil ;
		}
		case 2: {
			H = saveStructState ;
			if( vars != nil) *vars = tNilAtom ;
			return tEofAtom ;
		}
	}
	InternalError("ReadTerm") ;
}
