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
static Bool delayToken, classicAtomsRead ;
static CharPt inPt, tokenTextPt, peekTokenTextPt ;
static Hdl readStack ;

static char auxBuffer[2000] ;	/* @@ to remove */

static CharPt strGet ;
#define GetChar0()		(strGet ? *strGet++ : Get0())
#define PeekChar0()		(strGet ? *strGet : Peek0())

static Bool IsFullStop(int c)
{
	return cx_isspace(c) || c == '%' || cx_iseof(c) ;
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

	if( strGet )
		longjmp(readJmp, 1) ;
	
	Write("*** READER ERROR (%s) ***\n", s) ;

	for( ch = auxBuffer ; ch < tokenTextPt && *ch == '\n' ; ch++ ) ;
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
		while( cc != '.' || not IsFullStop(PeekChar0()) ) {
			Put(cc = GetChar0()) ;
			if( cc == '\n' ) ErrorPointer(&n) ;
		}
		Nl() ;
	}

	ErrorPointer(&n) ;

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
			if( classicAtomsRead ) {
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
			if( cc == '.' && IsFullStop(PeekChar0()) )
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
			elif( ( cc == '.' && not IsFullStop(PeekChar0()) )
					|| cc == 'e' || cc == 'E' )
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
eofL:		if( nTokens == 0 ) longjmp(readJmp, 2) ;
			elif( strGet ) { tk = dotTk ; break ; }	/* fake that dotTk was seen */
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
	tokenTextPt = inPt = auxBuffer ;
	delayToken = false ;
}


/* TEST */

static void PrintToken()
{
	switch( tk ) {
		case atomTk:	WriteStd("atom(%s) ", XAtomOrTextName(tkTerm)) ; break ;
		case textTk:	WriteStd("text(%s) ", XAtomOrTextName(tkTerm)) ; break ;
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
			elif( p = Prefix(atom, &rp) ) {
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
			if( (p = Postfix(atom, &lp)) && n >= p && m <= lp ) {
				NextToken() ;
				if( (p = Infix(atom, &lp, &rp)) && n >= p && m <= lp ) {
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

static Pt DoReadTerm(Hdl vars)
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
			if( vars != nil ) *vars = VarDicToEqList() ;
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
			if( vars != nil) *vars = tNilAtom ;
			FreeBuffer() ;
			return tEofAtom ;
		}
	}
	InternalError("DoReadTerm") ;
	return nil ;
}

Pt ReadTerm(Hdl vars)
{
	classicAtomsRead = false ;
	return DoReadTerm(vars) ;
}

Pt ReadTermClassicAtoms(Hdl vars)
{
	classicAtomsRead = true ;
	return DoReadTerm(vars) ;
}

Pt ReadTermFromStr(CharPt s, Hdl vars)
{
	Pt t ;

	strGet = s ;
	NullIsEof() ;
	classicAtomsRead = false ;
	t = DoReadTerm(vars) ;
	strGet = nil ;
	NullIsNotEof() ;
	return t ;
}


/* CXPROLOG C'BUILTINS */

static void PSee()
{
	See(XTestAtomName(X0)) ;
	JumpNext()
}

static void PSeeing()
{
	if( UnifyWithAtomic(X0, MakeAtomOrText(Seeing())) ) JumpNext()
	DoFail()
}

static void PSeen()
{
	Seen() ;
	JumpNext()
}

static void PGet0()
{
	int c = Get0() ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PGet()
{
	int c = Get() ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PPeek()
{
	int c = Peek0() ;
	if( c == '\n' ) c = 10 ;
	if( UnifyWithNumber(X0, MakeInt(c)) ) JumpNext()
	DoFail()
}

static void PSkip()
{
	int c = XTestInt(X0) ;
	if( c == 10 ) c = '\n' ;
	Skip(c) ;
	JumpNext()
}

static void PGetLine()
{
	if( UnifyWithAtomic(X0, MakeAtomOrText(GetLine())) ) JumpNext()
	DoFail()
}

static void PRead()
{
	Pt t ;
	if( ( t = ReadTerm(nil) ) != nil && Unify(X0, t) ) JumpNext()
	DoFail()
}

static void PRead2()
{
	Pt t, names ;
	if( ( t = ReadTerm(&names) ) != nil &&
		Unify(X0, t) && Unify(X1, names) ) JumpNext()
	DoFail()
}

static void PTopRead2()
{
	Pt t, names ;

	WritePrompt() ;
	if( ( t = ReadTerm(&names) ) != nil &&
		Unify(X0, t) && Unify(X1, names) ) JumpNext()
	DoFail()
}

static void PARead()
{
	Pt t ;
	if( ( t = ReadTermClassicAtoms(nil) ) != nil && Unify(X0, t) ) JumpNext()
	DoFail()
}

static void PARead2()
{
	Pt t, names ;
	if( ( t = ReadTermClassicAtoms(&names) ) != nil &&
		Unify(X0, t) && Unify(X1, names) ) JumpNext()
	DoFail()
}

void InitTermRead()
{
	InstallCBuiltinPred("see", 1, PSee) ;
	InstallCBuiltinPred("seeing", 1, PSeeing) ;
	InstallCBuiltinPred("seen", 0, PSeen) ;

	InstallCBuiltinPred("get0", 1, PGet0) ;
	InstallCBuiltinPred("get", 1, PGet) ;
	InstallCBuiltinPred("peek", 1, PPeek) ;
	InstallCBuiltinPred("skip", 1, PSkip) ;
	InstallCBuiltinPred("get_line", 1, PGetLine) ;

	InstallCBuiltinPred("read", 1, PRead) ;
	InstallCBuiltinPred("read", 2, PRead2) ;
	InstallCBuiltinPred("@@_top_read", 2, PTopRead2) ;
	InstallCBuiltinPred("aread", 1, PARead) ;
	InstallCBuiltinPred("aread", 2, PARead2) ;
}
