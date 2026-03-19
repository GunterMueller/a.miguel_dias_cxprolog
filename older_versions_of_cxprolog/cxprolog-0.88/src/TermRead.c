/*
 *   This file is part of the CxProlog system

 *   TermRead.c
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
#include <math.h>


/* VARS */

#define initialVarsSize		256

static Hdl varsBegin, varsEnd, varsPt ;

static void VarsInit()
{
	varsBegin = PrimitiveAllocate(initialVarsSize) ;
	varsEnd = varsBegin + initialVarsSize ;
}

static Size NVars()
{
	return (varsBegin - varsEnd) / 2 ;
}

static void VarsExpand()
{
	Size size = varsEnd - varsBegin ;
	Hdl newVarsBegin, newVarsEnd ;
	MemoryGrowWarning("vars", size, size * 2, nil) ;
	newVarsBegin = PrimitiveRellocate(varsBegin, size, size * 2) ;
	newVarsEnd = newVarsBegin + size * 2 ;
	varsPt += newVarsBegin - varsBegin ;
	varsBegin = newVarsBegin ;
	varsEnd = newVarsEnd ;
}

static void DoTermReadVarTableRellocForStacks(Size globalOffset, Size localOffset)
{
	register Hdl h ;
	for( h = varsBegin ; h < varsPt ; h += 2 )
		h[1] += globalOffset ;
}

static void TermReadAtomGCMark()
{
	AtomGCMarkRange(varsBegin, varsPt) ;
}

static void VarsReset()
{
	varsPt = varsBegin ;
}

static Pt LookupVar(Pt at)
{
	register Hdl h ;
	Pt t ;
	if( at == tUnderAtom )
		return MakeVar() ;
	for( h = varsBegin ; h < varsPt ; h += 2 )
		if( h[0] == at )
			return h[1] ;
	if( (t = LookupCurrUnitParameter(at)) != nil )
		return t ;
	if( h == varsEnd )
		VarsExpand() ;
	*varsPt++ = at ;
	*varsPt++ = MakeVar() ;
	return varsPt[-1] ;
}

static Pt MakeVarList()
{
	register Hdl h ;
	register Pt list ;
	list = tNilAtom ;
	for( h = varsPt - 2 ; h >= varsBegin ; h -= 2 )
		list = MakeBinStruct(
				listFunctor,
				MakeBinStruct(eqFunctor, h[0], h[1]),
				list) ;
	return list ;
}



/* TERM BUFFER  */

#define maxTextBuffer	1024
static Char textBuffer[maxTextBuffer] ;
static CharPt inPt, inEndPt ;

#define TextBufferAdd(c)	if( inPt < inEndPt ) *inPt++ = (c)
#define TextBufferBack()	inPt--

static void InitTextBuffer()
{
	inPt = textBuffer ;
	inEndPt = textBuffer + maxTextBuffer ;
}


/* TOKENS */

typedef enum
{
	nxTk,		varTk,		strTk,		atomTk,
	openRTk0,
	openRTk,	openSTk,	openCTk,
	closeRTk,	closeSTk,	closeCTk,
	commaTk,	barTk,		dotTk,		endMarkTk
} TokenType ;

static Size nTokens ;
static TokenType tk ;
static Pt tkTerm ;
static Hdl tks ;
static FunctorPt constTkFunctor, varTkFunctor, strTkFunctor, atomTkFunctor ;
static Pt tDotTk, tOpenRTk0, tOpenRTk, tCloseRTk, tOpenSTk,
		  tCloseSTk, tOpenCTk, tCloseCTk, tCommaCTk, tBarTk ;

static void TokensInit()
{
	constTkFunctor = LookupFunctorByName("const_tk", 1) ;
	strTkFunctor = LookupFunctorByName("str_tk", 1) ;
	varTkFunctor = LookupFunctorByName("var_tk", 1) ;
	atomTkFunctor = LookupFunctorByName("atom_tk", 1) ;
	tDotTk = MakeAtom("dot_tk") ;
	tOpenRTk0 = MakeAtom("open_round0_tk") ;
	tOpenRTk = MakeAtom("open_round_tk") ;
	tCloseRTk = MakeAtom("close_round_tk") ;
	tOpenSTk = MakeAtom("open_square_tk") ;
	tCloseSTk = MakeAtom("close_square_tk") ;
	tOpenCTk = MakeAtom("open_curly_tk") ;
	tCloseCTk = MakeAtom("close_curly_tk") ;
	tCommaCTk = MakeAtom("comma_tk") ;
	tBarTk = MakeAtom("bar_tk") ;
}

static void GenToken(TokenType tt, Pt term)
{
	ScratchPush(tt) ;
	ScratchPush(term) ;
	nTokens++ ;
}

static void GenTokensStart()
{
	nTokens = 0 ;
	UseScratch() ;
}

static void GenTokensEnd()
{
	GenToken(endMarkTk, nil) ;
	FreeScratch() ;
}

static void StartReadingTokens(Hdl start)
{
	tks = start ;
}

static void NextToken()
{
	tk = ((TokenType)tks[0]) ;
	tkTerm = tks[1] ;
	tks += 2 ;
}

static void PeekToken()
{
	tk = ((TokenType)tks[0]) ;
	tkTerm = tks[1] ;
}

static Pt TokenAsTkTerm()
{
	switch( tk ) {
		case nxTk:		return MakeUnStruct(constTkFunctor, tkTerm) ;
		case varTk:		return MakeUnStruct(varTkFunctor, tkTerm) ;
		case strTk:		return MakeUnStruct(strTkFunctor, tkTerm) ;
		case atomTk:	return MakeUnStruct(atomTkFunctor, tkTerm) ;
		case openRTk0:	return tOpenRTk0 ;
		case openRTk:	return tOpenRTk ;
		case openSTk:	return tOpenSTk ;
		case openCTk:	return tOpenCTk ;
		case closeRTk:	return tCloseRTk ;
		case closeSTk:	return tCloseSTk ;
		case closeCTk:	return tCloseCTk ;
		case commaTk:	return tCommaCTk ;
		case barTk:		return tBarTk ;
		case dotTk:		return tDotTk ;
		default:		return InternalError("TokenAsTkTerm") ;
	}
}


/* SCANNER */

static int cc ;
static jmp_buf readJmp ;
static CharPt lexErrorMesg ;
static Bool onlyTokens ;

static StreamPt input ;
static Bool specialRead ;
#define GetChar0()		(StreamGetChar(input))
#define PeekChar0()		(StreamPeekChar(input))

static Bool CheckFullStop(Bool skip)
{
	int c = PeekChar0() ;
	if( cx_isspace(c) ) {
		if( skip ) GetChar0() ;
		return true ;
	}
	return c == '%' || cx_iseof(c) ;
}

static void InitLex()
{
	lexErrorMesg = nil ;
	cc = ' ' ;
}

static void LexError(CharPt s)
{
	if( specialRead )
		longjmp(readJmp, 1) ;

	if( lexErrorMesg == nil )
		lexErrorMesg = s ;
}

static void FinishErrors()
{
	if( lexErrorMesg != nil ) {
		register CharPt ch ;
		WriteErr("*** READER ERROR (%s) ***\n", lexErrorMesg) ;
		for( ch = textBuffer ; ch < inPt ; ch++ )
			WriteErr("%c", *ch) ;
		WriteErr(inPt < inEndPt ? "\n" : " ...\n" ) ;
		longjmp(readJmp, 1) ;
	}
}

static void ParserError(CharPt s)
{
	LexError(s) ;
	FinishErrors() ;
}

static void NextChar()
{
	cc = GetChar0() ;
	TextBufferAdd(cc) ;
}

static void NextCharEOF()
{
	cc = GetChar0() ;
	TextBufferAdd(cc) ;
	if( cx_iseof(cc) )
		ParserError("Premature end of file") ;
}

static void SkipChar()
{
	cc = GetChar0() ;
}

static void SkipCharEOF()
{
	cc = GetChar0() ;
	if( cx_iseof(cc) )
		ParserError("Premature end of file") ;
}

static CharPt ReadId()
{
	ScratchSave() ;
	do {
		ScratchAddCh(cc) ;
		NextChar() ;
	} while( cx_isalnum(cc) ) ;
	ScratchAddCh('\0') ;
	return ScratchRestore() ;
}

static CharPt ReadSymbol()
{
	ScratchSave() ;
	do {
		ScratchAddCh(cc) ;
		NextChar() ;
	} while( cx_issymbol(cc) ) ;
	ScratchAddCh('\0') ;
	return ScratchRestore() ;
}

static CharPt ReadQuoted()
{
	int quote = cc ;
	ScratchSave() ;
	NextCharEOF() ;	/* Skip first quote */
	for(;;) {
		for( ; cc != quote ; NextCharEOF() )
			ScratchAddCh(cc) ;
		NextChar() ;
		if( cc == quote ) {	/* a quote again */
			ScratchAddCh(cc) ;
			NextCharEOF() ;	/* skip second quote */
		}
		else break ;
	}
	ScratchAddCh('\0') ;
	return ScratchRestore() ;
}

static CharPt ReadSolo()
{
	ScratchSave() ;
	ScratchAddCh(cc) ;
	NextChar() ;
	ScratchAddCh('\0') ;
	return ScratchRestore() ;
}

static CharPt ReadInt10()
{
	ScratchSave() ;
	do {
		ScratchAddCh(cc) ;
		NextChar() ;
	} while( cx_isdigit(cc) ) ;
	ScratchAddCh('\0') ;
	return ScratchRestore() ;
}

static Pt ReadAndMakeInt(int base)
{
	register PInt i = 0 ;
	register int n ;
	for(;;) {
		if( cx_isdigit(cc) ) n = cc - '0' ;
		elif( 'a' <= cc && cc <= 'z' ) n = 10 + cc - 'a' ;
		else break ;
		if( n >= base ) break ;
		i = i * base + n ;
		NextChar() ;
	}
	return MakeInt(i) ;
}

static Pt ReadAndMakeNumber(CharPt s)
{
	Str1K sr ;
	Pt res ;
	Bool error = false ;
	if( strlen(s) > 256 ) {
		LexError("Number too long") ;
		error = true ;
	}
	else strcpy(sr, s) ;
	if( cc == '.' && !CheckFullStop(false) ) {
		NextChar() ;	/* Skip '.' */
		strcat(sr, ".") ;
		if( cx_isdigit(cc) ) {
			s = ReadInt10() ;
			if( strlen(s) > 256 ) {
				LexError("Float too long") ;
				error = true ;
			}
			else strcat(sr, s) ;
		}
		else {
			LexError("Missing decimal part of float") ;
			error = true ;
		}
	}
	if( cc == 'e' || cc == 'E' ) {
		NextChar() ;
		strcat(sr, "e") ;
		if( cc == '+' )
			NextChar() ;
		else if( cc == '-' ) {
			NextChar() ;
			strcat(sr, "-") ;
		}
		if( cx_isdigit(cc) ) {
			s = ReadInt10() ;
			if( strlen(s) > 256 ) {
				LexError("Float too long") ;
				error = true ;
			}
			else strcat(sr, s) ;
		}
		else {
			LexError("Missing exponent of float") ;
			error = true ;
		}
	}
	if( cx_isalnum(cc) ) {
		LexError("Bad ending in number") ;
		error = true ;
	}
	if( error )
		return nil ;
	if( (res = NumberFromStr(sr)) == nil ) {
		LexError("Malformed float") ;
		return nil ;
	}
	else return res ;
}

static void SkipComment(void)
{
	TextBufferBack() ;
	SkipChar() ;		/* skip '/', get '*' */
	while( cc != '/' ) {	/* start with a '*' */
		do { SkipCharEOF() ; } while( cc != '*' ) ;
		do { SkipChar() ; } while( cc == '*' ) ;
	}
	TextBufferAdd(' ') ;	/* Replace commentary by a ' ' */
	NextChar() ;	/* skip '/' */
}

static void SkipLineComment(void)
{
	TextBufferBack() ;
	do { SkipCharEOF() ; } while( cc != '\n' ) ;
	TextBufferAdd('\n') ;	/* Replace commentary by a NL */
	/* Doesn't skip NL so that read_tokens/1 works well */
}

static Size GenerateTokens()
{
	InitTextBuffer() ;
	InitLex() ;
	GenTokensStart() ;
	for(;;) {
		switch( CharType(cc) ) {
			case _BL: {
				do {
					if( cc == '\n' && onlyTokens ) goto exitL ;
					NextChar() ;
				} while( cx_isspace(cc) ) ;
afterSpaceL:	if( cc == '(' ) {
					GenToken(openRTk, nil) ;
					NextChar() ;
				}
				break ;
			}
			case _LC: GenToken(atomTk, MakeTempAtom(ReadId())) ; break ;
			case _UC: GenToken(varTk, MakeTempAtom(ReadId())) ; break ;
			case _SO: GenToken(atomTk, MakeTempAtom(ReadSolo())) ; break ;

			case '(': GenToken(openRTk0, nil) ; NextChar() ; break ;
			case ')': GenToken(closeRTk, nil) ; NextChar() ; break ;
			case '[': NextChar() ;
					  if( cc == ']' )
					  	{ GenToken(atomTk, tNilAtom) ; NextChar() ; }
					  else GenToken(openSTk, nil) ;
					  break ;
			case ']': GenToken(closeSTk, nil) ; NextChar() ; break ;
			case '{': NextChar() ;
					  if( cc == '}' )
						{ GenToken(atomTk, tBracketsAtom) ; NextChar() ; }
					  else GenToken(openCTk, nil) ;
					  break ;
			case '}': GenToken(closeCTk, nil) ; NextChar() ; break ;
			case '|': GenToken(barTk, nil) ; NextChar() ; break ;
			case ',': GenToken(commaTk, nil) ; NextChar() ; break ;

			case _SY: {	/* '.' and '/' are symbols */
				if( cc == '.' && CheckFullStop(true) ) {
					GenToken(dotTk, nil) ;
					goto exitL ;
				}
				elif( cc == '/' && PeekChar0() == '*' ) {
					SkipComment() ;
					goto afterSpaceL ;
				}
				else
					GenToken(atomTk, MakeTempAtom(ReadSymbol())) ;
				break ;
			}
			case '%': {
				SkipLineComment() ;
				break ;
			}
			case _DG: {
				CharPt s = ReadInt10() ;
				if( cc == '\'' || cc == '\"' ) {
					int base = strlen(s) < 4 ? atoi(s) : 999 ;
					NextCharEOF() ; /* Skip quote */
					switch( base ) {
						case 0: {
							int c = cc ;
							NextChar() ;
							if( cx_isalnum(cc) )
								LexError("Bad ending in 0'x literal") ;
							else
								GenToken(nxTk, MakeInt(c)) ;
							break ;
						}
						case 1: {
							Pt t ;
							if( (t = MakeExtraFromStr(ReadId())) == nil )
								LexError("Invalid 1'xxx_yyy literal") ;
							else
								GenToken(nxTk, t) ;
							break ;
						}
						default: {
							if( base > 36 )
								LexError("Too big a base in base'xxx. Max is 36.") ;
							else
								GenToken(nxTk, ReadAndMakeInt(base)) ;
							break ;
						}
					}
				}
				else {
					Pt t = ReadAndMakeNumber(s) ;
					if( t != nil )
						GenToken(nxTk, t) ;
				}
				break ;
			}

			case '\'': {
				GenToken(atomTk, MakeTempAtom(ReadQuoted())) ;
				break ;
			}
			case '\"':{
				CharPt s = ReadQuoted() ;
				if( compatibleStrings_flag )
					nTokens += strlen(s) ;
				GenToken(strTk, MakeTempAtom(s)) ;
				break ;
			}
            case _EF: {
                if( nTokens == 0 )  /* inject 'end_of_file'. */
                    longjmp(readJmp, 2) ;
                if( onlyTokens ) goto exitL ;
 				elif( specialRead )
					GenToken(dotTk, nil) ;
				else NextCharEOF() ; /* Generate an eof error */
                goto exitL ;
            }
			case __I: {
				NextChar() ;	
				LexError("Invalid character") ;
				break ;
			}
			default: {
				Warning("Ascii code: %d - Category: %d", cc, CharType(cc)) ;
				InternalError("GenerateTokens") ;
			}
		}
	}
exitL:	
	GenTokensEnd() ;
	FinishErrors() ;
	return nTokens ;
}

/* THE PARSER */

static Pt Term(int n) ;

static Pt Args(AtomPt atom)
{
	int arity = 0 ;
	NextToken() ;	/* skip ( */
	do {
		ScratchPush(Term(subPrec)) ;
		if( ++arity > maxArity )
			FileError("Highest arity (%d) exceeded on functor '%s/%d'",
								maxArity, AtomName(atom), arity) ;
		NextToken() ;
	} while( tk == commaTk ) ;
	if( tk != closeRTk ) ParserError("expected ')'") ;
	return MakeStruct(LookupFunctor(atom, arity), ScratchBack(arity)) ;
}

static Pt RoundBrackets()
{
	OperatorPt op ;
	Pt t = Term(maxPrec) ;
	NextToken() ;
	if( tk != closeRTk ) ParserError("expected ')'") ;
	
	if( IsStruct(t) && (op = AtomToOperator(XStructAtom(t))) != nil && OperatorParenthesised(op) )
		return MakeUnStruct(parFunctor,t) ; /* parenthesised operator */
	else
		return t ;
}

static Pt SquareBrackets()
{
	Size n = 0 ;
	PeekToken() ;	/* peek after [ */
	if( tk == closeSTk ) {
		NextToken() ;	/* skip ] */
		return tNilAtom ;
	}
	do {
		ScratchPush(Term(subPrec)) ;
		n++ ;
		NextToken() ;
	} while( tk == commaTk ) ;
	if( tk == barTk ) {
		ScratchPush(Term(subPrec)) ;
		NextToken() ;
	}
	else ScratchPush(tNilAtom) ;
	n++ ;
	if( tk != closeSTk ) ParserError("expected ']'") ;
	return ArrayToOpenList(ScratchBack(n), n) ;
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
	if( tk != closeCTk ) ParserError("expected '}'") ;
	return MakeUnStruct(bracketsFunctor, t) ;
}

static Pt MinusOp(AtomPt atom)
{
	if( atom == XAtom(tMinusAtom) && tk == nxTk && IsNumber(tkTerm) ) {
		Pt t = tkTerm ;
		NextToken() ;
		return IsInt(t) ? MakeInt(-XInt(t)) : MakeFloat(-XFloat(t)) ;
	}
	else return nil ;
}

/*
	The grammar:

	term		--> subterm(1200), dotTk.
	subterm(N)	--> term(M), { M <= N }.
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
	term(0)		--> atomTk, openRTk0, args, closeRTk.
				--> openRTk, subterm(1200), closeRTk.
				--> openCTk, subterm(1200), closeCTk.
				--> list | atomTk | strTk | nxTk | varTk.
*/

static Pt Term(int n)
{
	register Pt res ;
	AtomPt atom ;
	int m = 0,
		p, i,		/* priority of current operator */
		lp,	li,		/* priority of the left argument */
		rp, ri ;	/* priority of the right argument */
	NextToken() ;
	switch( tk ) {
		case openRTk0:	res = RoundBrackets() ; goto continueL ;
		case openRTk:	res = RoundBrackets() ; goto continueL ;
		case openSTk:	res = SquareBrackets() ; goto continueL ;
		case openCTk:	res = CurlyBrackets() ; goto continueL ;

		case closeRTk:	ParserError("invalid ')'") ;
		case closeSTk:	ParserError("invalid ']'") ;
		case closeCTk:	ParserError("invalid '}'") ;
		case commaTk:	ParserError("invalid ','") ;
		case barTk:		ParserError("invalid '|'") ;
		case dotTk:		ParserError("premature '.'") ;

		case nxTk:		res = tkTerm ; goto continueL ;
		case varTk:		res = LookupVar(tkTerm) ; goto continueL ;
		case strTk:		if( compatibleStrings_flag )
							res = StringToPString(XAtomName(tkTerm)) ;
						else
							res = MakeUnStruct(stringFunctor, tkTerm) ;
						goto continueL ;
		case atomTk: {
			res = tkTerm ;
			atom = XAtom(tkTerm) ;
			PeekToken() ;
			if( tk == openRTk0 ) {
				res = Args(atom) ;
				goto continueL ;
			}
			elif( (p = Prefix(atom, &rp)) != 0 ) {
				Pt temp_res = MinusOp(atom) ;
				if( temp_res != nil ) {
					res = temp_res ;
					goto continueL ;
				}
				if( n < p ) {
					NextToken() ;
					ParserError("Incompatible precedences (1)") ;
				}
				switch( tk ) {
					case nxTk:		goto prefixL ;
					case varTk:		goto prefixL ;
					case strTk:		goto prefixL ;
					case openRTk0:	goto prefixL ;
					case openRTk:	goto prefixL ;
					case openSTk:	goto prefixL ;
					case openCTk:	goto prefixL ;
					case atomTk: {
						if(  (Infix(XAtom(tkTerm), &lp, &rp) && m <= lp)
						  || (Postfix(XAtom(tkTerm), &lp) && m <= lp) )
								/* fall through */ ;
						else goto prefixL ;
					}
					case commaTk:
					case closeRTk:
					case closeSTk:
					case closeCTk:
					case dotTk:
					case barTk: {
						if( n < m ) {
							NextToken() ;
							ParserError("Incompatible precedences (2)") ;
						}
						res = TagAtom(atom) ;
						goto continueL ;
					}
					default: break ;
				}
				InternalError("Term (1)") ;
			}
			else goto continueL ;
		}
		default: break ;
	}
	InternalError("Term (2)") ;

continueL:
	PeekToken() ;
	switch( tk ) {
		case closeRTk:	return res ; /* doesn't consume token */ 
		case closeSTk:	return res ; /* doesn't consume token */ 
		case closeCTk:	return res ; /* doesn't consume token */ 
		case dotTk:		return res ; /* doesn't consume token */ 

		case nxTk:		NextToken() ; ParserError("misplaced constant") ;
		case varTk:		NextToken() ; ParserError("misplaced var") ;
		case strTk:		NextToken() ; ParserError("misplaced string") ;
		case openRTk0:	NextToken() ; ParserError("misplaced '('") ;
		case openRTk:	NextToken() ; ParserError("misplaced '('") ;
		case openSTk:	NextToken() ; ParserError("misplaced '['") ;
		case openCTk:	NextToken() ; ParserError("misplaced '{'") ;

		case commaTk: {
			if( n >= commaPrec && m < commaPrec ) {
				NextToken() ;
				m = commaPrec ;
				res = MakeBinStruct(commaFunctor, res, Term(m)) ;
				if( n > m ) goto continueL ;
				else return res ;
			}
			else return res ; /* doesn't consume token */
		}
		case barTk: {
			if( n >= barPrec && m < barPrec ) {
				NextToken() ;
				m = barPrec ;
				if( barIsSemicolon_flag )
					res = MakeBinStruct(semicolonFunctor, res, Term(m)) ;
				else
					res = MakeBinStruct(barFunctor, res, Term(m)) ;
				if( n > m ) goto continueL ;
				else return res ;
			}
			else return res ; /* doesn't consume token */
		}
		case atomTk: {
			atom = XAtom(tkTerm) ;
			if( (p = Postfix(atom, &lp)) != 0 && n >= p && m <= lp ) {
				NextToken() ;
				if( (i = Infix(atom, &li, &ri)) != 0 && n >= i && m <= li ) {
					/* postfix and infix - look one token ahead to decide ... */
					PeekToken() ;
					switch( tk ) {
						case nxTk:		goto infixL ;
						case varTk:		goto infixL ;
						case strTk:		goto infixL ;
						case openRTk0:	goto infixL ;
						case openRTk:	goto infixL ;
						case openSTk:	goto infixL ;
						case openCTk:	goto infixL ;

						case closeCTk:	goto postfixL ;
						case closeRTk:	goto postfixL ;
						case closeSTk:	goto postfixL ;
						case commaTk:	goto postfixL ;
						case barTk:		goto postfixL ;
						case dotTk:		goto postfixL ;
                        case atomTk: {
 							int x, lx, rx ;
							AtomPt atx = XAtom(tkTerm) ;
							if( AtomToOperator(atx) == nil )
								goto infixL ;
#if 1
						/* tries infix-prefix  */
							elif( (x = Prefix(atx, &rx)) != 0 && ri >= x )
								goto infixL ;
							else goto postfixL ;
#else
						/* tries posfix-posfix  */
							elif( (x=Postfix(atx, &lx)) != 0 && lp <= x )
								goto postfixL ;
						/* tries posfix-infix  */
							elif( (x=Infix(atx, &lx, &rx)) != 0 && p <= lx )
								goto postfixL ;
						/* selects infix-prefix */
							else
								goto infixL ;
#endif
					}
						default: break ;
					}
					InternalError("Term (3)") ;
				}
				else goto postfixL ;
			}
			elif( (i = Infix(atom, &li, &ri)) != 0 && n >= i && m <= li ) {
				NextToken() ;
				goto infixL ;
			}
			else return res ; /* doesn't consume token */
		}
		default: break ;
	}
	InternalError("Term (4)") ;

prefixL:
	res = MakeUnStruct(LookupFunctor(atom, 1), Term(rp)) ;
	m = p ;
	goto continueL ;
infixL:
	p = i ; lp = li ; rp = ri ;
	res = MakeBinStruct(LookupFunctor(atom, 2), res, Term(rp)) ;
	m = p ;
	goto continueL ;
postfixL:
	res = MakeUnStruct(LookupFunctor(atom, 1), res) ;
	m = p ;
	goto continueL ;
}

static Pt ZDoReadTokens(StreamPt srm)
{
	input = srm ;
	HSave() ;
	onlyTokens = true ;
	switch( setjmp(readJmp) ) {
		case 0: {
			Pt list = tNilAtom ;
			Hdl h = &list + 1 ;
			Size expectedSize = 6 * GenerateTokens() ;
			ZEnsureFreeSpaceOnStacks(expectedSize, "ZDoReadTokens") ;

			StartReadingTokens(ScratchBegin()) ;
			for(;;) {
				NextToken() ;
				if( tk == endMarkTk ) break ;
				h[-1] = MakeList(TokenAsTkTerm(), tNilAtom) ;
				h = H ;
			} ;
			if( expectedSize < HGrown() )
				FatalError("expectedSize = %ld, grown = %ld",
					expectedSize, HGrown()) ;
			FreeScratch() ;
			return list ;
		}
		case 1: {
			HRestore() ;
			FreeScratch() ;
			return nil ;
		}
		case 2: {
			FreeScratch() ;
			HRestore() ;
			return eofPt ;
		}
	}
	InternalError("ZDoReadTokens") ;
	return nil ;
}

static Pt ZDoReadTerm(StreamPt srm, Bool special)
{
	input = srm ;
	specialRead = special ;
	HSave() ;
	onlyTokens = false ;
	switch( setjmp(readJmp) ) {
		case 0: {
			Pt res ;
			Size expectedSize = 2 * GenerateTokens() ;
			ZEnsureFreeSpaceOnStacks(expectedSize, "ZDoReadTerm") ;

			VarsReset() ;
			StartReadingTokens(UseScratch()) ;
			res = Term(maxPrec) ;
			NextToken() ;
			if( tk != dotTk ) ParserError("Malformed term") ;
			if( expectedSize < HGrown() ) {
				Warning("**** expectedSize = %ld, grown = %ld",
					expectedSize, HGrown()) ;
				ParserError("Internal Error") ;
			}
			FreeScratch() ;
			return res ;
		}
		case 1: {
			FreeScratch() ;
			HRestore() ;
			VarsReset() ;
			return nil ;
		}
		case 2: {
			FreeScratch() ;
			HRestore() ;
			VarsReset() ;
			return eofPt ;
		}
	}
	InternalError("ZDoReadTerm") ;
	return nil ;
}

/* PUBLIC */

Pt ZReadTerm(StreamPt srm)
{
    return ZDoReadTerm(srm, false) ;
}

Pt ZTermFromStr(CharPt s)
{
 	StreamPt input = StringStreamOpen(s) ;
    Pt t = ZDoReadTerm(input, true) ;
	if( !StreamAtEnd(input) ) t = nil ;
	StreamClose(input) ;
    return t ;
}

Pt ZTermFromList(Pt list)
{
 	StreamPt input = ListStreamOpen(list) ;
    Pt t = ZDoReadTerm(input, true) ;
	if( !StreamAtEnd(input) ) t = nil ;
	StreamClose(input) ;
    return t ;
}

void TermReadVarTableRellocForStacks(Size globalOffset, Size localOffset)
{
	DoTermReadVarTableRellocForStacks(globalOffset, localOffset) ;
}


/* CXPROLOG C'BUILTINS */

static void PReadTerm()
{
	Pt t = ZReadTerm(currIn) ; /* stacks may grow */
	if( t == eofPt ) t = tEofAtom ;
	MustBe( t != nil && Unify(X0, t) )
}

static void PSReadTerm()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	Pt t = ZReadTerm(srm) ; /* stacks may grow */
	if( t == eofPt ) t = tEofAtom ;
	MustBe( t != nil && Unify(X1, t) )
}

static void PVarNames()
{
	ZEnsureFreeSpaceOnStacks(5 * NVars(), "varnames/1") ; /* stacks may grow */
	MustBe( Unify(X0, MakeVarList()) )
}

static void PReadTokens()
{
	Pt t = ZDoReadTokens(currIn) ; /* stacks may grow */
	if( t == eofPt ) t = tEofAtom ;
	MustBe( t != nil && Unify(X0, t) )
}

static void PSReadTokens()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	Pt t = ZDoReadTokens(srm) ; /* stacks may grow */
	if( t == eofPt ) t = tEofAtom ;
	MustBe( t != nil && Unify(X1, t) )
}

void TermReadInit()
{
	VarsInit() ;
	TokensInit() ;
	InstallAtomGCHandler(TermReadAtomGCMark) ;

	InstallCBuiltinPred("read", 1, PReadTerm) ;
	InstallCBuiltinPred("read", 2, PSReadTerm) ;
	InstallCBuiltinPred("varnames", 1, PVarNames) ;

	InstallCBuiltinPred("read_tokens", 1, PReadTokens) ;
	InstallCBuiltinPred("read_tokens", 2, PSReadTokens) ;
}
