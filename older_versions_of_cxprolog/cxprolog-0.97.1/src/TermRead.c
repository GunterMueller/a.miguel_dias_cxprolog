/*
 *   This file is part of the CxProlog system

 *   TermRead.c
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


/* VARS */

#define varsInitialCapacity		256

static Hdl varsBegin, varsEnd, varsPt ;

static void VarsInit(void)
{
	varsBegin = Allocate(varsInitialCapacity, false) ;
	varsEnd = varsBegin + varsInitialCapacity ;
}

static Size NVars(void)
{
	return (varsEnd - varsBegin) / 2 ;
}

static void VarsExpand(void)
{
	Size oldCapacity = varsEnd - varsBegin ;
	Size newCapacity = oldCapacity * 2 ;
	Hdl newVarsBegin, newVarsEnd ;
	MemoryGrowInfo("vars", oldCapacity, newCapacity) ;
	newVarsBegin = Reallocate(varsBegin, oldCapacity, newCapacity) ;
	newVarsEnd = newVarsBegin + newCapacity ;
	varsPt += newVarsBegin - varsBegin ;
	varsBegin = newVarsBegin ;
	varsEnd = newVarsEnd ;
}

static void TermReadBasicGCMark(void)
{
	ExtraGCMarkRange(varsBegin, varsPt) ;
}

static void RelocateTermReadVarTable(Size globalOffset, Size localOffset)
{
	register Hdl h ;
	for( h = varsBegin ; h < varsPt ; h += 2 )
		h[1] += globalOffset ;
}

static void VarsReset(void)
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

static Pt MakeVarList(void)
{
	register Hdl h ;
	register Pt list ;
	list = tNilAtom ;
	for( h = varsPt - 2 ; h >= varsBegin ; h -= 2 )
		list = MakeList(
				MakeBinStruct(eqFunctor, h[0], h[1]),
				list) ;
	return list ;
}


/* TOKENS */

typedef enum
{
	spacesTk,	commentTk,				/* Must be first. Not used, for now. */
	nxTk,		varTk,		strTk,		atomTk,
	openRTk0,
	openRTk,	openSTk,	openCTk,
	closeRTk,	closeSTk,	closeCTk,
	commaTk,	barTk,		dotTk,		endMarkTk,
} TokenType ;

#define InvisibleTokenType(tk)	((tk) <= commentTk)

static Size nTokens ;
static TokenType tk ;
static Pt tkTerm ;
static Hdl tks ;
static FunctorPt spacesTkFunctor, commentTkFunctor ;
static FunctorPt constTkFunctor, varTkFunctor, strTkFunctor, atomTkFunctor ;
static Pt tDotTk, tOpenRTk0, tOpenRTk, tCloseRTk, tOpenSTk,
			tCloseSTk, tOpenCTk, tCloseCTk, tCommaCTk, tBarTk ;

static void TokensInit(void)
{
	spacesTkFunctor = LookupFunctorByName("spaces_tk", 1) ;
	commentTkFunctor = LookupFunctorByName("comment_tk", 1) ;
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

static void GenToken(TokenType tk, Pt term)
{
	ScratchPush(tk) ;
	ScratchPush(term) ;
	ScratchPush(BigStrOffset(bigStrAPt)) ; /* Save offset */
	nTokens++ ;
}

static void GenTokensStart(void)
{
	nTokens = 0 ;
	UseScratch() ;
}

static void GenTokensEnd(void)
{
	GenToken(endMarkTk, nil) ;
	FreeScratch() ;
}

static void ReadingTokensStart(void)
{
	tks = VUseScratch() ;
}

static void ReadingTokensEnd(void)
{
	FreeScratch() ;
}

static void NextToken(void)
{
	tk = ((TokenType)tks[0]) ;
	tkTerm = tks[1] ;
	tks += 3 ; /* indeed is a 3 */
}

static Size PreviousTokenOffset(void)
{
	return cPInt(tks[-1]) ;
}

static void PeekToken(void)
{
	tk = ((TokenType)tks[0]) ;
	tkTerm = tks[1] ;
}

#if COMPASS
static void PeekToken2(void)
{
	tk = ((TokenType)tks[3]) ;
	tkTerm = tks[4] ;
}

static Bool IsCloseToken(TokenType t)
{
	switch( t ) {
		case commaTk:	return true ;
		case closeRTk:	return true ;
		case closeSTk:	return true ;
		case closeCTk:	return true ;
		case dotTk:		return true ;
		case barTk:		return true ;
		default:		return false ;
	}
}
#endif

static Pt TokenAsTkTerm(void)
{
	switch( tk ) {
		case spacesTk:	return MakeUnStruct(spacesTkFunctor, tkTerm) ;
		case commentTk:	return MakeUnStruct(commentTkFunctor, tkTerm) ;
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

#if 0
static void ShowTokens(void)
{
	Mesg("TOKENS:") ;
	ReadingTokensStart() ;
	for(;;) {
		NextToken() ;
		if( tk == endMarkTk ) break ;
		if( tk == atomTk )
			Mesg("Token = %s %lx", XAtomName(tkTerm), tkTerm) ;
	}
	ReadingTokensEnd() ;
	MesgW("ENDTOKENS:") ;
}
#endif


/* ERROR MESSAGES */

static Bool readingFromString ;
static CharPt lexErrorMesg ;
static Size lexErrorOffset ;

static void EndLex(void) ;

static void ReaderReset(void)
{
	EndLex() ;
	ReadingTokensEnd() ;
	HRestore() ;
	VarsReset() ;
}
	
static void IssueErrorMesg(CharPt mesg, Size hereOffset)
{
	CharPt here = BigStrAddr(hereOffset) ;
	CharPt inText ;
	BigStrAddChar('\0') ;
	inText = ClearTermText(BigStrBegin(), false) ;
	ReaderReset() ;
	SyntaxError("%s.\n%s#HERE#%s\n", mesg,
				GStrMakeSegm(inText, here), here) ;
}

static void LexError(CharPt mesg)
{
	if( lexErrorMesg == nil ) {
		if( readingFromString ) {
			ReaderReset() ;
			EventForceFail() ;
		}
		else {
			lexErrorMesg = mesg ;
			lexErrorOffset = BigStrOffset(BigStrCurr()) ;
		}
	}
}

static void FinishLexErrors(void)
{
	if( lexErrorMesg != nil )
		IssueErrorMesg(lexErrorMesg, lexErrorOffset) ;
}

static void LexPrematureEOF(void)
{
	LexError("Premature end of file") ;
	FinishLexErrors() ;
}

static void ParserError(CharPt mesg) /* always called after NextToken() */
{
	if( readingFromString ) {
		ReaderReset() ;
		EventForceFail() ;
	}
	else	
		IssueErrorMesg(mesg, PreviousTokenOffset()) ;
}

/* SCANNER */

/* All the input text is stored in BigStr, and this is useful for:
    - collecting the text that makes up each atom, number, etc;
    - issuing error messages;
    - for implementing the read_with_source/3 predicate.
*/

static StreamPt input ;
static WChar cc ;
static Bool onlyTokens ;

#define GetChar()			(cc = StreamGet(input))
#define PeekChar()			(StreamPeek(input))

#define StoreChar(c)		(BigStrAddChar(c))
#define MarkNull()			(BigStrMarkNull())
#define StoreCurr()			(BigStrCurr())

static void InitLex(void)
{
	lexErrorMesg = nil ;
	BigStrOpen() ;
}

static void EndLex(void)
{
	BigStrClose() ;
}

static void NextChar(void)
{
	StoreChar(cc) ;
	GetChar() ;
}

static void NextCharEOF(void)
{
	if( cx_iseof(cc) )
		LexPrematureEOF() ;
	StoreChar(cc) ;
	GetChar() ;
}

static Bool CheckFullStop(Bool skip)
{
	WChar c = PeekChar() ;
	if( cx_isspace(c) ) {
		if( skip ) NextChar() ;
		return true ;
	}
	return c == '%' || cx_iseof(c) ;
}

static CharPt ReadId(void)
{
	bigStrBPt = StoreCurr() ;
	do {
		NextChar() ;
	} while( cx_isalnum(cc) ) ;
	MarkNull() ;
	return bigStrBPt ;
}

static CharPt ReadDigits(void)
{
	bigStrBPt = StoreCurr() ;
	do {
		NextChar() ;
	} while( cx_isdigit(cc) ) ;
	MarkNull() ;
	return bigStrBPt ;
}

static PInt ReadInt(int base)
{
	register PInt n = 0 ;
	register int d ;
	for(;;) {
		if( cx_isdigit(cc) )
			d = cc - '0' ;
		elif( InRange(cc, 'a', 'z') )
			d = 10 + cc - 'a' ;
		elif( InRange(cc, 'A', 'Z') )
			d = 10 + cc - 'A' ;
		else
			break ;
		if( d >= base )
			break ;
		n = n * base + d ;
		NextChar() ;
	}
	return n ;
}
	
static PInt ReadOctalInt(void)
{
	register PInt n = 0 ;
	register int d ;
	for(;;) {
		if( InRange(cc, '0', '7') )
			d = cc - '0' ;
		else
			break ;
		if( n > (maxInt >> 3) ) {
			LexError("Too long an octal escape sequence") ;
			n = '!' ;
		}
		else
			n = (n << 3) + d ;
		NextChar() ;
	}
	return n ;
}
	
static PInt ReadHexaInt(void)
{
	register PInt n = 0 ;
	register int d ;
	for(;;) {
		if( cx_isdigit(cc) )
			d = cc - '0' ;
		elif( InRange(cc, 'a', 'f') )
			d = 10 + cc - 'a' ;
		elif( InRange(cc, 'A', 'F') )
			d = 10 + cc - 'A' ;
		else
			break ;
		if( n > (maxInt >> 4) ) {
			LexError("Too long a hexadecimal escape sequence") ;
			n = '!' ;
		}
		else
			n = (n << 4) + d ;
		NextChar() ;
	}
	return n ;
}

static WChar ReadEscapeSequence(void)
{
	switch( cc ) {
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7': {
			PInt n = ReadOctalInt() ;
			if( cc != '\\' )
				LexError("Invalid octal escape sequence") ;
			return CharFixCode(n) ;
		}
		case 'a': return '\a' ;
		case 'b': return '\b' ;
		case 'f': return '\f' ;
		case 'n': return '\n' ;
		case 'r': return '\r' ;
		case 't': return '\t' ;
		case 'v': return '\v' ;
		case 'x': case 'X': {
			PInt n ;
			NextChar() ;	/* Skip 'x', 'X' */
			n = ReadHexaInt() ;
			if( cc != '\\' )
				LexError("Invalid hexadecimal escape sequence") ;
			return CharFixCode(n) ;
		}
		default: return cc ;					
	}
}

static Pt ReadQuotedAtom(void)
{
	WChar quote = cc ;
	NextChar() ;  /* skip quote */
	BigStr2Open() ;
	for(;;) {
		if( cc == quote ) {
			NextChar() ;  /* skip quote */
			if( cc == quote )  /* another quote? */
				BigStr2AddChar(cc) ;
			else
				break ;
		}
		elif( cc == '\\' ) { /* Escape sequence */
			NextChar() ;  /* skip '\\' */
			BigStr2AddChar(ReadEscapeSequence()) ;
		}
		else
			BigStr2AddChar(cc) ;
		NextCharEOF() ;
	}
	return MakeTempAtom(BigStr2Close()) ;
}

static Pt ReadIdAtom(void)
{
	do {
		NextChar() ;
	} while( cx_isalnum(cc) ) ;
	MarkNull() ;
	return MakeTempAtom(bigStrAPt) ;
}

static Pt ReadSymbolsAtom(void)
{
	do {
		NextChar() ;
	} while( cx_issymbol(cc) ) ;
	MarkNull() ;
	return MakeTempAtom(bigStrAPt) ;
}

static Pt ReadSoloAtom(void)
{
	NextChar() ;
	MarkNull() ;
	return MakeTempAtom(bigStrAPt) ;
}

static Pt ReadNXAtomic(int base)
{
	if( base > 36 )
		LexError("Too big a base in base'xxx. Max is 36.") ;
	switch( base ) {
		case 0: {
			WChar c = cc ;
			NextCharEOF() ;
			if( cx_isalnum(cc) )
				LexError("Bad ending in 0'x literal") ;
			else
				return MakeInt(c) ;
			break ;
		}
		case 1: {
			Pt t ;
			if( (t = MakeExtraFromStr(ReadId())) == nil )
				LexError("Invalid 1'xxx_yyy literal") ;
			elif( t == tFalseAtom )
				LexError("1'xxx_yyy object does not exist") ;
			else
				return t ;
			break ;
		}
		default: {
			return MakeInt(ReadInt(base)) ;
			break ;
		}
	}
	return minusOneIntPt ;
}

static Pt ReadNumber(void)
{
	CharPt s ;
	Pt res ;
	Bool error = false ;
	if( strlen(bigStrAPt) > 256 ) {
		LexError("Number too long") ;
		error = true ;
	}
	if( cc == '.' && !CheckFullStop(false) ) {
		NextChar() ;
		if( cx_isdigit(cc) ) {
			s = ReadDigits() ;
			if( strlen(s) > 256 ) {
				LexError("Float too long") ;
				error = true ;
			}
		}
		else {
			LexError("Missing decimal part of float") ;
			error = true ;
		}
	}
	if( cc == 'e' || cc == 'E' ) {
		NextChar() ;
		if( cc == '+' )
			NextChar() ;
		else if( cc == '-' )
			NextChar() ;
		if( cx_isdigit(cc) ) {
			s = ReadDigits() ;
			if( strlen(s) > 5 ) {
				LexError("Exponent too long") ;
				error = true ;
			}
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
		return nanFloatPt ;

	if( (res = NumberFromStr(bigStrAPt)) == nil ) {
		LexError("Malformed float") ;
		return nanFloatPt ;
	}
	else return res ;
}

static void SkipComment(void)
{
	NextChar() ;		/* skip '/', get '*' */
	while( cc != '/' ) {	/* start with a '*' */
		do { NextCharEOF() ; } while( cc != '*' ) ;
		do { NextChar() ; } while( cc == '*' ) ;
	}
	NextChar() ;	/* skip '/' */
}

static void SkipLineComment(void)
{
	do { NextCharEOF() ; } while( cc != '\n' ) ;
	/* Doesn't skip NL so that read_tokens/1 works well */
}

static Size GenerateTokens(void)
{
	InitLex() ;
	GenTokensStart() ;
	GetChar() ;
	for(;;) {
		bigStrAPt = StoreCurr() ;
		switch( CharType(cc) ) {
			case _BL: {
				if( onlyTokens )
					do {
						if( cc == '\n' ) goto exitL ;
						NextChar() ;
					} while( cx_isspace(cc) ) ;
				else
					do {
						NextChar() ;
					} while( cx_isspace(cc) ) ;

afterSpaceL:	if( cc == '(' ) {
					bigStrAPt = StoreCurr() ;
					GenToken(openRTk, nil) ;
					NextChar() ;
				}
				break ;
			}
			case _LC: GenToken(atomTk, ReadIdAtom()) ; break ;
			case _UC: GenToken(varTk, ReadIdAtom()) ; break ;
			case _SO: GenToken(atomTk, ReadSoloAtom()) ; break ;

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

			case '%':  SkipLineComment() ; break ;
			case _SY: {	/* '.' and '/' are symbols */
				if( cc == '.' && CheckFullStop(true) )
					{ GenToken(dotTk, nil) ; goto exitL ; }
				elif( cc == '/' && PeekChar() == '*' )
					{ SkipComment() ; goto afterSpaceL ; }
				else
					GenToken(atomTk, ReadSymbolsAtom()) ;
				break ;
			}

			case _DG: {
				CharPt s = ReadDigits() ;
				if( cc == '\'' || cc == '\"' ) {
					int base = strlen(s) < 4 ? atoi(s) : 999 ;
					NextChar() ; /* Skip quote */
					GenToken(nxTk, ReadNXAtomic(base)) ;
				}
				else
					GenToken(nxTk, ReadNumber()) ;
				break ;
			}

			case '\'': GenToken(atomTk, ReadQuotedAtom()) ; break ;
			case '\"':{
				Pt t = ReadQuotedAtom() ;
				if( compatibleStrings_flag )
					nTokens += CharLen(XAtomName(t)) ;
				GenToken(strTk, t) ;
				break ;
			}

            case _EF: {
				if( nTokens == 0 )  /* will inject 'end_of_file'. */
					return -1 ;
				elif( onlyTokens ) ;
				elif( readingFromString )
					GenToken(dotTk, nil) ;
				else
					LexPrematureEOF() ;
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
	EndLex() ;
	FinishLexErrors() ;
	return nTokens ;
}

CharPt ClearTermText(register CharPt s, Bool clearDot)
{
	CharPt beg ;
/* Skip blanks at begin */
	for( ; BasicCharType(*cUCharPt(s)) == _BL ; s++ )
		;
	beg = s ;
/* Delete blanks at end */
	for( s = beg + strlen(beg) - 1
		; beg <= s && BasicCharType(*cUCharPt(s)) == _BL
		; s-- ) ;
	if( clearDot && *s == '.' ) 
		*s = '\0' ;
	else
		s[1] = '\0' ;
	return beg ;	
}

static CharPt TermInput(void)
{
	return ClearTermText(BigStrBegin(), false) ;
}
	

/* THE PARSER */

static Pt Term(int n) ;

static Pt Args(AtomPt atom)
{
	int arity = 0 ;
	NextToken() ;	/* skip ( */
	do {
		ScratchPush(Term(subPrec)) ;
		if( ++arity > maxFunctorArity )
			FileError("Highest arity (%d) exceeded on functor '%s/%d'",
								maxFunctorArity, AtomName(atom), arity) ;
		NextToken() ;
	} while( tk == commaTk ) ;
	if( tk != closeRTk ) ParserError("expected ')'") ;
	return MakeStruct(LookupFunctor(atom, arity), ScratchBack(arity)) ;
}

static Pt RoundBrackets(void)
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

static Pt SquareBrackets(void)
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

static Pt CurlyBrackets(void)
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

#if COMPASS
static Pt MinusOp(AtomPt atom)
{
	if( atom == XAtom(tMinusAtom) && tk == nxTk && IsNumber(tkTerm) ) {
		Pt t = tkTerm ;
		NextToken() ;
		return IsInt(t) ? MakeInt(-XInt(t)) : MakeFloat(-XFloat(t)) ;
	}
	else return nil ;
}
#endif

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
	args		--> subterm(999), commaTk, args.
				--> subterm(999).
	term(0)		--> atomTk, openRTk0, args, cTermloseRTk.
				--> openRTk, subterm(1200), closeRTk.
				--> openCTk, subterm(1200), closeCTk.
				--> list | atomTk | strTk | nxTk | varTk.
*/

#if COMPASS
static Pt Term(int n)
{
	register Pt res ;
	AtomPt atom = nil ;  /* avoids warning */
	int m = 0,
		p = 0, i,	/* priority of current operator */ /* = 0 avoids warning */
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
					if( IsCloseToken(tk) ) goto simpleAtom ; /* new */
					else {
						NextToken() ;
						ParserError("Incompatible precedences (1)") ;
					}
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
						if( Infix(XAtom(tkTerm), &lp, &rp) ) {
							PeekToken2() ;
							if( IsCloseToken(tk) ) goto prefixL ;
							else goto simpleAtom ;
						}
						elif( Postfix(XAtom(tkTerm), &lp) ) {
							PeekToken2() ;
							if( IsCloseToken(tk) ) goto prefixL ;
							else goto simpleAtom ;
						}
						else goto prefixL ;
					}
					case commaTk:	goto simpleAtom ;
					case closeRTk:	goto simpleAtom ;
					case closeSTk:	goto simpleAtom ;
					case closeCTk:	goto simpleAtom ;
					case dotTk:		goto simpleAtom ;
					case barTk:		goto simpleAtom ;
					default: break ;
				}
				InternalError("Term (1)") ;
			}
			else goto continueL ;
		}
		default: break ;
	}
	InternalError("Term (2)") ;

simpleAtom:
	res = TagAtom(atom) ;
	/* fall through */

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
				res = MakeBinStruct(barIsSemicolon_flag ? semicolonFunctor : barFunctor,
														res, Term(m)) ;
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
							int x, rx ;
							AtomPt atx = XAtom(tkTerm) ;
							if( AtomToOperator(atx) == nil )
								goto infixL ;
							else {
								PeekToken2() ;
								if( IsCloseToken(tk) ) goto infixL ;
							}							
#if 1
						/* tries infix-prefix  */
							if( (x = Prefix(atx, &rx)) != 0 && ri >= x )
								goto infixL ;
							else goto postfixL ;
#else
						/* tries posfix-posfix  */
							if( (x=Postfix(atx, &lx)) != 0 && lp <= x )
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
#else

static Pt Term(int termPrec)
{
	Pt res ;
	AtomPt atom ;
	int leftPrec = 0 ;	/* current left precedence */
	int p, i,			/* precedence of current operator */
		lp,	li,			/* precedence of the left argument */
		rp, ri ;		/* precedence of the right argument */
	int q, lq, rq ;		/* auxiliary */
		
	NextToken() ;
	switch( tk ) {
		case openRTk0:	res = RoundBrackets() ; goto continueL ;
		case openRTk:	res = RoundBrackets() ; goto continueL ;
		case openSTk:	res = SquareBrackets() ; goto continueL ;
		case openCTk:	res = CurlyBrackets() ; goto continueL ;

		case closeRTk:	ParserError("invalid ')'") ;
		case closeSTk:	ParserError("invalid ']'") ;
		case closeCTk:	ParserError("invalid '}'") ;
		case commaTk:	res = MakeAtom(",") ; goto continueL ; /* simple atom */
		case barTk:		res = MakeAtom("|") ; goto continueL ; /* simple atom */
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
/* Decide if the first token, an atom here, is a prefix operator or a simple atom */
			elif( (p = Prefix(atom, &rp)) != 0 && p <= termPrec ) {
				switch( tk ) {
					case nxTk: {
						if( atom == XAtom(tMinusAtom) && IsNumber(tkTerm) ) {
							res = IsInt(tkTerm)
									? MakeInt(-XInt(tkTerm))
									: MakeFloat(-XFloat(tkTerm)) ;
							NextToken() ;
							goto continueL ;							
						}
						else goto prefixL ;
					}
					case atomTk: {
						if( (q = Prefix(XAtom(tkTerm), &rq)) != 0 && q <= rp )
							goto prefixL ;
						elif( (q = Infix(XAtom(tkTerm), &lq, &rq)) != 0 && q <= termPrec )
							goto continueL ; /* simple atom */
						else goto prefixL ;
					}
					case varTk:		goto prefixL ;
					case strTk:		goto prefixL ;
					case openRTk0:	goto prefixL ;
					case openRTk:	goto prefixL ;
					case openSTk:	goto prefixL ;
					case openCTk:	goto prefixL ;
					case commaTk:	goto continueL ; /* simple atom */
					case closeRTk:	goto continueL ; /* simple atom */
					case closeSTk:	goto continueL ; /* simple atom */
					case closeCTk:	goto continueL ; /* simple atom */
					case dotTk:		goto continueL ; /* simple atom */
					case barTk:		goto continueL ; /* simple atom */
					default: break ;
				}
				return InternalError("Term (1)") ;
			}
			else goto continueL ; /* simple atom */
		}
		default: break ;
	}
	return InternalError("Term (2)") ;

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
			if( leftPrec < commaPrec && commaPrec <= termPrec ) {
				NextToken() ;
				res = MakeBinStruct(commaFunctor, res, Term(commaPrec)) ;
				leftPrec = commaPrec ;
				goto continueL ;
			}
			else return res ; /* doesn't consume token */
		}
		case barTk: {
			if(  leftPrec < barPrec && barPrec <= termPrec ) {
				NextToken() ;
				res = MakeBinStruct(barIsSemicolon_flag ? semicolonFunctor : barFunctor,
														res, Term(barPrec)) ;
				leftPrec = barPrec ;
				goto continueL ;
			}
			else return res ; /* doesn't consume token */
		}
		case atomTk: {
			atom = XAtom(tkTerm) ;
			if( (p = Postfix(atom, &lp)) != 0 && leftPrec <= lp && p <= termPrec ) {
				NextToken() ;
				if( (i = Infix(atom, &li, &ri)) != 0 && leftPrec <= li && i <= termPrec ) {
					/* second token postfix and infix - look ahead to decide ... */
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
							AtomPt atx = XAtom(tkTerm) ;
							if( AtomToOperator(atx) == nil )
								goto infixL ;	/* same as varTk */							
#if 1
						/* tries infix-prefix  */
							if( (q = Prefix(atx, &rq)) != 0 && q <= ri )
								goto infixL ;
							else goto postfixL ;
#else
						/* tries posfix-posfix  */
							if( (q = Postfix(atx, &lq)) != 0 && p <= lq && q <= termPrec )
								goto postfixL ;
						/* tries posfix-infix  */
							elif( ( q = Infix(atx, &lq, &rq)) != 0 && p <= lq && q <= termPrec )
								goto postfixL ;
						/* selects infix-prefix */
							else
								goto infixL ;
#endif
					}
						default: break ;
					}
					return InternalError("Term (3)") ;
				}
				else goto postfixL ;
			}
			elif( (i = Infix(atom, &li, &ri)) != 0 && leftPrec <= li && i <= termPrec ) {
				NextToken() ;
				goto infixL ;
			}
			else return res ; /* doesn't consume token */
		}
		default: break ;
	}
	return InternalError("Term (4)") ;

prefixL:
	res = MakeUnStruct(LookupFunctor(atom, 1), Term(rp)) ;
	leftPrec = p ;
	goto continueL ;
infixL:
	p = i ; lp = li ; rp = ri ;
	res = MakeBinStruct(LookupFunctor(atom, 2), res, Term(rp)) ;
	leftPrec = i ;
	goto continueL ;
postfixL:
	res = MakeUnStruct(LookupFunctor(atom, 1), res) ;
	leftPrec = p ;
	goto continueL ;
}
#endif

static Pt ZDoReadTokens(StreamPt srm)
{
	Pt list = tNilAtom ;
	Hdl h = &list + 1 ;
	Size tokens, expectedSize ;
	input = srm ;
	HSave() ;
	onlyTokens = true ;

	if( (tokens = GenerateTokens()) == -1 ) { /* Means EOF */
		ReaderReset() ;
		return tEofAtom ;
	}
	expectedSize = 6 * tokens ;
	ZEnsureFreeSpaceOnStacks(expectedSize) ;

	ReadingTokensStart() ;
	for(;;) {
		NextToken() ;
		if( tk == endMarkTk ) break ;
		h[-1] = MakeList(TokenAsTkTerm(), tNilAtom) ;
		h = H ;
	} ;
	if( expectedSize < HGrown() )
		Warning("expectedSize = %ld, grown = %ld", expectedSize, HGrown()) ;
	ReadingTokensEnd() ;
	return list ;
}

static Pt ZDoReadTerm(StreamPt srm, Bool fromString)
{
	Pt res ;
	Size tokens, expectedSize ;
	input = srm ;
	HSave() ;
	onlyTokens = false ;
	readingFromString = fromString ;

	if( (tokens = GenerateTokens()) == -1 ) { /* Means EOF */
		ReaderReset() ;
		return tEofAtom ;
	}
	expectedSize = 2 * tokens ;
	ZEnsureFreeSpaceOnStacks(expectedSize) ;
	
	VarsReset() ;
/* Will write over the tokens, but no problem... */
	ReadingTokensStart() ;
	res = Term(maxPrec) ;
	NextToken() ;
	if( tk != dotTk ) ParserError("Malformed term") ;
	if( expectedSize < HGrown() )
		Warning("expectedSize = %ld, grown = %ld", expectedSize, HGrown()) ;
	ReadingTokensEnd() ;
	return res ;
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
	StreamClose(input, nil) ;
    return t ;
}

Pt ZTermFromList(Pt list)
{
	StreamPt input = ListStreamOpen(list) ;
    Pt t = ZDoReadTerm(input, true) ;
	StreamClose(input, nil) ;
    return t ;
}


/* CXPROLOG C'BUILTINS */

static void PReadTerm()
{
	Pt t = ZReadTerm(currIn) ; /* stacks may grow */
	MustBe( t != nil && Unify(X0, t) ) ;
}

static void PSReadTerm()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	Pt t = ZReadTerm(srm) ; /* stacks may grow */
	MustBe( t != nil && Unify(X1, t) ) ;
}

static void PSReadTermWithSource()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	Pt t = ZReadTerm(srm) ; /* stacks may grow */
	MustBe( t != nil && Unify(X1, t)
			&& UnifyWithAtomic(X2, MakeTempAtom(TermInput())) ) ;
}

static void PReadTermTopLevel()
{
	Pt t ;
	InterLineBeginTopPrompt(XTestAtomName(X0), XTestAtomName(X1)) ;
	t = ZReadTerm(userIn) ; /* stacks may grow */
	InterLineEndTopPrompt() ;
	MustBe( t != nil && Unify(X2, t) ) ;
}

static void PVarNames()
{
	ZEnsureFreeSpaceOnStacks(5 * NVars()) ; /* stacks may grow */
	MustBe( Unify(X0, MakeVarList()) ) ;
}

static void PReadTokens()
{
	Pt t = ZDoReadTokens(currIn) ; /* stacks may grow */
	MustBe( t != nil && Unify(X0, t) ) ;
}

static void PSReadTokens()
{
	StreamPt srm = XTestStream(X0, mRead) ;
	Pt t = ZDoReadTokens(srm) ; /* stacks may grow */
	MustBe( t != nil && Unify(X1, t) ) ;
}


/* INIT */

void TermReadInit()
{
	InterLineRestart() ;
	VarsInit() ;
	TokensInit() ;
	ExtraGCHandlerInstall("TERMREAD", TermReadBasicGCMark) ;
	InstallRelocateStacksHandler(RelocateTermReadVarTable) ;

	InstallCBuiltinPred("read", 1, PReadTerm) ;
	InstallCBuiltinPred("read", 2, PSReadTerm) ;
	InstallCBuiltinPred("read_with_source", 3, PSReadTermWithSource) ;
	InstallCBuiltinPred("$$_top_read", 3, PReadTermTopLevel) ;
	InstallCBuiltinPred("varnames", 1, PVarNames) ;

	InstallCBuiltinPred("read_tokens", 1, PReadTokens) ;
	InstallCBuiltinPred("read_tokens", 2, PSReadTokens) ;
}
