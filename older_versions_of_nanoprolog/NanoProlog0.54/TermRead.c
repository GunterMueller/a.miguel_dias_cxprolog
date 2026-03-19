/*
 *   This file is part of the NanoProlog system

 *   TermRead.c
 *   by A.Miguel Dias - 92/2/23
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931120: longer digit strings in ints and reals in the Mac 
 931120: chars '^' and #' are now handled properly
 931120: better handling of comments (which can now be very long)
 931117: release of version 0.5
 931117: release of version 0.5

*/

#include "NanoProlog.h"
#include <ctype.h>
#include <math.h>


/* VAR DICTIONARY */

static Pt LookupVar(char *id)
{
	register VarDescriptor *vd ;
	register AtomPt name ;

	if( id[0] == '_' && id[1] == '\0' )
		return( MakeVar() ) ;

	name = LookupAtom(id) ;
	dotable(vd, varDic.vars, varDic.nVars)
		if( vd->name == name ) return( vd->var ) ;
	if( varDic.nVars++ >= maxVarsPerTerm )
		Error("Too many variables in term") ;
	vd->var = MakeVar() ;
	vd->name = name ;
	return( vd->var ) ;
}

static Pt MakeVarList()
{
	register VarDescriptor *vd ;
	Pt h, t ;

	if( varDic.nVars == 0 )
		return( tNilAtom ) ;

	t = tNilAtom ;
	dotable(vd, varDic.vars, varDic.nVars)
		t = MakeList(
				MakeBinStruct(LookupFunctor2("=", 2), TagAtom(vd->name), vd->var),
				t) ;
	return( t ) ;
}


/* SCANNER */

typedef enum
{
	vnsTk,		atomTk,
	openRTk,	openSTk,	openCTk,
	closeRTk,	closeSTk,	closeCTk,
	commaTk,	barTk,
	dotTk
} TokenType ;

#define tkAtom				((AtomPt)(tkVal))
#define tkTerm				((Pt)(tkVal))

#define eofChar				26
#define maxText				512
#define maxInText			2048
static int cc ;
static TokenType tk ;
static void *tkVal ;
static int nTokens, tkPreBlanks ;
static jmp_buf readJmp ;
static char inText[maxInText], *inPt, *startPt, *peekStartPt ;

Bool IsFullStopChar(int c)
{
	return( isspace(c) || c == '%' || c == eofChar ) ;
}

static int NextChar()
{
	*inPt++ = cc = Get0() ;
}

static void ReaderError(CharPt s)
{
	int n, m ;
	char *ch ;

/*	if( s != nil ) startPt = inPt - 1 ;*/
	if( tk != dotTk )
		for(;;)
		{
			while( cc != '.' && cc != eofChar ) NextChar() ;
			if( cc == eofChar ) break ;
			if( IsFullStopChar(Peek0()) ) break ;
			NextChar() ;
		}
	if( cc == eofChar ) inPt-- ;

	if( s == nil ) printf("*** READER ERROR ***\n") ;
	else printf("*** READER ERROR (%s) ***\n", s) ;

	for( ch = inText ; ch < startPt && *ch == '\n' ; ch++ ) ;
	for( n = 0 ; ch < startPt ; ch++ )
	{
		n++ ;
		if( *ch == '\n' ) n = 0 ;
		Put(*ch) ;
	}
	for( ; ch < inPt && *ch != '\n' ; ch++ ) Put(*ch) ;
	Nl() ;
	for( m = 0 ; m < n ; m++ ) Put('-') ;
	PutString(n < 75 ? "^---" : "^") ;
	for(  ; ch < inPt ; ch++ ) Put(*ch) ;
	Nl() ;
	longjmp(readJmp, 1) ;
}

static int NextCharEOF()
{
	if( cc == eofChar ) ReaderError("Premature end of file") ;
	*inPt++ = cc = Get0() ;
}

static void ReadId(CharPt st)
{
	register CharPt s = st, send = s + maxText - 3 ;

	do
	{
		if( s >= send ) ReaderError("Atom or var name too long") ;
		*s++ = cc ;
		NextChar() ;
	} while( isalnum(cc) || cc == '_' ) ;
	*s = '\0' ;
}

Bool IsSymbol(int c)
{
	switch( c )
	{
		case '$': case '&': case '*': case '+': case '-':
		case ':': case '<': case '=': case '>': case '?':
		case '@': case '\\': case '^': case '`': case '~':
		case '.': case '/': 
			return( true ) ;
		default:
			return( false ) ;
	}
}

static void ReadSymbol(CharPt st)
{
	register CharPt s = st, send = s + maxText - 3 ;

	do
	{
		if( s >= send ) ReaderError("Symbol too long") ;
		*s++ = cc ;
		NextChar() ;
	} while( IsSymbol(cc) ) ;
	*s = '\0' ;
}

static void ReadQuoted(CharPt st)
{
	int quote = cc ;
	register CharPt s = st, send = s + maxText - 3 ;

	NextChar() ;	/* Skip quote */
	for(;;)
	{
		for( ; cc != quote ; NextCharEOF() )
		{
			if( s >= send ) ReaderError("Quoted text too long") ;
			else *s++ = cc ;
		}
		NextChar() ;
		if( cc == quote )	/* A quote again */
		{
			*s++ = quote ;
			NextChar() ;
		}
		else break ;
	}
	*s = '\0' ;
}

static Int ReadInt10()
{
	register Int i = 0 ;

	do
	{
		i = i * 10 + cc - '0' ;
		NextChar() ;
	}
	while( isdigit(cc) ) ;
	return( i ) ;
}

static Int ReadInt(Int base, int *nd)
{
	register Int i = 0 ;
	register int n ;
	int dummy ;

	if( nd == nil ) nd = &dummy ;
	*nd = 0 ;
	for(;;)
	{
		if( isdigit(cc) ) n = cc - '0' ;
		elif( 'a' <= cc && cc <= 'z' ) n = 10 + cc - 'a' ;
		else break ;
		if( n >= base ) break ;
		(*nd)++ ;
		i = i * base + n ;
		NextChar() ;
	}
	if( *nd == 0 ) ReaderError("Missig digits") ;
	return( i ) ;
}

static void NextToken()
{
	char s[maxText] ;
	int n ;

	if( peekStartPt ) { startPt = peekStartPt ; peekStartPt = nil ; return ; }
redoL:
	tkPreBlanks = 0 ;
	while( isspace(cc) ) { NextChar() ; tkPreBlanks++ ; }
	startPt = inPt - 1 ;
	switch( cc )
	{
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
		case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
		case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
		case 'v': case 'w': case 'x': case 'y': case 'z':
		{
			ReadId(s) ;
atomL:		tk = atomTk ;
			tkVal = LookupAtom(s) ;
			break ;
		}
		case '$': case '&': case '*': case '+': case '-':
		case ':': case '<': case '=': case '>': case '?':
		case '@': case '\\': case '^': case '`': case '~':
		{
symbolL:	ReadSymbol(s) ;
			goto atomL ;
			break ;
		}
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
		case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
		case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
		case 'V': case 'W': case 'X': case 'Y': case 'Z': case '_':
		{
			ReadId(s) ;
			tk = vnsTk ;
			tkVal = LookupVar(s) ;
			break ;
		}
		case '0': case '1':case '2':case '3':case '4':
		case '5':case '6':case '7':case '8':case '9':
		{
			Int i, d, e ;
			int nd, se ;

			i = ReadInt10() ;	
			if( cc == '\'' || cc == '\"' )
			{
				if( i > 36 ) ReaderError("Base too big in base'xxx") ;
				NextChar() ; /* Skip quote */
				if( i == 0 )
				{
					i = cc ;
					NextCharEOF() ;
					if( isalnum(cc) || cc == '_' )
						ReaderError("Too many chars in 0'x constant") ;
				}
				else i = ReadInt(i, nil) ;
				goto intL ;
			}
			if( cc != '.' && cc != 'e' && cc != 'E' )
			{
intL:			tk = vnsTk ;
				tkVal = MakeInt(i) ;
				break ;
			}
			d = nd = 0 ;
			if( cc == '.' )
			{
				n = Peek0() ;
				if( IsFullStopChar(n) ) goto intL ;
				elif( isdigit(n) )
				{
					NextChar() ;	/* Skip '.' */
					d = ReadInt(10, &nd) ;
				}
			}
			se = 1 ; e = 0 ;
			if( cc == 'e' || cc == 'E' )
			{
				NextChar() ;
				if( cc == '+' ) NextChar() ;
				else if( cc == '-' ) { se = -1 ; NextChar() ; }
				if( not isdigit(cc) ) ReaderError("Malformed exponent of real") ;
				else e = ReadInt10() ;
			}
realL:		tk = vnsTk ;
			tkVal = MakeReal((i + d * pow(10.0, -(double)nd))
							* pow(10.0, (double)se*e)) ;
			break ;
		}
		case '(': tk = openRTk ; NextChar() ; break ;
		case ')': tk = closeRTk ; NextChar() ; break ;
		case '[': tk = openSTk ; NextChar() ; break ;
		case ']': tk = closeSTk ; NextChar() ; break ;
		case '{': tk = openCTk ; NextChar() ; break ;
		case '}': tk = closeCTk ; NextChar() ; break ;
		case '|': tk = barTk ; NextChar() ; break ;
		case ',': tk = commaTk ; NextChar() ; break ;
		case ';': s[0] = ';' ; s[1] = '\0' ; NextChar() ; goto atomL ;
		case '!': s[0] = '!' ; s[1] = '\0' ; NextChar() ; goto atomL ;
		case '#': s[0] = '#' ; s[1] = '\0' ; NextChar() ; goto atomL ;
		case '\'': ReadQuoted(s) ; goto atomL ; break ;
		case '\"': ReadQuoted(s) ; tk = vnsTk ; tkVal = BufferToString(s) ; break ;
		case '.':
		{
			n = Peek0() ;
			if( IsFullStopChar(n) ) { tk = dotTk ; NextChar() ; }
			else goto symbolL ;
			break ;
		}
		case '%':
		{
			do { NextChar() ; inPt-- ; } while( cc != '\n' && cc != eofChar ) ;
			*inPt++ = '.' ; *inPt++ = '.' ; *inPt++ = '.' ; *inPt++ = '\n' ;
			if( cc == eofChar ) goto eofL ; else goto redoL ;
			break ;
		}
		case '/':
		{
			if( Peek0() != '*' ) goto symbolL ;
			NextChar() ;		/* skip / */
			while( cc != '/' )	/* start with a '*' */
			{
				do { NextCharEOF() ; inPt-- ; } while( cc != '*' ) ;
				do { NextChar() ; inPt-- ; } while( cc == '*' ) ;
			}
			*inPt++ = '*' ;
			*inPt++ = '/' ;
			NextChar() ;	/* skip / */
			goto redoL ;
			break ;
		}

		case eofChar:
		{
eofL:		if( nTokens == 0 ) longjmp(readJmp, 2) ;
			else NextCharEOF() ;	/* Generate an eof error */
			break ;
		}

		default:
		{
			Mesg("Ascii code: <%d>", cc) ;
			ReaderError("Invalid character") ;
			break ;
		}
	}
	nTokens++ ;
}

static void PeekToken()
{
	CharPt save ;

	if( peekStartPt == nil )
	{
		save = startPt ;
		NextToken() ;
		peekStartPt = startPt ;
		startPt = save ;
	}
}

void InitLex()
{
	ResetVarDic() ;
	cc = ' ' ;
	nTokens = 0 ;
	tk = barTk ;
	startPt = inPt = inText ;
	peekStartPt = nil ;
}


/* TEST */

static void PrintToken()
{
	switch( tk )
	{
		case atomTk:	printf("atom(%s) ", AtomName(tkAtom)) ; break ;
		case vnsTk:		printf("vns(") ;
						WriteTerm(tkTerm) ;
						printf(") ") ;
						break ;
		case dotTk:		printf(".\n") ; break ;
		case openRTk:	printf("( ") ; break ;
		case closeRTk:	printf(") ") ; break ;
		case openSTk:	printf("[ ") ; break ;
		case closeSTk:	printf("] ") ; break ;
		case openCTk:	printf("{ ") ; break ;
		case closeCTk:	printf("} ") ; break ;
		case barTk:		printf("| ") ; break ;
		default:		printf("OTHER ") ; break ;
	}
}


/* PARSER

 Recursive descendent with the most frequent cases involving
 operators handled directly. Locally ambiguous cases would require
 backtracking.

 The grammar:

	term		--> subterm(1200), dotTk.
	subterm(N)	--> term(MN), { M <= N }.
	term(N)		--> 				op(N,fx).
				--> 				op(N,fy).
				--> 				op(N,fx),	subterm(N-1).
				--> 				op(N,fy),	subterm(N).
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
	do
	{
		Push(readStack, Term(subPrec)) ;
		NextToken() ;
	}
	while( tk == commaTk ) ;
	if( tk != closeRTk ) ReaderError("')' expected") ;
	arity = readStack - save ;
	readStack = save ;
	return( MakeStruct(LookupFunctor(atom, arity), save) ) ;
}

static Pt RoundBrackets()
{
	Pt t ;

	t = Term(maxPrec) ;
	NextToken() ;
	if( tk != closeRTk ) ReaderError("')' expected") ;
	return( t ) ;
}

static Pt SquareBrackets()
{
	Hdl save = readStack ;
	int n ;

	PeekToken() ;	/* peek after [ */
	if( tk == closeSTk )
	{
		NextToken() ;	/* skip ] */
		return( tNilAtom ) ;
	}
	Push(readStack, Term(subPrec)) ;
	NextToken() ;
	while( tk == commaTk )
	{
		Push(readStack, Term(subPrec)) ;
		NextToken() ;
	}	
	if( tk == barTk )
	{
		Push(readStack, Term(subPrec)) ;
		NextToken() ;
	}
	else Push(readStack, tNilAtom) ;
	if( tk != closeSTk ) ReaderError("']' expected") ;
	n = readStack - save ;
	readStack = save ;
	return( ArrayToList(n, save) ) ;
}

static Pt CurlyBrackets()
{
	Pt t ;

	PeekToken() ;	/* peek after { */
	if( tk == closeCTk )
	{
		NextToken() ;	/* skip } */
		return( MakeAtom("{}") ) ;
	}
	t = Term(maxPrec) ;
	NextToken() ;
	if( tk != closeCTk )
		ReaderError("'}' expected") ;
	return( MakeUnStruct(LookupFunctor2("{}", 1), t) ) ;
}

static Pt MinusOp(AtomPt atom)
{
	if( atom == LookupAtom("-") && tk == vnsTk && IsNumber(tkTerm) )
	{
		Pt t = tkTerm ;
		NextToken() ;
		return( IsInt(t) ? MakeInt(-XInt(t)) : MakeReal(-XReal(t)) ) ;
	}
	else return( nil ) ;
}

static Pt Term(int n)
{
	register Pt res ;
	AtomPt atom ;
	int m = 0, p, lp, rp ;

	NextToken() ;
	switch( tk )
	{
		case vnsTk:		res = tkTerm ; break ;
		case openRTk:	res = RoundBrackets() ; break ;
		case openSTk:	res = SquareBrackets() ; break ;
		case openCTk:	res = CurlyBrackets() ; break ;
		case closeRTk: case closeSTk: case closeCTk:
		case commaTk: case barTk: case dotTk:			goto errorL ;
		case atomTk:
		{
			atom = tkAtom ;
			PeekToken() ;
			if( tk == openRTk && tkPreBlanks == 0 )
			{
				res = Args(atom) ;
				break ;
			}
			elif( p = Prefix(atom, &rp) )
			{
				if( (res = MinusOp(atom)) != nil ) break ;
				if( n < p ) goto errorL ;
				switch( tk )
				{
					case vnsTk: case openRTk:
					case openSTk: case openCTk:					goto prefixL ;
					case commaTk: case closeRTk: case closeSTk:
					case closeCTk: case dotTk: case barTk:		goto atomL ;
					case atomTk:
					{
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
		default:	InternalError("Term (2)") ;
	}

rightL:
	PeekToken() ;
	switch( tk )
	{
		case dotTk: case closeRTk: case closeSTk: case closeCTk:	goto doneL ;
		case vnsTk: case openRTk: case openSTk: case openCTk:		goto errorL ;
		case commaTk:
		{
			if( n >= commaPrec && m < commaPrec )
			{
				m = commaPrec ;
				NextToken() ;
				res = MakeBinStruct(commaFunctor, res, Term(m)) ;
				if( n > m ) goto rightL ;
			}
			goto doneL ;
		}
		case barTk:
		{
			if( n >= barPrec && m < barPrec )
			{
				m = barPrec ;
				NextToken() ;
				res = MakeBinStruct(semicolonFunctor, res, Term(m)) ;
				if( n > m ) goto rightL ;
			}
			goto doneL ;
		}
		case atomTk:
		{
			atom = tkAtom ;
			if( (p = Postfix(atom, &lp)) && n >= p && m <= lp )
			{
				NextToken() ;
				if( (p = Infix(atom, &lp, &rp)) && n >= p && m <= lp )
				{
					PeekToken() ;
					switch( tk )
					{
						case openRTk: case openSTk:
						case openCTk: case vnsTk:		goto infixL ;
						case commaTk: case barTk:
						case closeRTk: case closeSTk:	goto posfixL ;
						case atomTk:
						{
							if( ExclusivelyPrefix(tkAtom) ) goto infixL ;
							else goto posfixL ;
						}
					}
					InternalError("Term (3)") ;
				}
				else goto posfixL ;
			}
			elif( (p = Infix(atom, &lp, &rp)) && n >= p && m <= lp )
			{
				NextToken() ;
				goto infixL ;
			}
			else goto doneL ;	/* not an operator */
		}
	}
	InternalError("Term (4)") ;

errorL:
	ReaderError(nil) ;
doneL:
	return( res ) ;
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
	readStack = codeBuff ;
	saveStructState = H ;
	switch( setjmp(readJmp) )
	{
		case 0:
		{
			Pt res = Term(maxPrec) ;
			NextToken() ;
			if( tk != dotTk) ReaderError("'.' expected") ;
			if( vars != nil) *vars = MakeVarList() ;
			return( res ) ;
		}
		case 1:
		{
			H = saveStructState ;
			return( nil ) ;
		}
		case 2:
		{
			H = saveStructState ;
			if( vars != nil) *vars = tNilAtom ;
			return( TagAtom(eofAtom) ) ;
		}
	}
	InternalError("ReadTerm") ;
}
