/*
 *   This file is part of the NanoProlog system

 *   TermWrite.c
 *   by A.Miguel Dias - 93/8/23
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

 931120: WriteDepth: -1 -> 32000; #include <stdio.h>
 931117: release of version 0.5

*/

#include "NanoProlog.h"
#include "ctype.h"

static WriteMode wMode ;
static int termMaxDepth = 30, listMaxLength = 999 ;
static int currTermDepth ;

/* (a.b) -> [a|b]	 [x] == .(x,nil). */

static void PutQString(CharPt s, Bool b)
{
	if( b ) Put('\'') ;
	PutString(s) ;
	if( b ) Put('\'') ;
}

static void PutAtom(AtomPt at)
{
	register CharPt s = AtomName(at) ;

	if( wMode != display && wMode != quoted ) PutString(s) ;
	else switch( *s )
	{
		case ',': case ';': case '!':
		{
			PutQString(s, s[1] != '\0') ;
			break ;
		}
		case '[':
		{
			PutQString(s, s[1] != ']' || s[2] != '\0') ;
			break ;
		}
		case '{':
		{
			PutQString(s, s[1] != '}' || s[2] != '\0') ;
			break ;
		}
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
		case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
		case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
		case 'v': case 'w': case 'x': case 'y': case 'z':
		{
			for( ; *s != '\0' ; s++ )
				if( not (isalnum(*s) || *s == '_') )
					{ PutQString(AtomName(at), true) ; return ; }
			PutString(AtomName(at)) ;
			break ;
		}
		default:
		{
			for( ; *s != '\0' ; s++ )
				if( not IsSymbol(*s) )
					{ PutQString(AtomName(at), true) ; return ; }
			PutString(AtomName(at)) ;
			break ;
		}
	}
}

static void WriteTerm2(register Pt term, int p) ;

static void WriteStruct(Pt term, int m)
{
	register StructPt st = XStruct(term) ;
	register AtomPt atom = StructAtom(st) ;
	int p, lp, rp ;
	
	switch( XStructArity(term) )
	{
		case 0: InternalError("WriteStruct") ;
		case 1:
		{
			if( wMode == display ) goto displayL ; 
			elif( atom == LookupAtom("$VAR") )
			{
				Pt t = Drf(XStructArg(term, 0)) ;
				if( IsAtom(t) )
					PutAtom(XAtom(term)) ;
				elif( IsInt(t) && (p = XInt(t)) >= 0 )
				{
					char s[256] ;
					s[0] = 'A' + p % 26 ;
					s[1] = '\0' ;
					if( p > 25 ) sprintf(s + 1, "%d", p / 26) ;
					PutString(s) ;
				}
				else goto displayL ;
			}
			elif( atom == LookupAtom("{}") )
			{
				Put('{') ;
				WriteTerm2(StructArg(st, 0), maxPrec) ;
				Put('}') ;
			}
			elif( p = Prefix(atom, &rp) )
			{
				if( p > m ) Put('(') ;
				PutAtom(atom) ;
				if( isalpha(AtomName(atom)[0]) ) Put(' ') ;
				WriteTerm2(StructArg(st, 0), rp) ;
				if( p > m ) Put(')') ;
			}
			elif( p = Postfix(atom, &lp) )
			{
				if( p > m ) Put('(') ;
				WriteTerm2(StructArg(st, 0), lp) ;
				if( isalpha(AtomName(atom)[0]) ) Put(' ') ;
				PutAtom(atom) ;
				if( p > m ) Put(')') ;
			}
			else goto displayL ;
			break ;
		}
		case 2:
		{
			if( wMode == display ) goto displayL ; 
			elif( (p = Infix(atom, &lp, &rp)) != 0 )
			{
				if( p > m ) Put('(') ;
				WriteTerm2(XStructArg(term, 0), lp) ;
				if( isalpha(AtomName(atom)[0]) ) Put(' ') ;
				PutAtom(atom) ;
				if( isalpha(AtomName(atom)[0]) ) Put(' ') ;
				WriteTerm2(XStructArg(term, 1), rp) ;
				if( p > m ) Put(')') ;
			}
			else goto displayL ;
			break ;
		}
		default:
		{
displayL:	PutAtom(atom) ;
			Put('(') ;
			WriteTerm2(StructArg(st, 0), subPrec) ;
			for( p = 1 ; p < StructArity(st) ; p++ )
			{
				Put(',') ;
				WriteTerm2(StructArg(st, p), subPrec) ;
			}
			Put(')') ;
			break ;
		}
	}
}

static void WriteList(Pt term)
{
	register int i ;
	
	Put('[') ;
	for( i = 0 ; i < listMaxLength ; i++ )
	{
		WriteTerm2(XListHead(term), subPrec) ;
		term = Drf(XListTail(term)) ;
		if( not IsList(term) ) break ;
		Put(',') ;
	}
	if( i == listMaxLength )
		WriteTerm2(MakeAtom("..."), subPrec) ;
	elif( term != tNilAtom )
	{
		Put('|') ;
		WriteTerm2(term, subPrec) ;
	}
	Put(']') ;
}

static Bool IPortray(Pt term)
{
	return( false ) ;
}

static void WriteTerm2(register Pt term, int p)
{	
	char s[256] ;

	VarValue(term) ;
	if( IsAtom(term) ) PutAtom(XAtom(term));
	elif( IsVar(term) ) PutString(VarName(term)) ;
	elif( IsInt(term) ) { sprintf(s, "%ld", XInt(term)) ; PutString(s) ; }
	elif( IsReal(term) ) { sprintf(s, "%.5g", XReal(term)) ; PutString(s) ; }
	elif( IsList(term) )
	{
		if( currTermDepth > termMaxDepth ) PutAtom(LookupAtom("...")) ;
		elif( wMode == print && IPortray(term) ) ;
		else { currTermDepth++ ; WriteList(term) ; currTermDepth-- ; }
	}
	elif( IsStruct(term) )
	{
		if( currTermDepth > termMaxDepth ) PutAtom(LookupAtom("...")) ;
		elif( wMode == print && IPortray(term) ) ;
		else { currTermDepth++ ; WriteStruct(term, p) ; currTermDepth--;}
	}
	else { sprintf(s, "<UNKNOWN TAG::%lx>", term) ; PutString(s) ; }
}

void GWriteTerm(Pt term, register WriteMode mode, Bool stdOutput)
{
	wMode = mode ;
	if( stdOutput ) SetTempOutput(dflOutputStream) ;
	WriteTerm2(term, maxPrec) ;
	if( stdOutput ) RestoreOutput() ;
}

void WriteTerm(Pt term)
{
	wMode = wNormal ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
}

void WritelnTerm(Pt term)
{
	wMode = wNormal ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
	Nl() ;
}

void WriteTermStd(Pt term)
{
	wMode = wNormal ;
	SetTempOutput(dflOutputStream) ;
	currTermDepth = 0 ;
	WriteTerm2(term, maxPrec) ;
	RestoreOutput() ;
}

void WriteDepth(int termDepth, int listLength)
{
	termMaxDepth = termDepth == 0 ? 32000 : termDepth ;
	listMaxLength = listLength == 0 ? 32000 : listLength ;
}
