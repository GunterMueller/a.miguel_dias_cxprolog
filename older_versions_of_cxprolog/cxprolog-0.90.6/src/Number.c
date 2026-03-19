/*
 *   This file is part of the CxProlog system

 *   Number.c
 *   by A.Miguel Dias - 2001/02/27
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL

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
#include <locale.h>

int intSize, floatSize ;
static Str32 floatFormat, intFormat ;


/* BOOL, CHAR, CODE & BYTE */

Pt MakeBool(Bool b)
{
	return b ? tTrueAtom : tFalseAtom ;
}

Pt MakeChar(int c)
{
	Str32 b ;
	CharPt s = b ;
	CharEncode(s, c) ;
	*s = '\0' ;
	return MakeAtom(b) ;
}

int XChar(Pt t) /* pre: IsAtom(t) */
{
	CharPt s = XAtomName(t) ;
	int c = CharDecode(s) ;
	if( c == '\0' || *s == '\0') return c ;
	else return -1 ;
}

Pt MakeCode(register int c) /* pre: c is valid code */
{
	if( c == '\n' ) c = 10 ;
	else if( c == eofMark ) c = eofCode_flag ;
	return TagInt( ClearTag(EncodeBits(c)) ) ;
}

int XCode(Pt t) /* pre: IsCode(t) */
{
	int c = cPInt(cWord(t) << 1) >> 3  ;
	if( c == 10 ) c = '\n' ;
	return c ;
}

Pt MakeByte(register int c)
{
	return TagInt( ClearTag(EncodeBits(c)) ) ;
}

int XByte(Pt t) /* pre: IsByte(t) */
{
	return (cWord(t) << 1) >> 3 ;
}


/* INT */

Pt MakeInt(PInt n)
{
	if( minInt <= n && n <= maxInt )
		return TagInt( ClearTag(EncodeBits(n)) ) ;
	else return MakeFloat(n) ;
}

Pt MakeInt64(Int64 n) /* used in the java interface */
{
	if( minInt <= n && n <= maxInt )
		return TagInt( ClearTag(EncodeBits(n)) ) ;
	else return MakeFloat(n) ;
}

PInt XInt(Pt t) /* pre: IsInt(t) */
{
	return cPInt(cWord(t) << 1) >> 3 ;
}

PInt XAsInt(Pt t) /* pre: IsNumber(t) */
{
	if( IsInt(t) ) return XInt(t) ;
	return XFloat(t) + 0.5 ;
}

Int64 XAsInt64(Pt t) /* pre: IsNumber(t) */
{
	if( IsInt(t) ) return XInt(t) ;
	return XFloat(t) + 0.5 ;
}

int CompareInt(PInt i1, PInt i2)
{
	return i1 == i2 ? 0 : i1 > i2 ? 1 : -1 ;
}


/* FLOAT */

typedef struct FloatEntry {
	struct FloatEntry *nextHash ;		/* Next PFloat in the hash chain */
	PFloat value ;						/* Float value */
} FloatEntry, *FloatEntryPt ;

#define floatTableSize	256

static FloatEntryPt *floatTable ;

static void FloatsInit()
{
	register int i ;
	floatTable = PermAllocate(floatTableSize) ;
	dotimes(i, floatTableSize)
		floatTable[i] = nil ;
}

static FloatEntryPt LookupFloatEntryPt(PFloat r)
{
	register Char slot ;
	register FloatEntryPt rt ;
	register CharPt s = cCharPt(&r) ;
/* hash function */
	slot = s[1] + s[3] + s[5] + s[7] ;
	if( floatSize >= 80 ) slot += s[9] ;
/* find float */
	doseq(rt, floatTable[slot], rt->nextHash)
		if( r == rt->value )
			return rt ;
/* create new float entry */
	rt = PermAllocate(WordsOf(FloatEntry)) ;
	rt->nextHash = floatTable[slot] ;
	rt->value = r ;
	floatTable[slot] = rt ;
	return rt ;
}

Pt MakeFloat(PFloat r)
{
	register PInt i = r ;
	if( i != r || i < minInt || i > maxInt )
		return TagFloat(LookupFloatEntryPt(r)) ;
	else return MakeInt(i) ;
}

PFloat XFloat(Pt t) /* pre: IsFloat(t) */
{
	return ((FloatEntryPt)XPt(t))->value ;
}

PFloat XAsFloat(Pt t) /* pre: IsNumber(t) */
{
	if( IsInt(t) ) return XInt(t) ;
	return ((FloatEntryPt)XPt(t))->value ;
}

int CompareFloat(PFloat r1, PFloat r2)
{
	if( r1 == r2 ) return 0 ;
	if(	r1 < r2 ) return -1 ;
	if( r1 > r2 ) return 1 ;
	return -2 ;
}

int CalculateFloatSize()
{
	register int i, n = 0 ;
	PFloat f = 1.01 ;
	register CharPt s = cCharPt(&f) ;
	f *= f ;
	dotimes(i, sizeof(PFloat))
		if( *s++ != 0 )
			n++ ;
	return n * 8 ;
}

static CharPt BuildFloatFormat(int precision)
{
	if( precision > 0 )
		return GStrFormat("%%.%dllg", precision) ;
	else
		return GStrFormat("%%.%dllf", -precision) ;
}

void FloatDisplayPrecUpdateFlag(int newValue)
{
	floatDisplayPrec_flag = newValue ;
	strcpy(floatFormat, BuildFloatFormat(floatDisplayPrec_flag)) ;
}


/* NUMBER */

/* The use of 'setlocale' makes the sintax of real numbers immune to the
   current locale configuration. Note that 'setlocale' is very fast if the
   locale doesn't really change, as will be the case almost always. */

CharPt XNumberAsStr(register Pt t) /* already deref */
{
	CharPt res ;

	if( IsInt(t) ) {
		res = GStrFormat(intFormat, XInt(t)) ;
	}
	elif( IsFloat(t) ) {
		CharPt loc ;
		PFloat f = XFloat(t) ;
		loc = setlocale(LC_NUMERIC, "C") ;
		res = GStrFormat(floatFormat, f) ;
		setlocale (LC_NUMERIC, loc) ;
	}
	elif( IsThisStruct(t, formatFunctor) ) {
		CharPt format ;
		PFloat f = XTestFloat(XStructArg(t,0)) ;
		CharPt loc ;

		loc = setlocale(LC_NUMERIC, "C") ;
		format = BuildFloatFormat(XTestInt(XStructArg(t,1))) ;
		res = GStrFormat(format, f) ;
		setlocale (LC_NUMERIC, loc) ;
	}	
	else return InternalError("XNumberAsStr") ;
	return res ;
}

Pt NumberFromStr(CharPt s)
{
	PFloat d ;
	CharPt loc ;

	if( InRange(s[0], '0', '9') ) ;
	elif( s[0] == '-' && InRange(s[1], '0', '9') ) ;
	else return nil ;

	loc = setlocale(LC_NUMERIC, "C") ;
	d = strtod(s, (char **)&s) ;
	setlocale (LC_NUMERIC, loc) ;

	if( s[0] == '\0' && InRange(s[-1], '0', '9') )
		return MakeFloat(d) ;
	else return nil ;
}

int CompareNumber(register Pt t1, register Pt t2) /* already deref */
{
	if( IsInt(t1) ) {
		if( IsInt(t2) ) return CompareInt(XInt(t1), XInt(t2)) ;
		if(IsFloat(t2) ) return CompareFloat(XInt(t1), XFloat(t2)) ;
	}
	elif( IsFloat(t1) ) {
		if( IsInt(t2) ) return CompareFloat(XFloat(t1), XInt(t2)) ;
		if( IsFloat(t2) ) return CompareFloat(XFloat(t1), XFloat(t2)) ;
	}
	InternalError("CompareNumber") ;
	return false ;
}

void NumbersInit()
{
	intSize = sizeof(Word) * 8 - 3 ;
	floatSize = CalculateFloatSize() ;
	strcpy(intFormat, "%ld") ;
	strcpy(floatFormat, BuildFloatFormat(floatDisplayPrec_flag)) ;
	FloatsInit() ;
}


/* CXPROLOG C'BUILTINS */

static void PFloats()
{
	register int i ;
	register Size n ;
	register FloatEntryPt rt ;
	ShowVersion() ;
	Write("Floats:\n") ;
	Write("  The high precision %d-bit FLOATS are stored in a %d-entry hash table\n",
					floatSize,
					floatTableSize) ;
	for( n = 0, i = 0 ; i < floatTableSize ; i++ )
		doseq(rt, floatTable[i], rt->nextHash) n++ ;
	Write("    Current number of floats in the hash table -> %7ld\n", n) ;
	Write("    Current average length of the hash chains -> %5f\n", n/(double)floatTableSize) ;
	Write("    Current length of the individual hash chains ->") ;
	for( i = 0 ; i < floatTableSize ; i++ ) {
		n = 0 ;
		doseq(rt, floatTable[i], rt->nextHash) n++ ;
		Write(" %ld", n) ;
	}
	Write("\n") ;
	JumpNext()
}

static void ShowFloat(PFloat f)
{
	int i ;
	register CharPt s = cCharPt(&f) ;
	Write("%18.8lle", f) ;
	dotimes(i, sizeof(PFloat))
		Write("%4d", s[i]) ;
	Write("\n") ;
}

static void PCheckFloats()
{
	int i ;
	PFloat f = 1.01 ;
	dotimes(i,20) {
		ShowFloat(f) ;
		f *= f ;
	}
	ShowFloat(0) ;
	ShowFloat(INFINITY) ;
	ShowFloat(NAN) ;
	JumpNext()
}

void NumbersInit2()
{
	InstallCBuiltinPred("floats", 0, PFloats) ;
	InstallCBuiltinPred("chk_floats", 0, PCheckFloats) ;
}
