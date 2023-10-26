/*
 *   This file is part of the CxProlog system

 *   Number.c
 *   by A.Miguel Dias - 2001/02/27
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
#include <locale.h>


/* INT */

Pt MakeInt(PInt n)
{
	if( minInt <= n && n <= maxInt )
		return TagInt( ClearTag(EncodeBits(n)) ) ;
	else return MakeFloat(n) ;
}

PInt XInt(Pt t)
{
	return cPInt(cWord(t) << 1) >> 3 ;
}

int CompareInt(PInt i1, PInt i2)
{
	return i1 == i2 ? 0 : i1 > i2 ? 1 : -1 ;
}

Pt MakeCode(register int c)
{
	if( c == '\n' ) c = 10 ;
	else if( c == eofMark ) c = eofCode_flag ;
	return TagInt( ClearTag(EncodeBits(c)) ) ;
}

int XCode(Pt t)
{
	int c = cPInt(cWord(t) << 1) >> 3  ;
	if( c == eofCode_flag ) c = eofMark ;
	if( c == 10 ) c = '\n' ;
	return c ;
}

Pt MakeChar(int c)
{
	if( c == eofMark )
		return tEofAtom ;
	else {
		static Char b[2] = "\0\0" ;
		b[0] = c ;
		return MakeAtom(b) ;
	}
}

Pt MakeByte(register int c)
{
	return TagInt( ClearTag(EncodeBits(c)) ) ;
}

int XByte(Pt t)
{
	return (cWord(t) << 1) >> 3 ;
}


/* FLOAT */

typedef struct FloatEntry {
	struct FloatEntry *nextHash ;		/* Next PFloat in the hash chain */
	PFloat value ;						/* Float value */
} FloatEntry, *FloatEntryPt ;

#define floatTableSize	256

static FloatEntryPt *floatTable ;
static PFloat nanFloat ;

static void FloatsInit()
{
	register int i ;
	floatTable = Allocate(floatTableSize) ;
	dotimes(i, floatTableSize)
		floatTable[i] = nil ;
	nanFloat = infFloat/infFloat ;
}

static FloatEntryPt LookupFloatEntryPt(PFloat r)
{
	register Char slot ;
	register FloatEntryPt rt ;
	register CharPt s = cCharPt(&r) ;
/* hash function */
	slot = s[1] + s[3] + s[5] + s[7] ;
	if( sizeof(PFloat) >= 10 )
		slot += s[9] ;
	if( sizeof(PFloat) >= 12 )
		slot += s[11] ;
/* find float */
	doseq(rt, floatTable[slot], rt->nextHash)
		if( r == rt->value )
			return rt ;
/* create new float entry */
	rt = Allocate(WordsOf(FloatEntry)) ;
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

PFloat XFloat(Pt p)
{
	return ((FloatEntryPt)XPt(p))->value ;
}

PFloat XAsFloat(Pt p)
{
	if( IsInt(p) ) return XInt(p) ;
	return ((FloatEntryPt)XPt(p))->value ;
}

int CompareFloat(PFloat r1, PFloat r2)
{
	return r1 == r2 ? 0 : r1 > r2 ? 1 : -1 ;
}

static int CalculateFloatSize()
{
	register int i, n = sizeof(PFloat) ;
	PFloat f = 1.01 ;
	register CharPt s = cCharPt(&f) ;
	f *= f ;
	dotimes(i, sizeof(PFloat))
		if( *s++ == 0 )
			n-- ;
	return n * 8 ;
}

static Bool IsNan(PFloat f)
{
	register CharPt a = cCharPt(&f) ;
	register CharPt b = cCharPt(&nanFloat) ;
	register int i ;
	dotimes(i, sizeof(PFloat))
		if( *a++ != *b++ )
			return false ;
	return true ;
}

static CharPt BuildFloatFormat(int precision)
{
	/* The use of 'setlocale' makes the sintax of real numbers immune to the
       current locale configuration. Note that 'setlocale' is very fast if the
       locale doesn't really change, as will be the case almost always. */
	CharPt loc = setlocale(LC_NUMERIC, "C") ;
	if( precision > 0 )
		sprintf(retString, "%%.%dllg", precision) ;
	else
		sprintf(retString, "%%.%dllf", -precision) ;
	return retString ;
	setlocale (LC_NUMERIC, loc) ;
}


/* NUMBER */

int intSize, floatSize ;
CharPt floatFormat, intFormat ;

CharPt XNumberAsStr(register Pt t) /* already deref */
{
	Str256 format ;
	if( IsInt(t) ) {
		sprintf(retString, intFormat, XInt(t)) ;
		return retString ;
	}
	if( IsFloat(t) ) {
		PFloat f = XFloat(t) ;
		if( f == infFloat ) return "inf" ;
		if( IsNan(f) ) return "undef" ;
		strcpy(format, BuildFloatFormat(floatDisplayPrecision_flag)) ;
		sprintf(retString, format, f) ;
		return retString ;
	}
	if( IsThisStruct(t, formatFunctor) ) {
		PFloat f = XTestFloat(XStructArg(t,0)) ;
		if( f == infFloat ) return "inf" ;
		if( IsNan(f) ) return "undef" ;
		strcpy(format, BuildFloatFormat(XTestInt(XStructArg(t,1)))) ;
		sprintf(retString, format, f) ;
		return retString ;
	}	
	return InternalError("NumberAsStr") ;
}

Pt NumberFromStr(CharPt s)
{
	PFloat d ;
	CharPt loc ;

	if( InRange(s[0], '0', '9') ) ;
	elif( s[0] == '-' && InRange(s[1], '0', '9') ) ;
	else return nil ;

	/* The use of 'setlocale' makes the sintax of real numbers immune to the
       current locale configuration. Note that 'setlocale' is very fast if the
       locale doesn't really change, as will be the case almost always. */
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
	floatFormat = "%llf" ;
	intFormat = "%ld" ;
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
	Write("  The high precision FLOATS are stored in a %d-entry hash table\n",
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

static void PCheckFloats()
{
	register int i, j ;
	PFloat f = 1.01 ;
	register CharPt s = cCharPt(&f) ;
	dotimes(i,20) {
		Write("%18.8lle", f) ;
		dotimes(j, sizeof(PFloat))
			Write("%4d", s[j]) ;
		Write("\n") ;
		f *= f ;
	}
	f = 0 ;
	Write("%18.8lle", f) ;
	dotimes(j, sizeof(PFloat))
		Write("%4d", s[j]) ;
	Write("\n") ;
	f = infFloat ;
	Write("%18.8lle", f) ;
	dotimes(j, sizeof(PFloat))
		Write("%4d", s[j]) ;
	Write("\n") ;
	f = nanFloat ;
	Write("%18.8lle", f) ;
	dotimes(j, sizeof(PFloat))
		Write("%4d", s[j]) ;
	Write("\n") ;
	
	JumpNext()
}

void NumbersInit2()
{
	InstallCBuiltinPred("floats", 0, PFloats) ;
	InstallCBuiltinPred("chk_floats", 0, PCheckFloats) ;
}
