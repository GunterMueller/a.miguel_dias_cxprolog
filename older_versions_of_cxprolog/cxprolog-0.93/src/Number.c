/*
 *   This file is part of the CxProlog system

 *   Number.c
 *   by A.Miguel Dias - 2001/02/27
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL

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
#include <locale.h>
#include <math.h>

#ifdef FP_ILOGB0
#define HAS_LONG_DOUBLE_OPS		1
#endif

int intSize, floatSize ;
static Str32 intFormat, lLIntFormat, floatFormat, intFloatFormat ;


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
	int c = cPInt(cWord(t) << 1) >> 3 ;
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

Pt minusOneIntPt, zeroIntPt, oneIntPt, twoIntPt, threeIntPt,
   maxIntPt, maxUIntPt, minIntPt ;

Pt MakeInt(PInt n)
{
	if( minInt <= n && n <= maxInt )
		return TagInt( ClearTag(EncodeBits(n)) ) ;
	else return MakeFloat(n) ;
}

Pt MakeUInt(PInt n)
{
	if( n < 0 ) Error("Negative argument in MakeUInt") ;
	if( n <= maxUInt )
		return TagInt( ClearTag(EncodeBits(n)) ) ;
	else return MakeFloat(n) ;
}

Pt MakeLLInt(LLInt n) /* used in the java interface */
{
	if( minInt <= n && n <= maxInt )
		return TagInt( ClearTag(EncodeBits(n)) ) ;
	else {
		PFloat f = cPFloat(n) ;
		if( IsIntFloat(f) )
			return MakeFloat(f) ;
	}
	return Error(GStrFormat("Cannot encode the large-int '%s'", lLIntFormat), n) ;
}

PInt XInt(Pt t) /* pre: IsInt(t) */
{
	return cPInt(cWord(t) << 1) >> 3 ;
}

PInt XUInt(Pt t) /* pre: IsNat(t) */
{
	return (cWord(t) << 1) >> 3 ;
}

PInt XAsInt(Pt t) /* pre: IsNumber(t) */
{
	if( IsInt(t) ) return XInt(t) ;
	return cPInt(XFloat(t) + 0.5) ;
}

LLInt XAsLLInt(Pt t) /* pre: IsNumber(t) */
{
	if( IsInt(t) ) return XInt(t) ;
	else {
		PFloat f = XFloat(t) ;
		if( IsIntFloat(f) )
			return (LLInt)f ;
		else {
			Error("Invalid large-int '%s'", XNumberAsStr(t)) ;
			return 0 ;
		}
	}
}

int CompareInt(PInt i1, PInt i2)
{
	return i1 == i2 ? 0 : i1 > i2 ? 1 : -1 ;
}


/* FLOAT */

Pt nanFloatPt, infFloatPt ;
static PFloat nanFloat, infFloat, ninfFloat ;

typedef struct FloatEntry {
	struct FloatEntry *nextHash ;		/* Next PFloat in the hash chain */
	PFloat value ;						/* Float value */
} FloatEntry, *FloatEntryPt ;

#define floatTableCapacity	256

static FloatEntryPt *floatTable ;

static void FloatsInit()
{
	register int i ;
	floatTable = PermAllocate(floatTableCapacity) ;
	dotimes(i, floatTableCapacity)
		floatTable[i] = nil ;
}

static FloatEntryPt LookupFloatEntryPt(PFloat r)
{
	register UChar slot ;
	register FloatEntryPt rt ;
	register UCharPt u = cUCharPt(&r) ;
/* hash function */
	slot = u[1] + u[3] + u[5] + u[7] ;
	if( floatSize >= 80 ) slot += u[9] ;
/* find float */
	doseq(rt, floatTable[slot], rt->nextHash)
		if( rt->value == r )
			return rt ;
/* create new float entry */
	rt = PermAllocate(WordsOf(FloatEntry)) ;
	rt->value = r ;
	rt->nextHash = floatTable[slot] ;
	floatTable[slot] = rt ;
	return rt ;
}

Pt MakeFloat(PFloat r)
{
	register PInt i = cPInt(r) ;
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
#if HAS_LONG_DOUBLE_OPS
	if( precision > 0 )
		return GStrFormat("%%.%dllg", precision) ;
	else
		return GStrFormat("%%.%dllf", -precision) ;
#else
	if( precision > 0 )
		return GStrFormat("%%.%dlg", precision) ;
	else
		return GStrFormat("%%.%dlf", -precision) ;
#endif
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
	if( IsInt(t) )
		return GStrFormat(intFormat, XInt(t)) ;
	elif( IsFloat(t) ) {
		PFloat f = XFloat(t) ;
		if( IsIntFloat(f) ) return GStrFormat(intFloatFormat, f) ;
		elif( f != f ) return "nan" ;
		elif( f == infFloat ) return "inf" ;
		elif( f == ninfFloat ) return "-inf" ;
		else {
			CharPt loc = setlocale(LC_NUMERIC, "C") ;
			CharPt res = GStrFormat(floatFormat, f) ;
			setlocale (LC_NUMERIC, loc) ;
			return res ;
		}
	}
	elif( IsThisStruct(t, colonFunctor) ) {
		PFloat f = XTestFloat(XStructArg(t,0)) ;
		CharPt format = BuildFloatFormat(XTestInt(XStructArg(t,1))) ;
		CharPt loc = setlocale(LC_NUMERIC, "C") ;
		CharPt res = GStrFormat(format, f) ;
		setlocale (LC_NUMERIC, loc) ;
		return res ;
	}	
	else return InternalError("XNumberAsStr") ;
}

Pt NumberFromStr(CharPt s)
{
	PFloat d ;
	CharPt loc ;

	if( InRange(s[0], '0', '9') ) ;
	elif( s[0] == '-' && InRange(s[1], '0', '9') ) ;
	else return nil ;

	loc = setlocale(LC_NUMERIC, "C") ;
#if HAS_LONG_DOUBLE_OPS
	d = strtold(s, (char **)&s) ;
#else
	d = strtod(s, (char **)&s) ;
#endif
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
#if HAS_LONG_LONG
	strcpy(lLIntFormat, "%lld") ;
#else
	strcpy(lLIntFormat, "%ld") ;
#endif
	strcpy(floatFormat, BuildFloatFormat(floatDisplayPrec_flag)) ;
	strcpy(intFloatFormat, BuildFloatFormat(80)) ;
	FloatsInit() ;
}


/* CXPROLOG C'BUILTINS */

static void PFloats()
{
	register int i ;
	register Size n ;
	register FloatEntryPt rt ;
	ShowVersion() ;
	Write("FLOATS:\n") ;
	Write("  The high precision %d-bit FLOATS are stored in a %d-entry hash table\n",
					floatSize,
					floatTableCapacity) ;
	for( n = 0, i = 0 ; i < floatTableCapacity ; i++ )
		doseq(rt, floatTable[i], rt->nextHash) n++ ;
	Write("    Current number of floats in the hash table -> %7ld\n", n) ;
	Write("    Current average length of the hash chains -> %5f\n",
											n/(double)floatTableCapacity) ;
	Write("    Current length of the individual hash chains ->") ;
	for( i = 0 ; i < floatTableCapacity ; i++ ) {
		n = 0 ;
		doseq(rt, floatTable[i], rt->nextHash) n++ ;
		Write(" %ld", n) ;
	}
	Write("\n") ;
	JumpNext() ;
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
	ShowFloat(nanFloat) ;
	ShowFloat(infFloat) ;
	JumpNext() ;
}
void NumbersInit2()
{
	PFloat zero = 0.0 ;
	nanFloat = 0.0/zero ;	/* div by zero */
	infFloat = 1.0/zero ;	/* div by zero */
	ninfFloat = -infFloat ;

	minusOneIntPt = MakeInt(-1) ;
	zeroIntPt = MakeInt(0) ;
	oneIntPt  = MakeInt(1) ;
	twoIntPt = MakeInt(2) ;
	threeIntPt = MakeInt(3) ;
	maxIntPt = MakeInt(maxInt) ;
	maxUIntPt = MakeUInt(maxUInt) ;
	minIntPt = MakeInt(minInt) ;

	nanFloatPt = MakeFloat(nanFloat) ;
	infFloatPt = MakeFloat(infFloat) ;

	InstallCBuiltinPred("floats", 0, PFloats) ;
	InstallCBuiltinPred("chk_floats", 0, PCheckFloats) ;
}
