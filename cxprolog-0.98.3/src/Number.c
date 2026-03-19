/*
 *   This file is part of the CxProlog system

 *   Number.c
 *   by A.Miguel Dias - 2001/02/27
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2016 A.Miguel Dias, CITI, DI/FCT/UNL

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

#if FLOAT_IS_LONG_DOUBLE
#define FMTD		"%%%d.%dL%s.0"
#define STRTOD		strtold
#else
#define FMTD		"%%%d.%d%s.0"
#define STRTOD		strtod
#endif


/* BOOL, CHAR, CODE & BYTE */

Pt MakeBool(Bool b)
{
	return b ? tTrueAtom : tFalseAtom ;
}

Pt MakeChar(WChar c)
{
	Str32 b ;
	CharPt s = b ;
	if( c == EOF ) return tEofAtom ;
	CharEncode(s, c) ;
	*s = '\0' ;
	return MakeAtom(b) ;
}

WChar XChar(Pt t) /* pre: IsAtom(t) */
{
	Str s = XAtomName(t) ;
	WChar c = CharDecode(s) ;
	if( c == '\0' || *s == '\0') return c ;
	else return -1 ;
}

Pt MakeCode(WChar c) /* pre: c is valid code */
{
	if( c == '\n' ) c = 10 ;
	else if( c == EOF ) c = eofCode_flag ;
	return EncodeInt(c) ;
}

WChar XCode(Pt t) /* pre: IsCode(t) */
{
	WChar c = DecodeInt(t) ;
	if( c == 10 ) c = '\n' ;
	return c ;
}

Pt MakeByte(register int c) /* pre: IsByte(c) */
{
	if( c == EOF ) c = -1 ;
	return EncodeInt(c) ;
}

int XByte(Pt t) /* pre: IsByte(t) */
{
	return DecodeUInt(t) ;
}


/* INT */

static Str32 intFormat = "", lLIntFormat = "" ;

Pt minusOneIntPt, zeroIntPt, oneIntPt, twoIntPt, threeIntPt,
   maxIntPt, maxUIntPt, minIntPt ;

Pt MakeInt(PInt n)
{
	if( minInt <= n && n <= maxInt )
		return EncodeInt(n) ;
	else return MakeFloat(n) ;
}

Pt MakeUInt(PInt n)
{
	if( n < 0 ) Error("Negative argument in MakeUInt") ;
	if( n <= maxUInt )
		return EncodeInt(n) ;
	else return MakeFloat(n) ;
}

Pt MakeLLInt(LLInt n) /* used in the java interface */
{
	if( minInt <= n && n <= maxInt )
		return EncodeInt(n) ;
	else {
		PFloat f = cPFloat(n) ;
		if( IsIntFloat(f) )
			return MakeFloat(f) ;
	}
	return Error(GStrFormat("Cannot encode the large-int '%s'", lLIntFormat), n) ;
}

PInt XInt(Pt t) /* pre: IsInt(t) */
{
	return DecodeInt(t) ;
}

PInt XUInt(Pt t) /* pre: IsNat(t) */
{
	return DecodeUInt(t) ;
}

PInt XAsInt(Pt t) /* pre: IsNumber(t) */
{
	if( IsInt(t) ) return XInt(t) ;
	else return cPInt(XFloat(t) + 0.5) ;
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

static void IntsInit(void)
{
	strcpy(intFormat, "%ld") ;
	strcpy(lLIntFormat, HasLongs() ? "%lld" : "%ld") ;
}


/* FLOAT */

/* CxProlog uses the highest precision float numbers available in C.
   The fact that floats are atomic terms is a bit of a problem.
   In CxProlog each atomic term must fit one machine word and
   atomic terms comparison must be implemented by simple word
   comparison. So we need to store each float as a unique entry
   in an hash table and represent that float by a pointer to the
   corresponding unique entry.
*/

static ExtraTypePt floatType ;

int floatSize ;
Pt nanFloatPt, infFloatPt, ninfFloatPt, eFloatPt, piFloatPt, epsilonFloatPt ;
static Str32 floatFormat = "" ;

typedef struct FloatEntry {
	ExtraDef(FloatEntry) ;
	PFloat value ;						/* Float value */
} FloatEntry, *FloatEntryPt ;

#define	cFloatEntryPt(f)			((FloatEntryPt)f)

#define FloatEntryValue(f)			(cFloatEntryPt(f)->value)
#define XFloatEntryValue(t)			FloatEntryValue(XPt(t))

static FloatEntryPt nanFloatEntry = nil ;

static Bool LookupFloatEntryAux(CVoidPt x, CVoidPt ref)
{
	return FloatEntryValue(x) == *cPFloatPt(ref) ;
}
static FloatEntryPt LookupFloatEntry(PFloat r)
{
	if( isnan(r) && nanFloatEntry != nil )
		return nanFloatEntry ;
	register UCharPt u = cUCharPt(&r) ;
	UChar slot = u[1] + u[3] + u[5] + u[7] + (floatSize >= 80 ? u[9] : 0) ; /* hash */
	FloatEntryPt f ;
	if( (f = ExtraFindFirst(floatType, slot, LookupFloatEntryAux, &r)) == nil ) {
		f = ExtraNew(floatType, slot) ;
		FloatEntryValue(f) = r ;
	}
	return f ;
}

Pt MakeFloat(PFloat r)
{
	return TagFloat(LookupFloatEntry(r)) ;
#if old
	register PInt i = cPInt(r) ;
	if( i != r || i < minInt || i > maxInt )
		return TagFloat(LookupFloatEntry(r)) ;
	else return MakeInt(i) ;
#endif
}

static Pt MakePermFloat(PFloat r)
{
	ExtraPermanent(LookupFloatEntry(r)) ;
	return MakeFloat(r) ;
}

PFloat XFloat(Pt t) /* pre: IsFloat(t) */
{
	return XFloatEntryValue(t) ;
}

PFloat XAsFloat(Pt t) /* pre: IsNumber(t) */
{
	if( IsInt(t) )
		return cPFloat(XInt(t)) ;
	else
		return XFloatEntryValue(t) ;
}

Str FloatAsStr(Str cfmt, int field, int prec, PFloat f)
{
	if( isnan(f) ) return "nan" ;
	elif( isinf(f) ) return f > 0 ? "+inf" : "-inf" ;
	else {
		Str fmt =
			field == -1 && prec == -1
				? cfmt
				: GStrFormat(FMTD, field, prec, cfmt) ;
		Str loc = setlocale(LC_NUMERIC, "C") ;
		Str res = GStrFormat(fmt, f) ;
		setlocale (LC_NUMERIC, loc) ;
	/* now, ensure there is a '.' or an 'e' */
		register CharPt pt;
		int dots = 0, es = 0;
		for( pt = cCharPt(res) ; *pt != '\0' ; pt++ )
			if( *pt == '.' ) dots++ ;
			elif( *pt == 'e' ) es++ ;
		if( dots > 1 || es > 0 )
			pt[-2] = '\0';
		return res ;
	}
}

int CompareFloat(PFloat r1, PFloat r2)
{
	if( r1 == r2 ) return 0 ;
	if(	r1 < r2 ) return -1 ;
	if( r1 > r2 ) return 1 ;
	return -2 ;
}

int CalculateIntSize(void)
{
	return sizeof(Word) * 8 - 3 ;
}

static int NonZero(PFloat f)	// passing parameter makes the float "clean"
{
	Str s = cCharPt(&f) ;
	unsigned int i, n = 0 ;
	dotimes(i, sizeof(PFloat))
		if( s[i] != 0 )
			n++ ;
	return n ;
}

int CalculateFloatSize(void)
{
	PFloat f = 1.0101 ;
	f *= f ;
	return NonZero(f) * 8 ;
}

Bool HasLongs(void)
{
#if defined(LLONG_MAX)
	return true ;
#else
	return false ;
#endif
}

void FloatDisplayPrecUpdateFlag(int newValue)
{
	floatDisplayPrec_flag = newValue ;
	if( floatDisplayPrec_flag > 0 )
		strcpy(floatFormat, GStrFormat(FMTD, 0, floatDisplayPrec_flag, "g")) ;
	else
		strcpy(floatFormat, GStrFormat(FMTD, 0, -floatDisplayPrec_flag, "f")) ;
}

static PFloat CalcEpsilon(void)
{
	PFloat ep = 1.0, one = 1.0 ;
	for( ; one + ep > one ; ep /= 2 ) ;
	return ep * 2 ;
}

static Size FloatSizeFun(CVoidPt x)
{
	Unused(x) ;
	return WordsOf(FloatEntry) ;
}

static Bool FloatBasicGCDelete(VoidPt x)
{
	Unused(x) ;
	return true ;
}

static void FloatsInit(void)
{
	floatType = ExtraTypeNew("FLOAT", FloatSizeFun, nil, FloatBasicGCDelete, 256) ;
	ExtraTypeDoesNotSupportAliasing(floatType) ;
	FloatDisplayPrecUpdateFlag(floatDisplayPrec_flag) ;
}


/* NUMBER */

/* The use of 'setlocale' makes the sintax of real numbers immune to the
   current locale configuration. Note that 'setlocale' is very fast if the
   locale doesn't really change, as it will be the case almost always. */

Str XNumberAsStr(register Pt t) /* already deref */
{
	if( IsInt(t) )
		return GStrFormat(intFormat, XInt(t)) ;
	elif( IsFloat(t) )
		return FloatAsStr(floatFormat, -1, -1, XFloat(t)) ;
	else
		return InternalError("XNumberAsStr") ;
}

Pt NumberFromStr(Str s)
{
	PInt i ;
	PFloat d ;
	Str loc ;
	CharPt end;
	
	if( InRange(s[0], '0', '9') ) ;
	elif( s[0] == '-' && InRange(s[1], '0', '9') ) ;
	else return nil ;

	i = strtol(s, &end, 10) ;
	if( end == nil || end[0] == '\0' )
		return MakeInt(i) ;	
	
	loc = setlocale(LC_NUMERIC, "C") ;
	d = STRTOD(s, &end) ;
	setlocale(LC_NUMERIC, loc) ;

	if( end == nil || (end[0] == '\0' && InRange(end[-1], '0', '9')) )
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

void NumbersInit(void)
{
	IntsInit() ;
	FloatsInit() ;
}


/* CXPROLOG C'BUILTINS */

static Size FloatsAux(CVoidPt x)
{
	FloatEntryPt f = cFloatEntryPt(x) ;
	Write("%s", FloatAsStr(floatFormat, -1, -1, FloatEntryValue(f))) ;
	return 1 ;
}
static void PFloats(void)
{
	ExtraShow(floatType, FloatsAux) ;
	JumpNext() ;
}

static void ShowFloat_double(double f)
{
	unsigned int i ;
	register Str s = cCharPt(&f) ;
	Write(GStrFormat("%25.100g", f)) ;
	dotimes(i, sizeof(double))
		Write("%4d", s[i]) ;
	Write("\n") ;
}

static void ShowFloat_long_double(long double f)
{
	unsigned int i ;
	register Str s = cCharPt(&f) ;
	Write(GStrFormat("%25.100Lg", f)) ;
	dotimes(i, sizeof(long double))
		Write("%4d", s[i]) ;
	Write("\n") ;
}

static void CheckFloats_double(void)
{
	long base = 12340 ;
	long mult = 100000 ;
	int i ;
	double f = base ;
	dotimes(i, 5) {
		ShowFloat_double(f) ;
		f = f * mult + base ;
	}
	ShowFloat_double(0) ;
	ShowFloat_double(NAN) ;
	ShowFloat_double(INFINITY) ;
}

static void CheckFloats_long_double(void)
{
	long base = 12340 ;
	long mult = 100000 ;
	int i ;
	long double f = base ;
	dotimes(i, 5) {
		ShowFloat_long_double(f) ;
		f = f * mult + base ;
	}
	ShowFloat_long_double(0) ;
	ShowFloat_long_double(NAN) ;
	ShowFloat_long_double(INFINITY) ;
}

static void PCheckFloats(void)
{
	CheckFloats_double() ;	
	CheckFloats_long_double() ;
	JumpNext() ;
}

void NumbersInit2(void)
{
	minusOneIntPt = MakeInt(-1) ;
	zeroIntPt = MakeInt(0) ;
	oneIntPt  = MakeInt(1) ;
	twoIntPt = MakeInt(2) ;
	threeIntPt = MakeInt(3) ;
	maxIntPt = MakeInt(maxInt) ;
	maxUIntPt = MakeUInt(maxUInt) ;
	minIntPt = MakeInt(minInt) ;

	nanFloatEntry = LookupFloatEntry(NAN) ;
	nanFloatPt = MakePermFloat(NAN) ;
	infFloatPt = MakePermFloat(INFINITY) ;
	ninfFloatPt = MakePermFloat(-INFINITY) ;
	eFloatPt = MakePermFloat(2.7182818284590452353602874713526625L) ;
	piFloatPt = MakePermFloat(3.1415926535897932384626433832795029L) ;
	epsilonFloatPt = MakePermFloat(CalcEpsilon()) ;

	InstallCBuiltinPred("floats", 0, PFloats) ;
	InstallCBuiltinPred("floats_check", 0, PCheckFloats) ;
}
