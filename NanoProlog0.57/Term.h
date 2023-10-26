/*
 *   This file is part of the NanoProlog system

 *   Term.h
 *   by A.Miguel Dias - 89/11/14
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

991230: Added new tag 'extra' 
931203: Tags for 64 bit machines and for machines that reserve the sign bit.
 		 IsAtomic and IsNumber was optimized.
931117: release of version 0.5

*/

#ifndef _Term_
#define _Term_

#ifndef _NanoProlog_
#include "NanoProlog.h"
#endif

/* ATENTION: vars are represented by UNMODIFIED addresses.
		If you want to add new definitions of tags be carefull with "tagVar" */

/**********************************************************************************/
#if tagSet == 1
/*	TAGS:	var		000.............................
 *	32BITS	struct 	001.............................
 *			list	010.............................
 *			extra	011.............................
 *			atom	100.............................
 *			spare	101.............................
 *			int		110.............................
 *			real	111.............................
 */
#define tagVar				cWord(0x00000000)
#define tagStruct			cWord(0x20000000)
#define tagList				cWord(0x40000000)
#define tagExtra			cWord(0x60000000)
#define tagAtom				cWord(0x80000000)
#define tagSpare			cWord(0xA0000000)
#define tagInt				cWord(0xC0000000)
#define tagReal				cWord(0xE0000000)

#define allTags				cWord(0xE0000000)
#define intSign				cWord(0x10000000)

#define EncodeInt(t)		t
#define DecodeInt(t)		t
#if ieeeFloats
#define EncodeReal(m)		{ m.asWord >>= 3 ; }
#define DecodeReal(m)		{ m.asWord <<= 3 ; }
#else
#define EncodeReal(m)		{ m.asShorts.hiHalf >>= 3 ; }
#define DecodeReal(m)		{ m.asShorts.hiHalf <<= 3 ; }
#endif

#define IsAtomic(t)			( cInt(t) < 0 )
#define IsNumber(t)			( cWord(t) >= tagInt )
#endif
/**********************************************************************************/
#if tagSet == 2
/*	TAGS:	var		0.............................00
 *	32BITS	struct 	0.............................01
 *			list	0.............................10
 *			extra	0.............................11
 *			atom	1.............................00
 *			spare	1.............................01
 *			int		1.............................10
 *			real	1.............................11
 */	
#define tagVar				cWord(0x00000000)
#define tagStruct			cWord(0x00000001)
#define tagList				cWord(0x00000002)
#define tagExtra			cWord(0x00000003)
#define tagAtom				cWord(0x80000000)
#define tagSpare			cWord(0x80000001)
#define tagInt				cWord(0x80000002)
#define tagReal				cWord(0x80000003)

#define allTags				cWord(0x80000003)
#define intSign				cWord(0x40000000)

#define EncodeInt(t)		t << 2
#define DecodeInt(t)		t >> 2
#if ieeeFloats
#define EncodeReal(m)		{ m.asWord = (m.asWord >> 3) << 2 ; }
#define DecodeReal(m)		{ m.asWord <<= 1 ; }
#else
#define EncodeReal(m)		{ m.asShorts.hiHalf >>= 3 ; m.asWord <<= 2 ; }
#define DecodeReal(m)		{ m.asWord >>= 2 ; m.asShorts.hiHalf <<= 3 ; }
#endif

#define IsAtomic(t)			( cInt(t) < 0 )
#endif
/**********************************************************************************/
#if tagSet == 3
/*	TAGS:	var		100.............................
 *	32BITS	struct 	101.............................
 *			list	110.............................
 *			extra	111.............................
 *			atom	000.............................
 *			spare	001.............................
 *			int		010.............................
 *			real	011.............................
 */
#define tagVar				cWord(0x80000000)
#define tagStruct			cWord(0xA0000000)
#define tagList				cWord(0xC0000000)
#define tagExtra			cWord(0xE0000000)
#define tagAtom				cWord(0x00000000)
#define tagSpare			cWord(0x20000000)
#define tagInt				cWord(0x40000000)
#define tagReal				cWord(0x60000000)

#define allTags				cWord(0xE0000000)
#define intSign				cWord(0x10000000)

#define EncodeInt(t)		t
#define DecodeInt(t)		t
#if ieeeFloats
#define EncodeReal(m)		{ m.asWord >>= 3 ; }
#define DecodeReal(m)		{ m.asWord <<= 3 ; }
#else
#define EncodeReal(m)		{ m.asShorts.hiHalf >>= 3 ; }
#define DecodeReal(m)		{ m.asShorts.hiHalf <<= 3 ; }
#endif

#define IsAtomic(t)			( cInt(t) >= 0 )
#endif
/**********************************************************************************/
#if tagSet == 4
/*	TAGS:	var		000.............................................................
 *	64BITS	struct 	001.............................................................
 *			list	010.............................................................
 *			extra	011.............................................................
 *			atom	100.............................................................
 *			spare	101.............................................................
 *			int		110.............................................................
 *			real	111.............................................................
 */
#define tagVar				cWord(0x0000000000000000)
#define tagStruct			cWord(0x2000000000000000)
#define tagList				cWord(0x4000000000000000)
#define tagExtra			cWord(0x6000000000000000)
#define tagAtom				cWord(0x8000000000000000)
#define tagSpare			cWord(0xA000000000000000)
#define tagInt				cWord(0xC000000000000000)
#define tagReal				cWord(0xE000000000000000)

#define allTags				cWord(0xE000000000000000)
#define intSign				cWord(0x1000000000000000)

#define EncodeInt(t)		t
#define DecodeInt(t)		t
#if ieeeFloats
#define EncodeReal(m)		{ m.asWord >>= 3 ; }
#define DecodeReal(m)		{ m.asWord <<= 3 ; }
#else
#define EncodeReal(m)		{ m.asInts.hiHalf >>= 3 ; }
#define DecodeReal(m)		{ m.asInts.hiHalf <<= 3 ; }
#endif

#define IsAtomic(t)			( cInt(t) < 0 )
#define IsNumber(t)			( cWord(t) >= tagInt )
#endif
/**********************************************************************************/
/**********************************************************************************/

#define GetTag(t)			( cWord(t) & allTags )
#define ClearTag(t)			( cWord(t) & ~allTags )
#define XPt(t)				cPt( cWord(t) & (~allTags | tagVar) )
#define	XHdl(t)				cHdl(XPt(t))

void CheckHost() ;

/* ATOM */

typedef struct Atom
{
	struct Atom *next ;				/* Next atom in the hash chain */
	struct Functor *functors ;		/* Functor list */
/*	char name[] ;	*/				/* Name string */
} Atom, *AtomPt ;

#define IsAtom(t)			( GetTag(t) == tagAtom )
#define	IsNilAtom(t)		( (t) == tNilAtom )
#define TagAtom(atom)		cPt( cWord(atom) | tagAtom )
#define MakeAtom(name)		TagAtom(LookupAtom(name))
#define	cAtomPt(a)			((AtomPt)a)
#define	XAtom(t)			cAtomPt(XPt(t))

#define AtomFunctors(a)		(a)->functors
#define AtomName(a)			cCharPt((a) + 1)
#define XAtomName(t)		AtomName(XAtom(t))

extern AtomPt nilAtom, trueAtom, eofAtom, userAtom ;
extern Pt tNilAtom, tCutAtom, tFailAtom ;

typedef void (*AtomProc)(AtomPt) ;

AtomPt LookupAtom(CharPt name) ;
void CreateInitialAtoms(void) ;
void ForEachAtom(AtomProc p) ;
void ListAtoms(void) ;



/* FUNCTOR */

typedef struct Functor
{
	struct Functor *next ;			/* Next functor of different arity */
	AtomPt atom ;					/* Functor's atom */
	struct Predicate *predicates ;	/* Predicates for this functor */
	int arity ;						/* Functor's arity */
	Bool cPred : 1 ;				/* Functor of C predicate */
	Bool isSys : 1 ;				/* Functor of system predicate */
	Bool meta : 1 ;					/* Functor of system meta predicate ",/;/not" */
} Functor, *FunctorPt ;

#define	cFunctorPt(f)		((FunctorPt)f)
#define FunctorAtom(f)		(f)->atom
#define FunctorName(f)		AtomName(FunctorAtom(f))
#define FunctorArity(f)		(f)->arity
#define FunctorPreds(f)		(f)->predicates
#define FunctorIsC(f)		(f)->cPred
#define FunctorIsSys(f)		(f)->isSys
#define FunctorIsMeta(f)	(f)->meta
#define LookupFunctor2(n,a)	LookupFunctor(LookupAtom(n), a)

extern FunctorPt commaFunctor, semicolonFunctor, neckFunctor,
		listFunctor, switchFunctor, cutFunctor ;

FunctorPt LookupFunctor(AtomPt atom, int arity) ;
CharPt FunctorNameArity(FunctorPt f) ;
void CreateInitialFunctors(void) ;

#define Lt(v1,v2)			( cPt(v1) <  cPt(v2) )
#define Le(v1,v2)			( cPt(v1) <= cPt(v2) )
#define Gt(v1,v2)			( cPt(v1) >  cPt(v2) )
#define Ge(v1,v2)			( cPt(v1) >= cPt(v2) )
#define Eq(v1,v2)			( cPt(v1) == cPt(v2) )
#define Ne(v1,v2)			( cPt(v1) != cPt(v2) )



/* VAR */

typedef Hdl Var ;

#define IsVar(t)			( GetTag(t) == tagVar )
#define XVar(t)				cVar(t)

#define cVar(t)				( (Var)(t) )
#define VarContents(v)		( *XVar(v) )
#define IsLink(v)			( VarContents(v) != v )
#define SetVar(v,t)			cPt( VarContents(v) = cPt(t) )
#define DrfVar(v)			( (v) = VarContents(v) )
#define LTVar(v1,v2)		( XVar(v1) < XVar(v2) )
#define LEVar(v1,v2)		( XVar(v1) <= XVar(v2) )

/* Argument of ResetVar is not a Pt, is a proper Var */
#define ResetVar(v)			cPt( (v) = cPt( &(v) ) )
#define PushVar(p)			( ResetVar(*p), cPt(p++) )
#define IsVarLink(t)		( IsVar(t) && IsLink(t) )

#ifndef VarValue
#define VarValue(t)			while( IsVarLink(t) ) DrfVar(t)
#endif

#define VarValue2(t,i)		{ t = i ; VarValue(t) ; }

extern Pt drf, drf0, drf1, drf2, drf3, drf4, drf5, drf6 ;

Pt Drf(Pt t) ;
Pt MakeVar(void) ;
CharPt VarName(Pt t) ;


/* INT */

#define IsInt(t)			( GetTag(t) == tagInt )
#define maxInt				cInt((~cWord(0))>>4)
#define minInt				cInt(~cWord(maxInt))
#define XInt(t)				cInt( cWord(t) & intSign			\
								? DecodeInt(cWord(t)) | minInt	\
								: DecodeInt(cWord(t)) & maxInt )
#define MakeInt(i)			cPt( ClearTag(EncodeInt(cWord(i))) | tagInt )

#define IsNat(t)			( IsInt(t) && XInt(t) >= 0 )
#define IsPos(t)			( IsInt(t) && XInt(t) > 0 )
#define IsByte(t)			( IsNat(t) && XInt(t) <= 255 )



/* REAL */

#define IsReal(t)			( GetTag(t) == tagReal )

Pt MakeReal(Real r) ;
Real XReal(Pt p) ;



/* RELLOCATE FLAG */

/*
#define TagRelloc(st)		 cHdl( cWord(t) | tagReal )
#define IsRelloc(t)			( GetTag(t) == tagReal )
#define XRelloc(t)			cHdl(ClearTag(t))
*/

#define TagRelloc(t)		cHdl( t )
#define IsRelloc(t)			( false )
#define XRelloc(t)			cHdl( t )



/* EXTRA : (extra types) */

#define TagExtra(st)		 cPt( cWord(st) | tagExtra )
#define IsExtra(t)			( GetTag(t) == tagExtra )
#define XExtraSubTag(t)		((cCharPt(XPt(t)))[0])
#define XExtra(t)			(cCharPt(XPt(t)))

Pt MakeExtra(CharPt str, int subcode) ;
Pt GroundExtra(Pt t) ;
CharPt XExtraName(Pt t) ;
Bool EqExtra(Pt t1, Pt t2) ;
void WriteExtra(Pt t) ;

// add here the subTags of the new extra primitive types
#define textSubTag	33



/* STRUCTS */

typedef struct Struct
{
	FunctorPt functor ;		/* Struct functor */
/*	Pt args[] ;	*/			/* Formal arguments */
} Struct, *StructPt ;

#define TagStruct(st)		cPt( cWord(st) | tagStruct )

#define cStructPt(s)		((StructPt)(s))
#define	XStruct(t)			cStructPt(XPt(t))

#define IsStruct(t)			( GetTag(t) == tagStruct )
#define StructFunctor(s)	(s)->functor
#define StructAtom(s)		FunctorAtom(StructFunctor(s))
#define StructName(s)		FunctorName(StructFunctor(s))
#define StructArity(s)		FunctorArity(StructFunctor(s))
#define StructArgs(s)		cHdl((s) + 1)
#define StructArg(s,i)		StructArgs(s)[i]

#define XStructFunctor(t)	StructFunctor(XStruct(t))
#define XStructAtom(t)		StructAtom(XStruct(t))
#define XStructName(t)		StructName(XStruct(t))
#define XStructArity(t)		StructArity(XStruct(t))
#define XStructArgs(t)		StructArgs(XStruct(t))
#define XStructArg(t,i)		StructArg(XStruct(t),i)
#define XStructNameArity(t)	StructNameArity(XStruct(t))

#define IsThisStruct(t,f)	( IsStruct(t) && XStructFunctor(t) == f )
#define IsSameStruct(t1,t2)	( IsStruct(t1) && IsStruct(t2) && \
							   XStructFunctor(t1) == XStructFunctor(t2) )

Pt MakeStruct(FunctorPt functor, Hdl args) ;
Pt MakeUnStruct(FunctorPt functor, Pt arg) ;
Pt MakeBinStruct(FunctorPt functor, Pt arg0, Pt arg1) ;
CharPt StructNameArity(Struct *st) ;
void SplitClauseTerm(Pt c, Pt *h, Pt *t) ;
Pt ClauseHead(Pt cl) ;



/* LISTS */

typedef struct List
{
	Pt head, tail ;			/* List components */
} List, *ListPt ;

#define TagList(l)			cPt( cWord(l) | tagList )

#define cListPt(l)			((ListPt)(l))
#define	XList(t)			cListPt(XPt(t))

#define IsList(t)			( GetTag(t) == tagList )
#define ListHead(l)			(l)->head
#define ListTail(l)			(l)->tail
#define ListArgs(l)			cHdl(l)
#define ListArg(l,i)		ListArgs(l)[i]

#define XListHead(t)		ListHead(XList(t))
#define XListTail(t)		ListTail(XList(t))
#define XListArgs(t)		ListArgs(XList(t))
#define XListArg(t,i)		ListArg(XList(t),i)

Pt MakeList(Pt h, Pt t) ;
Pt ArrayToList(int n, Hdl elems) ;
Pt ArrayToListAddNil(int n, Hdl elems) ;
int ListLength(Pt l, Bool *proper) ;
Pt PStringToAtom(Pt l) ;
Pt AtomToPString(Pt a) ;



/* TERMS */

#ifndef IsNumber
#define	IsNumber(t)			( IsInt(t) || IsReal(t) )
#endif
#ifndef IsRecord
#define	IsRecord(t)			( IsStruct(t) || IsList(t) )
#endif
#ifndef IsAtomic
#define IsAtomic(t)			( IsAtom(t) || IsNumber(t) )
#endif
#define TermArity(t)		( IsStruct(t) ? XStructArity(t) :\
							  IsList(t) ? 2 : 0 )
#define TermArgs(t)	 		( IsStruct(t) ? XStructArgs(t) :\
							  IsList(t) ? XListArgs(t) : nil )
#define TermName(t)	 		( IsStruct(t) ? XStructName(t) :\
							  IsList(t) ? "." :\
							  IsAtom(t) ? XAtomName(t) :\
							  "OTHER_TERM" )
void CheckHost(void) ;
Pt StringToPString(CharPt s) ;
CharPt PStringToString(Pt list) ;
CharPt PConcatString(Pt l) ;
CharPt TermTypeStr(Pt t) ;
Pt MakeCleanTerm(FunctorPt functor) ;
Pt CopyTermToHeap(Pt term) ;
void FreeHeapTerm(Pt term) ;
Pt CopyTermToGlobal(Pt term) ;
void TypeError(CharPt s) ;
void TypeCheck(CharPt types) ;

#endif