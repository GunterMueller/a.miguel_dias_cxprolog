/*
 *   This file is part of the CxProlog system

 *   Term.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Term_
#define _Term_

#if ULONG_MAX <= 0xFFFFFFFFL	/* 32 bit machine */
/**********************************************************************************/
/*	TAGS:	var		0.............................00
 *	32BITS	extra	0.............................01
 *			struct	0.............................10
 *			list	0.............................11
 *			atom	1.............................00
 *			text	1.............................01
 *			int		1.............................10
 *			real	1.............................11
 */
#define tagVar				cWord(0x00000000)
#define tagExtra			cWord(0x00000001)
#define tagStruct			cWord(0x00000002)
#define tagList				cWord(0x00000003)
#define tagAtom				cWord(0x80000000)
#define tagText				cWord(0x80000001)
#define tagInt				cWord(0x80000002)
#define tagReal				cWord(0x80000003)

#define tags3				cWord(0x80000003)
#define tags2				cWord(0x80000002)
#define tags1				cWord(0x80000000)
#define intSign				cWord(0x40000000)

#define EncodeInt(t)		((t) << 2)
#define DecodeInt(t)		((t) >> 2)
#define EncodeRealIEEE(m)	{ m.asWord = (m.asWord >> 3) << 2 ; }
#define DecodeRealIEEE(m)	{ m.asWord = (m.asWord >> 2) << 3 ; }
#define EncodeRealNoIEEE(m)	{ m.asShorts.hiHalf >>= 3 ; m.asWord <<= 2 ; }
#define DecodeRealNoIEEE(m)	{ m.asWord >>= 2 ; m.asShorts.hiHalf <<= 3 ; }

#define IsAtomic(t)			( cInt(t) < 0 )

#else	/* 64 bit machine */
/**********************************************************************************/
/*	TAGS:	var		000.............................................................
 *	64BITS	extra	001.............................................................
 *			struct	010.............................................................
 *			list	011.............................................................
 *			atom	100.............................................................
 *			text	101.............................................................
 *			int		110.............................................................
 *			real	111.............................................................
 */
#define tagVar				cWord(0x0000000000000000)
#define tagExtra			cWord(0x2000000000000000)
#define tagStruct			cWord(0x4000000000000000)
#define tagList				cWord(0x6000000000000000)
#define tagAtom				cWord(0x8000000000000000)
#define tagText				cWord(0xA000000000000000)
#define tagInt				cWord(0xC000000000000000)
#define tagReal				cWord(0xE000000000000000)

#define tags3				cWord(0xE000000000000000)
#define tags2				cWord(0xC000000000000000)
#define tags1				cWord(0x8000000000000000)
#define intSign				cWord(0x1000000000000000)

#define EncodeInt(t)		(t)
#define DecodeInt(t)		(t)
#define EncodeRealIEEE(m)	{ m.asWord >>= 3 ; }
#define DecodeRealIEEE(m)	{ m.asWord <<= 3 ; }
#define EncodeRealNoIEEE(m)	{ m.asInts.hiHalf >>= 3 ; }
#define DecodeRealNoIEEE(m)	{ m.asInts.hiHalf <<= 3 ; }

#define IsAtomic(t)			( cInt(t) < 0 )

#endif
/**********************************************************************************/

#define GetTag(t)			( cWord(t) & tags3 )
#define ClearTag(t)			( cWord(t) & ~tags3 )
#define XPt(t)				cPt( cWord(t) & (~tags3 | tagVar) )
#define	XHdl(t)				cHdl(XPt(t))
#define GetTag2(t)			( cWord(t) & tags2 )
#define GetTag1(t)			( cWord(t) & tags1 )



/* ATOM & TEXT */

#define IsAtom(t)			( GetTag(t) == tagAtom )
#define TagAtom(atom)		cPt( cWord(atom) | tagAtom )
#define MakeAtom(name)		TagAtom(LookupAtom(name))
#define	XAtom(t)			cAtomPt(XPt(t))

#define IsText(t)			( GetTag(t) == tagText )
#define TagText(tx)			cPt( (cWord(tx) - sizeof(Atom)) | tagText )
#define UntagText(t)		cPt( cWord(XPt(t)) + sizeof(Atom) )
#define TextToAtom(tx)		MakeAtom(XAtomOrTextName(tx))

#define XAtomOrTextName(t)	AtomName(XAtom(t))
#define XAtomOrTextAsAtom(t) XAtom( IsAtom(t) ? (t) : TextToAtom(t) )



/* VAR */

typedef Hdl Var ;

#define IsVar(t)			( GetTag(t) == tagVar )
#define XVar(t)				cVar(t)

#define cVar(t)				( (Var)(t) )
#define VarContents(v)		( *XVar(v) )
#define IsLink(v)			( VarContents(v) != v )
#define SetVar(v,t)			cPt( VarContents(v) = cPt(t) )
#define DrfVar(v)			( (v) = VarContents(v) )

#define Lt(v1,v2)			( cPt(v1) <  cPt(v2) )
#define Le(v1,v2)			( cPt(v1) <= cPt(v2) )
#define Gt(v1,v2)			( cPt(v1) >  cPt(v2) )
#define Ge(v1,v2)			( cPt(v1) >= cPt(v2) )
#define Eq(v1,v2)			( cPt(v1) == cPt(v2) )
#define Ne(v1,v2)			( cPt(v1) != cPt(v2) )

/* Parameter of ResetVar is not a Pt, is a proper Var */
#define ResetVar(v)			( v = cPt(&(v)) )
#define PushVar(p)			( ResetVar(*p), cPt(p++) )

#define VarValue(t)			while( IsVar(t) && IsLink(t) ) DrfVar(t)
#define VarValue2(t,i)		{ t = i ; VarValue(t) ; }

#define VarValuePre(t)		while( IsVar(t) && IsLink(t) && IsVar(VarContents(t)) ) \
								DrfVar(t)

Pt Drf(Pt t) ;
Pt MakeVar(void) ;
CharPt VarName(Pt t) ;
Bool IsVarName(CharPt s) ;



/* INT */

#define IsInt(t)			( GetTag(t) == tagInt )
#define TagInt(l)			cPt( cWord(l) | tagInt )

#define maxInt				cInt((~cWord(0))>>4)
#define minInt				cInt(~cWord(maxInt))
#define XInt(t)				cInt( cWord(t) & intSign			\
								? DecodeInt(cWord(t)) | minInt	\
								: DecodeInt(cWord(t)) & maxInt )
#define MakeInt(i)			TagInt( ClearTag(EncodeInt(cWord(i))) )

#define IsNat(t)			( IsInt(t) && XInt(t) >= 0 )
#define IsPos(t)			( IsInt(t) && XInt(t) > 0 )
#define IsByte(t)			( IsNat(t) && XInt(t) <= 255 )



/* REAL */

#define IsReal(t)			( GetTag(t) == tagReal )
#define TagReal(l)			cPt( cWord(l) | tagReal )

void InitReals(void) ;
Pt MakeReal(Real r) ;
Real XReal(Pt p) ;



/* STRUCTS */

typedef struct Struct
{
	FunctorPt functor ;		/* Struct functor */
/*	Pt args[] ;	*/			/* Formal arguments */
} Struct, *StructPt ;


#define cStructPt(s)		((StructPt)(s))
#define	XStruct(t)			cStructPt(XPt(t))

#define IsStruct(t)			( GetTag(t) == tagStruct )
#define TagStruct(st)		cPt( cWord(st) | tagStruct )

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
#define IsUnitParam(t)		( XStructFunctor(t) == unitParamFunctor )

Pt MakeStruct(FunctorPt functor, Hdl args) ;
Pt MakeCleanStruct(FunctorPt functor) ;
Pt MakeUnStruct(FunctorPt functor, Pt arg) ;
Pt MakeBinStruct(FunctorPt functor, Pt arg0, Pt arg1) ;
CharPt StructNameArity(StructPt st) ;
void SplitClauseTerm(Pt c, Pt *h, Pt *t) ;
Int XUnitParam(Pt t) ;



/* LISTS */

typedef struct List
{
	Pt head, tail ;			/* List components */
} List, *ListPt ;

#define cListPt(l)			((ListPt)(l))
#define	XList(t)			cListPt(XPt(t))

#define IsList(t)			( GetTag(t) == tagList )
#define TagList(l)			cPt( cWord(l) | tagList )

#define ListHead(l)			(l)->head
#define ListTail(l)			(l)->tail
#define ListArgs(l)			cHdl(l)
#define ListArg(l,i)		ListArgs(l)[i]

#define XListHead(t)		ListHead(XList(t))
#define XListTail(t)		ListTail(XList(t))
#define XListArgs(t)		ListArgs(XList(t))
#define XListArg(t,i)		ListArg(XList(t),i)

Pt MakeList(Pt h, Pt t) ;
Pt ArrayToOpenList(Hdl array, int n) ;
Pt ArrayToList(Hdl array, int n) ;
Hdl ListToArray(Pt list, int *len) ;
int ListLength(Pt list) ;
int KeyListLength(Pt list) ;



/* EXTRA : (extra types) */

/* subTags for extra primitive types.  0 < subTag < 256 */
#define threadSubTag	33
#define queueSubTag		34
#define stackSubTag		35
#define dictSubTag		36
#define streamSubTag	37

#define TagExtra(x)			cPt( cWord(x) | tagExtra )
#define IsExtra(t)			( GetTag(t) == tagExtra )
#define XExtraSubTag(t)		(XPt(t)[0])

CharPt XExtraTypeStr(Pt t) ; 
CharPt XExtraAsStr(Pt t) ;
Pt MakeExtraFromStr(CharPt s) ;
Pt MakeExtraPermanent(Pt t) ;
Bool EqExtra(Pt t1, Pt t2) ;



/* TERMS */

#ifndef IsRecord
#define IsRecord(t)			( GetTag2(t) == tagStruct )
#endif
#ifndef IsAtomic
#define IsAtomic(t)			( GetTag1(t) == tagAtom )
#endif
#ifndef IsAtomOrText
#define IsAtomOrText(t)		( GetTag2(t) == tagAtom )
#endif
#ifndef IsNumber
#define IsNumber(t)			( GetTag2(t) == tagInt )
#endif
#define TermArity(t)		( IsStruct(t) ? XStructArity(t) :\
							  IsList(t) ? 2 : 0 )
#define TermArgs(t)			( IsStruct(t) ? XStructArgs(t) :\
							  IsList(t) ? XListArgs(t) : nil )
#define TermName(t)			( IsStruct(t) ? XStructName(t) :\
							  IsList(t) ? "." :\
							  IsAtomOrText(t) ? XAtomOrTextName(t) :\
							  "OTHER_TERM" )
Pt StringToPString(CharPt s) ;
CharPt PConcatString(Pt list) ;
CharPt TermTypeStr(Pt t) ;
long TermSize(Pt term) ;
Pt AllocateTermForAssert(Pt term) ;
Pt AllocateTermForAssign(Pt term) ;
void ReleaseTerm(Pt term) ;
Pt PushTerm(Pt term) ;

AtomPt XTestAtom(Pt t) ;
CharPt XTestAtomName(Pt t) ;
Int XTestInt(Pt t) ;
Int XTestPosInt(Pt t) ;
Int XTestNat(Pt t) ;
Bool XTestBool(Pt t) ;
Pt XTestFlag(Pt t) ;
Pt XTestVar(Pt t) ;
Pt XTestNonVar(Pt t) ;
FunctorPt XTestFunctor(Pt t) ;
FunctorPt XTestFunctor2(Pt t1, Pt t2) ;

void InitTerms(void) ;

#endif
