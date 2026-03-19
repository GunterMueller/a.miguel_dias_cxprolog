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

/*************************************************
 * TAGS: var	0.............................00 *
 *		 extra	0.............................01 *
 *		 struct	0.............................10 *
 *		 list	0.............................11 *
 *		 atom	1.............................00 *
 *		 real	1.............................01 *
 *		 int	1.............................10 *
 *		 bigint	1.............................11 *
 *************************************************/

#if WORD_SIZE == 32
  #define tagVar			cWord(0x00000000)
  #define tagExtra			cWord(0x00000001)
  #define tagStruct			cWord(0x00000002)
  #define tagList			cWord(0x00000003)
  #define tagAtom			cWord(0x80000000)
  #define tagReal			cWord(0x80000001)
  #define tagInt			cWord(0x80000002)
  #define tagBigInt			cWord(0x80000003)

  #define tags1				cWord(0x80000000)
  #define tags2				cWord(0x80000002)
  #define tags3				cWord(0x80000003)
#endif

#if WORD_SIZE == 64
  #define tagVar			cWord(0x0000000000000000)
  #define tagExtra			cWord(0x0000000000000001)
  #define tagStruct			cWord(0x0000000000000002)
  #define tagList			cWord(0x0000000000000003)
  #define tagAtom			cWord(0x8000000000000000)
  #define tagText			cWord(0x8000000000000001)
  #define tagInt			cWord(0x8000000000000002)
  #define tagBigInt			cWord(0xC000000000000002)
  #define tagReal			cWord(0x8000000000000003)

  #define tags1				cWord(0x8000000000000000)
  #define tags2				cWord(0x8000000000000002)
  #define tags3				cWord(0x8000000000000003)
#endif

#define GetTag1(t)			( cWord(t) & tags1 )
#define GetTag2(t)			( cWord(t) & tags2 )
#define GetTag3(t)			( cWord(t) & tags3 )
#define ClearTag3(t)		( cWord(t) & ~tags3 )

#define GetTag(t)			GetTag3(t)
#define ClearTag(t)			ClearTag3(t)
#define XPt(t)				cPt( ClearTag(t) )
#define	XHdl(t)				cHdl( ClearTag(t) )

#define EncodeBits(t)		(cWord(t) << 2)
#define DecodeBits(t)		(cWord(t) >> 2)
#define PackBits(t)			((t) >> 3)
#define UnpackBits(t)		((t) << 3)



/* ATOM & TEXT */

#define IsAtomic(t)			( GetTag1(t) != 0 )

#define IsAtomOrText(t)		( GetTag(t) == tagAtom )

#define IsAtom(t)			( GetTag(t) == tagAtom )
#define TagAtom(atom)		cPt( cWord(atom) | tagAtom )
#define MakeAtom(name)		TagAtom(LookupAtom(name))
#define	XAtom(t)			cAtomPt(XPt(t))

/*#define IsText(t)			( GetTag(t) == tagText )*/
#define tagText				cWord(0)
#define IsText(t)			false
#define TagText(tx)			cPt( (cWord(tx) - sizeof(Atom)) | tagText )
#define UntagText(t)		cPt( cWord(XPt(t)) + sizeof(Atom) )
#define TextToAtom(tx)		MakeAtom(XAtomOrTextName(tx))

#define XAtomOrTextName(t)	AtomName(XAtom(t))
#define XAtomOrTextAsAtom(t) XAtom( IsAtom(t) ? (t) : TextToAtom(t) )



/* VAR */

#define IsVar(t)			( GetTag(t) == tagVar )

#define VarContents(v)		( *cHdl(v) )
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
#define ResetVar(v)			( *cHdl(v) = cPt(v) )
#define PushVar(p)			( ResetVar(p), cPt(p++) )

#define VarValue(t)			while( IsVar(t) && IsLink(t) ) DrfVar(t)
#define VarValue2(t,i)		{ t = i ; VarValue(t) ; }

#define VarValuePre(t)		while( IsVar(t) && IsLink(t) && IsVar(VarContents(t)) ) \
								DrfVar(t)

Pt Drf(Pt t) ;
Pt MakeVar(void) ;
CharPt VarName(Pt t) ;
Bool IsVarName(CharPt s) ;



/* INT */

#define IsNumber(t)			( IsInt(t) || IsReal(t) )

#define IsInt(t)			( GetTag2(t) == tagInt )
#define TagInt(l)			cPt( cWord(l) | tagInt )

#define IsBigInt(t)			( GetTag(t) == tagBigInt )
#define TagBigInt(l)		cPt( cWord(l) | tagBigInt )



/* REAL */

#define IsReal(t)			( GetTag(t) == tagReal )
#define TagReal(l)			cPt( cWord(l) | tagReal )



/* STRUCTS */

typedef struct Struct
{
	FunctorPt functor ;		/* Struct functor */
/*	Pt args[] ;	*/			/* Formal arguments */
} Struct, *StructPt ;


#define cStructPt(s)		((StructPt)(s))
#define	XStruct(t)			cStructPt(XPt(t))

#define IsRecord(t)			( GetTag2(t) == tagStruct )

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
int XUnitParam(Pt t) ;



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
Pt ArrayToOpenList(Hdl array, Size n) ;
Pt ArrayToList(Hdl array, Size n) ;
Hdl ListToArray(Pt list, Size *len) ;
Size ListLength(Pt list) ;
Size KeyListLength(Pt list) ;



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

CharPt XExtraAsStr(Pt t) ;
Pt MakeExtraFromStr(CharPt s) ;



/* TERMS */

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
Size TermSize(Pt term) ;
Pt AllocateTermForAssert(Pt term) ;
Pt AllocateTermForAssign(Pt term) ;
void ReleaseTerm(Pt term) ;
Pt PushTerm(Pt term) ;

AtomPt XTestAtom(Pt t) ;
CharPt XTestAtomName(Pt t) ;
BigInt XTestInt(Pt t) ;
BigInt XTestPosInt(Pt t) ;
BigInt XTestNat(Pt t) ;
double XTestReal(Pt t) ;
Bool XTestBool(Pt t) ;
Pt XTestFlag(Pt t) ;
Pt XTestVar(Pt t) ;
Pt XTestNonVar(Pt t) ;
FunctorPt XTestFunctor(Pt t) ;
FunctorPt XTestFunctor2(Pt t1, Pt t2) ;

void InitTerms(void) ;

#endif
