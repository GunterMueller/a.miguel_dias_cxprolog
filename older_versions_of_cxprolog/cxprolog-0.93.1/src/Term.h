/*
 *   This file is part of the CxProlog system

 *   Term.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */
#ifndef _Term_
#define _Term_

#if !BACKWARDS
/** FORWARDS *************************************
 * TAGS: var	0.............................00 *
 *		 struct	0.............................10 *
 *		 list	0.............................11 *
 *		 extra	1.............................00 *
 *		 atom	1.............................01 *
 *		 int	1.............................10 *
 *		 float	1.............................11 *
 *************************************************/

#if ULONG_MAX == 0xFFFFFFFFL
/* 32-bit machine */
  #define tagVar			cWord(0x00000000)
  #define tagStruct			cWord(0x00000002)
  #define tagList			cWord(0x00000003)
  #define tagExtra			cWord(0x80000000)
  #define tagAtom			cWord(0x80000001)
  #define tagInt			cWord(0x80000002)
  #define tagIntNeg			cWord(0xc0000002)
  #define tagFloat			cWord(0x80000003)

  #define tag1				cWord(0x80000000)
  #define tag2				cWord(0x80000002)
  #define tag3				cWord(0x80000003)
  #define tag4				cWord(0xc0000003)
#else
/* 64-bit machine */
  #define tagVar			cWord(0x0000000000000000)
  #define tagStruct			cWord(0x0000000000000002)
  #define tagList			cWord(0x0000000000000003)
  #define tagExtra			cWord(0x8000000000000000)
  #define tagAtom			cWord(0x8000000000000001)
  #define tagInt			cWord(0x8000000000000002)
  #define tagIntNeg			cWord(0xc000000000000002)
  #define tagFloat			cWord(0x8000000000000003)

  #define tag1				cWord(0x8000000000000000)
  #define tag2				cWord(0x8000000000000002)
  #define tag3				cWord(0x8000000000000003)
  #define tag4				cWord(0xc000000000000003)
#endif
#else
/** BACKWARDS ************************************
 * TAGS: var	1.............................00 *
 *		 struct	1.............................10 *
 *		 list	1.............................11 *
 *		 extra	0.............................00 *
 *		 atom	0.............................01 *
 *		 int	0.............................10 *
 *		 float	0.............................11 *
 *************************************************/

#if ULONG_MAX == 0xFFFFFFFFL
/* 32-bit machine */
  #define tagVar			cWord(0x80000000)
  #define tagStruct			cWord(0x80000002)
  #define tagList			cWord(0x80000003)
  #define tagExtra			cWord(0x00000000)
  #define tagAtom			cWord(0x00000001)
  #define tagInt			cWord(0x00000002)
  #define tagIntNeg			cWord(0x40000002)
  #define tagFloat			cWord(0x00000003)

  #define tag1				cWord(0x80000000)
  #define tag2				cWord(0x80000002)
  #define tag3				cWord(0x80000003)
  #define tag4				cWord(0xc0000003)
#else
/* 64-bit machine */
  #define tagVar			cWord(0x8000000000000000)
  #define tagStruct			cWord(0x8000000000000002)
  #define tagList			cWord(0x8000000000000003)
  #define tagExtra			cWord(0x0000000000000000)
  #define tagAtom			cWord(0x0000000000000001)
  #define tagInt			cWord(0x0000000000000002)
  #define tagIntNeg			cWord(0x4000000000000002)
  #define tagFloat			cWord(0x0000000000000003)

  #define tag1				cWord(0x8000000000000000)
  #define tag2				cWord(0x8000000000000002)
  #define tag3				cWord(0x8000000000000003)
  #define tag4				cWord(0xc000000000000003)
#endif
#endif

#define GetTag1(t)			( cWord(t) & tag1 )
#define GetTag2(t)			( cWord(t) & tag2 )
#define GetTag3(t)			( cWord(t) & tag3 )
#define GetTag4(t)			( cWord(t) & tag4 )

#define ClearTag1(t)		( cWord(t) & ~tag1 )
#define ClearTag3(t)		( cWord(t) & ~tag3 )

#define GetTag(t)			GetTag3(t)
#define ClearTag(t)			ClearTag3(t)

#define XPt(t)				cPt( ClearTag(t) | tagVar )
#define	XHdl(t)				cHdl( XPt(t) )

#define TagA(pt,tag)		cPt( ClearTag1(pt) | tag )
#define TagR(pt,tag)		cPt( cWord(pt) | tag )

#define EncodeBits(t)		( cWord(t) << 2 )
#define DecodeBits(t)		( cWord(t) >> 2 )


/* GENERAL */

#define IsAtomic(t)			( GetTag1(t) == tagExtra )
#define IsNumber(t)			( GetTag2(t) == tagInt )
#define IsCompound(t)		( GetTag2(t) == tagStruct )


/* EMPTY */

#define EmptyCell			( cPt(0) )
#define IsEmpty(t)			( cPt(t) == EmptyCell )

void EmptyRange(Hdl a, Hdl z) ;
void EmptyRangeN(Hdl a, Size n) ;
void CopyUntilEmpty(Hdl z, Hdl a) ;
Hdl FindEmpty(Hdl a) ;
VoidPt PrimitiveAllocateEmpty(Size nWords, VoidPt last) ;


/* ATOM */

#define IsAtom(t)			( GetTag(t) == tagAtom )
#define TagAtom(atom)		TagA(atom, tagAtom)
#define MakeAtom(name)		TagAtom(LookupAtom(name))
#define MakeTempAtom(name)	TagAtom(LookupTempAtom(name))
#define	XAtom(t)			cAtomPt( XPt(t) )
#define XAtomName(t)		AtomName(XAtom(t))


/* VAR */

#define IsVar(t)			( GetTag(t) == tagVar )

#define IsLink(v)			( *cHdl(v) != cPt(v) )
#define SetVar(v,t)			( *cHdl(v) = cPt(t) )

#define DrfVar(v)			( (v) = *cHdl(v) )
#define VarValue(t)			while( IsVar(t) && IsLink(t) ) DrfVar(t)
#define VarValue2(t,i)		{ t = i ; VarValue(t) ; }

#define Lt(v1,v2)			( cPt(v1) <  cPt(v2) )
#define Le(v1,v2)			( cPt(v1) <= cPt(v2) )
#define Gt(v1,v2)			( cPt(v1) >  cPt(v2) )
#define Ge(v1,v2)			( cPt(v1) >= cPt(v2) )
#define Eq(v1,v2)			( cPt(v1) == cPt(v2) )
#define Ne(v1,v2)			( cPt(v1) != cPt(v2) )
#define Df(v1,v2)			( cPt(v1) - cPt(v2) )


Pt Drf(Pt t) ;
void PrepareDrfChecked(Pt term) ;
Pt DrfChecked(Pt t) ;
Pt MakeVar(void) ;
CharPt VarName(Pt t) ;
Bool IsVarName(CharPt s) ;


/* INT */

#define IsInt(t)			( GetTag(t) == tagInt )
#define TagInt(l)			TagA(l, tagInt)

#define IsNat(t)			( GetTag4(t) == tagInt )
#define IsNeg(t)			( GetTag4(t) == tagIntNeg )


/* FLOAT */

#define IsFloat(t)			( GetTag(t) == tagFloat )
#define TagFloat(l)			TagA(l, tagFloat)


/* STRUCTS */

#define IsStruct(t)			( GetTag(t) == tagStruct )
#define TagStruct(st)		TagR(st, tagStruct)

#define XStructFunctor(t)	(*cFunctorHdl(XPt(t)))
#define XStructAtom(t)		FunctorAtom(XStructFunctor(t))
#define XStructName(t)		FunctorName(XStructFunctor(t))
#define XStructArity(t)		FunctorArity(XStructFunctor(t))
#define XStructArgs(t)		(XHdl(t) + 1)
#define XStructArg(t,i)		(XStructArgs(t)[i])

#define IsThisStruct(t,f)	( IsStruct(t) && XStructFunctor(t) == f )
#define IsSameStruct(t1,t2)	( IsStruct(t1) && IsStruct(t2) && \
								XStructFunctor(t1) == XStructFunctor(t2) )
#define IsUnitParam(t)		( IsThisStruct(t, unitParamFunctor) )

Pt MakeStruct(FunctorPt functor, Hdl args) ;
Pt MakeCleanStruct(FunctorPt functor) ;
Pt MakeUnStruct(FunctorPt functor, Pt arg) ;
Pt MakeBinStruct(FunctorPt functor, Pt arg0, Pt arg1) ;
Pt MakeSlashTerm(FunctorPt f) ;
Pt MakeList(Pt head, Pt tail) ;
Pt MakeTriStruct(FunctorPt functor, Pt arg0, Pt arg1, Pt arg2) ;
CharPt XStructNameArity(Pt t) ;
void SplitNeckTerm(Pt c, Hdl parts) ;
int XUnitParam(Pt t) ;


/* LISTS */

#define IsList(t)			( GetTag(t) == tagList )
#define TagList(l)			TagR(l, tagList)

#define XListArgs(t)		XHdl(t)
#define XListArg(t,i)		(XListArgs(t)[i])
#define XListHead(t)		XListArg(t,0)
#define XListTail(t)		XListArg(t,1)

Pt MakeList(Pt h, Pt t) ;
Pt ArrayToOpenList(Hdl array, Size n) ;
Pt ArrayToList(Hdl array, Size n) ;
Pt ArrayToListRev(Hdl array, Size n) ;
Hdl ListToArray(Pt list, Size *len) ;
Size ListLength(Pt list) ;
Bool ListCheck(Pt list) ;


/* EXTRA : (extra types) */

#define IsExtra(t)          ( GetTag(t) == tagExtra )
#define TagExtra(x)         TagA(x, tagExtra)
#define XExtra(t)           cPt( XPt(t) )
#define IsAtomicStrict(t)   ( IsAtomic(t) && !IsExtra(t) )


/* TERMS */

#define termSegmSize 10
extern Hdl termSegm ;

Pt StringToPString(CharPt s) ;
Pt StringToAString(CharPt s) ;
CharPt TermTypeStr(Pt t) ;
void TermBasicGCMark(Pt t) ;
Size TermSize(Pt t) ;
Bool HasFreeVars(Pt t) ;
Pt AllocateTermForAssert(Pt t) ;
Pt AllocateTermForAssign(Pt t) ;
void ReleaseTerm(Pt t) ;
Pt ZPushTerm(Pt t) ;
Pt ZPushTerm_ConvUnitParams(Pt t) ;

Pt TestAtomic(Pt t) ;
Pt TestAtom(Pt t) ;
Pt TestList(Pt t) ;
AtomPt XTestAtom(Pt t) ;
CharPt XTestAtomName(Pt t) ;
CharPt XTestFileName(Pt t) ;
PInt XTestInt(Pt t) ;
Pt XTestIntOrVar(Pt t) ;
LLInt XTestLLInt(Pt t) ;
PInt XTestPosInt(Pt t) ;
PInt XTestNat(Pt t) ;
PInt XTestCode(Pt t) ;
PInt XTestByte(Pt t) ;
PInt XTestChar(Pt t) ;
PInt XTestCharOrCode(Pt t) ;
PInt XTestIntRange(Pt t, int a, int z) ;
PFloat XTestFloat(Pt t) ;
Bool XTestBool(Pt t) ;
Bool XTestOnOff(Pt t) ;
int XTestAtomAlt(Pt t, ...) ;
Pt XTestVar(Pt t) ;
Pt XTestNonVar(Pt t) ;
AtomPt XTermAtomOrNil(Pt t) ;
CharPt XTestTermName(Pt t) ;
Hdl XTermArgs(Pt t, int *arity) ;
FunctorPt XTestFunctor(Pt t) ;
int XTestArity(Pt t) ;
FunctorPt XTestFunctor2(Pt t1, Pt t2) ;
FunctorPt XTestSlash(Pt t) ;
void XTestSlashArgs(Pt t, Hdl a0, Hdl a1) ;
FunctorPt XTestStruct(Pt t, Hdl *args) ;

void TermsInit(void) ;

#endif
