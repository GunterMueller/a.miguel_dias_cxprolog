/*
 *   This file is part of the CxProlog system

 *   Term.h
 *   by A.Miguel Dias - 1989/11/14
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Term_
#define _Term_

/*************************************************
 * TAGS: var	0.............................00 *
 *		 extra	0.............................01 *
 *		 struct	0.............................10 *
 *		 list	0.............................11 *
 *		 atom	1.............................00 *
 *		 int	1.............................10 *
 *		 float	1.............................11 *
 *************************************************/

#if ULONG_MAX == 0xFFFFFFFFL
/* 32-bit machine */
  #define tagVar			cWord(0x00000000)
  #define tagExtra			cWord(0x00000001)
  #define tagStruct			cWord(0x00000002)
  #define tagList			cWord(0x00000003)
  #define tagAtom			cWord(0x80000000)
  #define tagInt			cWord(0x80000002)
  #define tagFloat			cWord(0x80000003)

  #define tags1				cWord(0x80000000)
  #define tags2				cWord(0x80000002)
  #define tags3				cWord(0x80000003)
#else
/* 64-bit machine */
  #define tagVar			cWord(0x0000000000000000)
  #define tagExtra			cWord(0x0000000000000001)
  #define tagStruct			cWord(0x0000000000000002)
  #define tagList			cWord(0x0000000000000003)
  #define tagAtom			cWord(0x8000000000000000)
  #define tagInt			cWord(0x8000000000000002)
  #define tagFloat			cWord(0x8000000000000003)

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



/* ATOM */

#define IsAtomic(t)			( GetTag1(t) != 0 )
#define IsAtom(t)			( GetTag(t) == tagAtom )
#define TagAtom(atom)		cPt( cWord(atom) | tagAtom )
#define MakeAtom(name)		TagAtom(LookupAtom(name))
#define MakeTempAtom(name)	TagAtom(LookupTempAtom(name))
#define	XAtom(t)			cAtomPt(ClearTag(t))
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
Pt MakeVar(void) ;
CharPt VarName(Pt t) ;
Bool IsVarName(CharPt s) ;



/* INT */

#define IsNumber(t)			( GetTag2(t) == tagInt )

#define IsInt(t)			( GetTag(t) == tagInt )
#define TagInt(l)			cPt( cWord(l) | tagInt )



/* FLOAT */

#define IsFloat(t)			( GetTag(t) == tagFloat )
#define TagFloat(l)			cPt( cWord(l) | tagFloat )



/* STRUCTS */

#define IsRecord(t)			( GetTag2(t) == tagStruct )

#define IsStruct(t)			( GetTag(t) == tagStruct )
#define TagStruct(st)		cPt( cWord(st) | tagStruct )

#define XStructFunctor(t)	(*(FunctorPt *)XHdl(t))
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
Pt MakeList(Pt head, Pt tail) ;
Pt MakeTriStruct(FunctorPt functor, Pt arg0, Pt arg1, Pt arg2) ;
CharPt XStructNameArity(Pt t) ;
void SplitNeckTerm(Pt c, Hdl parts) ;
int XUnitParam(Pt t) ;



/* LISTS */


#define IsList(t)			( GetTag(t) == tagList )
#define TagList(l)			cPt( cWord(l) | tagList )

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



/* EXTRA : (extra types) */

#define IsExtra(t)			( GetTag(t) == tagExtra )
#define TagExtra(x)			cPt( cWord(x) | tagExtra )



/* TERMS */

#define TermArity(t)		( IsStruct(t) ? XStructArity(t) :\
							  IsList(t) ? 2 : 0 )
#define TermArgs(t)			( IsStruct(t) ? XStructArgs(t) :\
							  IsList(t) ? XListArgs(t) : nil )
#define TermName(t)			( IsStruct(t) ? XStructName(t) :\
							  IsList(t) ? "." :\
							  IsAtom(t) ? XAtomName(t) :\
							  "OTHER_TERM" )
#define termSegmSize 10
extern Hdl termSegm ;

Pt StringToPString(CharPt s) ;
CharPt TermTypeStr(Pt t) ;
void TermAtomGCMark(Pt t) ;
Size TermSize(Pt t) ;
Pt AllocateTermForAssert(Pt t) ;
Pt AllocateTermForAssign(Pt t) ;
void ReleaseTerm(Pt t) ;
Pt ZPushTerm(Pt t) ;
Pt ZPushTerm_ConvUnitParams(Pt t) ;

Pt TestAtom(Pt t) ;
AtomPt XTestAtom(Pt t) ;
CharPt XTestAtomName(Pt t) ;
PInt XTestInt(Pt t) ;
PInt XTestPosInt(Pt t) ;
PInt XTestNat(Pt t) ;
PInt XTestCode(Pt t) ;
PInt XTestByte(Pt t) ;
PInt XTestChar(Pt t) ;
PInt XTestIntRange(Pt t, int a, int z) ;
PFloat XTestFloat(Pt t) ;
Bool XTestBool(Pt t) ;
Bool XTestOnOff(Pt t) ;
Pt XTestVar(Pt t) ;
Pt XTestNonVar(Pt t) ;
FunctorPt XTestFunctor(Pt t) ;
FunctorPt XTestFunctor2(Pt t1, Pt t2) ;
FunctorPt XTestStruct(register Pt t, Hdl *args) ;

void TermsInit(void) ;

#endif
