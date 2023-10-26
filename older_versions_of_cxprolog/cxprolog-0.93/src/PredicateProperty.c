/*
 *   This file is part of the CxProlog system

 *   PredicateProperty.c
 *   by A.Miguel Dias - 2005/07/30
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

#define PredIsUserDefined(pr)	( !PredIsUndefined(pr) && !PredIsBuiltin(pr) )
#define PredIsLocal(pr)			( PredIsUserDefined(pr)	&& !PredIsExtern(pr) )

static Bool GetPredIsBuiltin(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredIsBuiltin(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsUserDefined(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredIsUserDefined(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsLocal(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredIsLocal(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsStatic(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = !PredIsDynamic(pr) && !PredIsUndefined(pr) && !PredIsImported(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsDynamic(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredIsDynamic(pr) && PredIsLogical(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsDynamicIU(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredIsDynamic(pr) && !PredIsLogical(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredImportedFrom(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = ppImported && PredIsImported(pr) ;
	if( t == nil || !res ) return res ;
	else {
		Pt it = ZPushTerm(GetImportTerm(pr)) ; /* stacks may grow */
		return Unify(t, MakeUnStruct(LookupFunctor(atom, 1), it)) ;
	}
}

static Bool GetPredIsVisible(PredicatePt pr, AtomPt atom, Pt t)
{ 
	Bool res = ppVisible && PredIsUserDefined(pr) && PredIsVisible(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsGOpen(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = ppGOpen && PredIsUserDefined(pr) && PredIsVisible(pr) && PredIsOpen(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}	

static Bool GetPredIsGClose(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = ppGClose && PredIsUserDefined(pr) && PredIsVisible(pr) && !PredIsOpen(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsPrivate(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = ppPrivate && PredIsUserDefined(pr) && !PredIsVisible(pr) ;
	return  t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredIsMultifile(PredicatePt pr, AtomPt atom, Pt t)
{ 
	Bool res = PredIsMultifile(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredHasSource(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredKeepSource(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

static Bool GetPredFile(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = !PredIsUndefined(pr) && PredConsultFile(pr) != nil ;
	if( t == nil || !res ) return res ;
	else
		return Unify(t, MakeUnStruct(LookupFunctor(atom, 1),
						TagAtom(PredConsultFile(pr)))) ;
}

static Bool GetNumberOfClauses(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredIsLocal(pr) || PredIsDynamic(pr) ;
	return t == nil
			? res
			: res && Unify(t, MakeUnStruct(LookupFunctor(atom, 1),
											MakeInt(PredLength(pr)))) ;
}

static Bool GetFullName(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = !PredWasAbolished(pr) ;
	return t == nil
			? res
			: res && Unify(t, MakeUnStruct(LookupFunctor(atom, 1),
											PredNameArityTerm(pr))) ;
}

static Bool GetPredIsUndefined(PredicatePt pr, AtomPt atom, Pt t)
{
	Bool res = PredIsUndefined(pr) && !PredWasAbolished(pr) ;
	return t == nil ? res : res && Unify(t, TagAtom(atom)) ;
}

typedef struct {
	CharPt name ;
	Bool (*fun)(PredicatePt, AtomPt, Pt) ;
	AtomPt atom ;
} PredProperty, *PredPropertyPt ;

static PredProperty predProperties[] = {
	{ "builtin",			GetPredIsBuiltin },
	{ "user_defined",		GetPredIsUserDefined },
	{ "local",				GetPredIsLocal },
	{ "static",				GetPredIsStatic },
	{ "dynamic",			GetPredIsDynamic },
	{ "dynamic_iu",			GetPredIsDynamicIU },
	{ "imported_from",		GetPredImportedFrom },
	{ "visible",			GetPredIsVisible },
	{ "gopen",				GetPredIsGOpen },
	{ "gclose",				GetPredIsGClose },
	{ "private",			GetPredIsPrivate },
	{ "multifile",			GetPredIsMultifile },
	{ "source",				GetPredHasSource },
	{ "file",				GetPredFile },
	{ "undefined",			GetPredIsUndefined },
	{ "full_name",			GetFullName },
	{ "number_of_clauses",	GetNumberOfClauses },
	{ nil}
} ;

static PredPropertyPt propBuiltin, propUndefined ;

void WritePredicateProperties(StreamPt srm, PredicatePt pr)
{
	StreamWrite(srm, "%s", PredNameArity(pr)) ;
	if( PredIsC(pr) )
		StreamWrite(srm, " - c-builtin") ;
	elif( GetPredIsBuiltin(pr, nil, nil) )
		StreamWrite(srm, " - builtin") ;
	if( PredIsMeta(pr) )
		StreamWrite(srm, " - meta") ;		
	if( GetPredIsUserDefined(pr, nil, nil) )
		StreamWrite(srm, " - user_defined") ;
	if( GetPredIsLocal(pr, nil, nil) )
		StreamWrite(srm, " - local") ;
	if( GetPredImportedFrom(pr, nil, nil) )
		StreamWrite(srm, " - imported_from(%s)", TermAsStr(GetImportTerm(pr))) ;
	if( GetPredIsVisible(pr, nil, nil) )
		StreamWrite(srm, " - visible") ;
	if( GetPredIsGOpen(pr, nil, nil) )
		StreamWrite(srm, " - gopen") ;
	if( GetPredIsGClose(pr, nil, nil) )
		StreamWrite(srm, " - gclose") ;
	if( GetPredIsPrivate(pr, nil, nil) )
		StreamWrite(srm, " - private") ;
	if( GetPredIsStatic(pr, nil, nil) )
		StreamWrite(srm, " - static") ;
	if( GetPredIsDynamic(pr, nil, nil) )
		StreamWrite(srm, " - dynamic") ;
	if( GetPredIsDynamicIU(pr, nil, nil) )
		StreamWrite(srm, " - dynamic_iu") ;
	if( GetPredIsMultifile(pr, nil, nil) )
		StreamWrite(srm, " - multifile") ;
	if( GetPredHasSource(pr, nil, nil) )
		StreamWrite(srm, " - source") ;
	if( GetPredFile(pr, nil, nil) )
		StreamWrite(srm, " - file('%s')", AtomName(PredConsultFile(pr))) ;
	if( GetNumberOfClauses(pr, nil, nil) ) {
		int n = PredLength(pr) ;
		StreamWrite(srm, " - %d clause%s", n, n == 1 ? "" : "s") ;
	}
	if( GetPredIsUndefined(pr, nil, nil) )
		StreamWrite(srm, " - undefined") ;
	if( PredWasAbolished(pr) )
		StreamWrite(srm, " - abolished") ;
	StreamWrite(srm, "\n\n") ;
}

static PredPropertyPt GetProperty(Pt t)
{
	AtomPt a ;
	PredPropertyPt prop ;
	if( (a = XTermAtomOrNil(t)) == nil ) return nil ;
	for( prop = predProperties ; prop->name != nil ; prop++ )
		if( prop->atom == a ) return prop ;
	return nil ;
}

static Bool CallProperty(PredPropertyPt prop, PredicatePt pr, Pt t)
{
	return prop->fun(pr, prop->atom, X1) ;
}

static void InitPredProperties(void)
{
	PredPropertyPt prop ;
	for( prop = predProperties ; prop->name != nil ; prop++ )
		prop->atom = LookupAtom(prop->name) ;
	propBuiltin = GetProperty(MakeAtom("builtin")) ;
	propUndefined = GetProperty(MakeAtom("undefined")) ;
}



/* CXPROLOG C'BUILTINS */

static void PPredicateProperty()
{
	if( A(2) == tNilAtom ) {			/* init */
		X0 = Drf(X0) ;
		X1 = Drf(X1) ;
		if( IsVar(X0) && IsVar(X1) ) {	/* var-var init */
			A(2) = zeroIntPt ;
			A(3) = cPt(UnitPreds(CurrUnit())) ;
			A(4) = cPt(predProperties) ;
		}	
		elif( IsVar(X0) ) {				/* var-prop init */
			A(2) = oneIntPt ;
			A(4) = cPt(GetProperty(X1)) ;
			if( A(4) == nil ) Jump(DiscardAndFail) ;
			if( Eq(A(4), propBuiltin) )
				A(3) = cPt(UnitPreds(systemUnit)) ;	/* only the builtins */
			else
				A(3) = cPt(UnitPreds(CurrUnit())) ;
		}
		elif( IsVar(X1) ) {				/* pred-var init */
			A(2) = twoIntPt ;
			A(3) = cPt(FindPredicate(XTestFunctor(X0))) ;
			A(4) = cPt(predProperties) ;
			if( A(3) == nil ) Jump(DiscardAndFail) ;
		}
		else {									/* pred-prop handle */
			PredicatePt pr = FindPredicate(XTestFunctor(X0)) ;
			PredPropertyPt prop = GetProperty(X1) ;
			Discard() ;
			MustBe( pr != nil && prop != nil && CallProperty(prop, pr, X1) ) ;
		}
	}
	
	if( A(2) == twoIntPt ) {					/* pred-var handle */
		PredicatePt pr = cPredicatePt(A(3)) ;
		PredPropertyPt prop = (PredPropertyPt)(A(4)) ;
		for( ; prop->name != nil ; prop++ )
			if( CallProperty(prop, pr, X1) ) {
				A(4) = cPt(prop+1) ;
				JumpNext() ;
			}
		Jump(DiscardAndFail) ;
	}

	if( A(2) == oneIntPt ) {					/* var-prop handle */
		PredicatePt pr = cPredicatePt(A(3)) ;
		PredPropertyPt prop = (PredPropertyPt)(A(4)) ;
		doseq(pr, pr, PredNextU(pr))
			if( prop == propUndefined || !PredHiddenInGeneration(pr) )
				if( CallProperty(prop, pr, X1) ) break ;
		if( pr == nil ) Jump(DiscardAndFail) ;
		A(3) = cPt(PredNextU(pr)) ;
		MustBe( Unify(X0, MakeCleanStruct(PredFunctor(pr))) ) ;
	}

	if( A(2) == zeroIntPt ) {					/* var-var handle */
		PredicatePt pr = cPredicatePt(A(3)) ;
		PredPropertyPt prop = (PredPropertyPt)(A(4)) ;
		doseq(pr, pr, PredNextU(pr))
			if( !PredHiddenInGeneration(pr) ) {
				for( ; prop->name != nil ; prop++ ) {
					if( CallProperty(prop, pr, X1) ) {
						A(3) = cPt(pr) ;
						A(4) = cPt(prop+1) ;
						MustBe( Unify(X0, MakeCleanStruct(PredFunctor(pr))) ) ;
					}
				}
				prop = predProperties ;
			}
		Jump(DiscardAndFail) ;
	}
}

static void PNDCurrentPredicate()
{
	PredicatePt pr ;
	if( A(1) == tNilAtom ) {			/* init */
		X0 = Drf(X0) ;
		if( IsVar(X0) )
			A(1) = minusOneIntPt ;		/* var-var init */
		else {
			Pt a0, a1 ;
			XTestSlashArgs(X0, &a0, &a1) ;
			if( IsVar(a0) && IsVar(a1) )
				A(1) = minusOneIntPt ;	/* var-var init */
			elif( IsVar(a0) )
				A(1) = a1 ;				/* var-int init */
			elif( IsVar(a1) )
				A(1) = a0 ;				/* atom-var init */
			else {								/* atom-int handle */
				PredicatePt pr = FindPredicateInUnit(CurrUnit(), XTestSlash(X0)) ;
				Discard() ;
				MustBe( pr != nil && !PredHiddenInGeneration(pr) ) ;
			}
		}
		A(2) = cPt(UnitPreds(CurrUnit())) ;			
	}
	
	pr = cPredicatePt(A(2)) ;
	if( A(1) == minusOneIntPt ) {				/* var-var handle */
		doseq(pr, pr, PredNextU(pr))
			if( !PredHiddenInGeneration(pr) ) break ;
	}
	elif( IsAtom(A(1)) ) {						/* atom-var handle */
		AtomPt at = XAtom(A(1)) ;
		doseq(pr, pr, PredNextU(pr))
			if( PredAtom(pr) == at && !PredHiddenInGeneration(pr) ) break ;
	}
	else {										/* var-int handle */
		int n = XInt(A(1)) ;
		doseq(pr, pr, PredNextU(pr))
			if( PredArity(pr) == n && !PredHiddenInGeneration(pr) ) break ;
	}
	if( pr == nil ) Jump(DiscardAndFail) ;
	A(2) = cPt(PredNextU(pr)) ;
	MustBe( Unify(X0, MakeSlashTerm(PredFunctor(pr))) ) ;
}

static void PNDCurrentPredicate2()
{
	PredicatePt pr ;
	if( A(2) == tNilAtom ) {			/* init */
		X0 = Drf(X0) ;
		X1 = Drf(X1) ;
		if( IsVar(X0) && IsVar(X1) )	/* var-var init */
			A(2) = zeroIntPt ;
		elif( IsVar(X1) )				/* atom-var init */
			A(2) = cPt(XTestAtom(X0)) ;
		else {									/* var_or_atom-pred handle */
			PredicatePt pr = FindPredicateInUnit(CurrUnit(), XTestFunctor(X1)) ;
			Discard() ;
			MustBe( pr != nil && !PredHiddenInGeneration(pr)
					&& UnifyWithAtomic(X0, TagAtom(PredAtom(pr))) ) ;
		}
		A(3) = cPt(UnitPreds(CurrUnit())) ;
	}

	pr = cPredicatePt(A(3)) ;
	if( A(2) == zeroIntPt ) {					/* var-var handle */
		doseq(pr, pr, PredNextU(pr))
			if( !PredHiddenInGeneration(pr) ) break ;
	}
	else {										/* atom-var handle */
		AtomPt at = cAtomPt(A(2)) ;
		doseq(pr, pr, PredNextU(pr))
			if( PredAtom(pr) == at && !PredHiddenInGeneration(pr) ) break ;
	}
	if( pr == nil ) Jump(DiscardAndFail) ;
	A(3) = cPt(PredNextU(pr)) ;
	MustBe( UnifyWithAtomic(X0, TagAtom(PredAtom(pr)))
		 && Unify(X1, MakeCleanStruct(PredFunctor(pr))) ) ;
}

void PredicatePropertyInit()
{
	InitPredProperties() ;
	InstallGNDeterCBuiltinPred("predicate_property", 2, 3, PPredicateProperty) ;
	InstallGNDeterCBuiltinPred("current_predicate", 1, 2, PNDCurrentPredicate) ;
	InstallGNDeterCBuiltinPred("current_predicate", 2, 2, PNDCurrentPredicate2) ;
}
