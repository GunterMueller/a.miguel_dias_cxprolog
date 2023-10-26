/*
 *   This file is part of the CxProlog system

 *   Consult.c
 *   by A.Miguel Dias - 2005/08/27
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

static StackPt consultStack ;
static Bool oldReconsulting ;
AtomPt consultFile ;
Word16 consultGen = 0 ;

Bool PredHandleConsult(PredicatePt pr, Bool propChange)
{
	if( oldConsult_flag ) {
		if( oldReconsulting && PredConsultGen(pr) != consultGen ) {
			AbolishPredicate(pr, true) ;
		/* In case the flag oldConsult_flag keeps changing */
			PredConsultFile(pr) = ConsultFile() ;
			return true ;
		}
    }

	elif( Consulting() && PredConsultGen(pr) != consultGen ) {
		Bool undef = PredIsUndefined(pr) ;
		AtomPt prev = PredConsultFile(pr) ;
		if( PredIsMultifile(pr) ) {
			if( propChange )
				DatabaseError("Cannot change property of multifile predicate '%s'",
								PredNameArity(pr)) ;
			return false ;
		}
		AbolishPredicate(pr, false) ;
		PredConsultFile(pr) = ConsultFile() ;
		if( !undef ) {
			if( prev == nil )
				Warning("Redefined '%s' (previously defined at the top level)",
															PredNameArity(pr)) ;
			else
				Warning("Redefined '%s' (previously loaded from '%s')",
											PredNameArity(pr), AtomName(prev)) ;
		}
		return true ;
 	}

	if( PredIsUndefined(pr) )
		PredConsultFile(pr) = ConsultFile() ;
	return false ;
}

static void ZBasicLoadStream(StreamPt srm)
{				/* Only suppports assertion of clauses, not commands */
    Pt t ;
	AtomPt saveConsultFile = consultFile ;
	Bool saveOldReconsulting = oldReconsulting ;
	consultFile = nil ;
	oldReconsulting = false ;
	HSave() ;
	while( (t = ZReadTerm(srm)) != tEofAtom ) {
        if( t == nil )
            FatalError("Installing special builtin predicate") ;
        else SetNewClause(t, true, true) ;
        HRestore() ;
    }
	StreamClose(srm) ;
	consultFile = saveConsultFile ;
	oldReconsulting = saveOldReconsulting ;
}

void ZBasicLoadFile(CharPt fileName)
{
	ZBasicLoadStream(FileStreamOpen(fileName, mRead, nil)) ;
}

void ZBasicLoadStr(CharPt str)
{
	ZBasicLoadStream(StringStreamOpen(str)) ;
}



/* CXPROLOG C'BUILTINS */

static void PEnterConsult()
{
	register UnitPt u ;
	register PredicatePt pr ;
	register ClausePt cl ;
	StreamPt consultStream = XTestStream(X0, mRead) ;
	consultFile = StreamPath(consultStream) ;
	oldReconsulting = XTestBool(X1) ;
	if( oldReconsulting && !oldConsult_flag )
		InternalError("PEnterConsult") ;
	AtomIsPermanent(consultFile) = true ;
	StackPush(consultStack, X1) ;
	StackPush(consultStack, X0) ;

/* Increments generation clock and handles overflow if it occurs */
	if( ++consultGen == 0 ) {	/* Overflow */
		doseq(u, unitList, UnitNext(u))
			doseq(pr, UnitPreds(u), PredNextU(pr))
				PredConsultGen(pr) = 0 ;
		consultGen = 1 ;
	}

	if( oldConsult_flag || consultStream == userIn )
		/* nothing */ ;
	else {
			/* Delete all clauses from the current consult file */
		doseq(u, unitList, UnitNext(u))
			doseq(pr, UnitPreds(u), PredNextU(pr))
				if( PredIsMultifile(pr) ) {
					doseq(cl, PredClauses(pr), ClauseNext(cl))
						if( ClauseConsultFile(cl) == consultFile )
							DeleteClause(cl) ;
				}
				elif( PredConsultFile(pr) == consultFile )
					AbolishPredicate(pr, false) ;
	}
	JumpNext() ;
}

static void PExitConsult()
{
	Pt t ;
	if( !StackPop(consultStack) || !StackPop(consultStack) )
		Error("Too many '$exit_consult'") ;
	if( StackTop(consultStack, &t) ) {
		consultFile	= StreamPath(XTestStream(t, mRead)) ;
		StackFromTop(consultStack, 1, &t) ;
		oldReconsulting = XTestBool(t) ;
	}
	else {
		consultFile	= nil ;
		oldReconsulting = false ;
	}
	JumpNext() ;
}

static void PConsultClause()
{
	SetNewClause(X0, true, true) ;
	JumpNext() ;
}

static void PConsulting()
{
	Pt t ;
	MustBe( StackTop(consultStack, &t) && Unify(X0, t) ) ;
}

static void PCheckConsult()
{
	if( XTestBool(X0) && !oldConsult_flag )
		Error("Reconsult not available in ISO-Prolog") ;
	JumpNext() ;
}

void ConsultRestart()
{
	StackClear(consultStack) ;
	consultFile = nil ;
	oldReconsulting = false ;
}

void ConsultInit()
{
	consultStack = StackNew() ;
	ExtraSetHidden(consultStack) ;
	ExtraSetPermanent(consultStack) ;
	ConsultRestart() ;

	InstallCBuiltinPred("$$_enter_consult", 2, PEnterConsult) ;
	InstallCBuiltinPred("$exit_consult", 0, PExitConsult) ;
	InstallCBuiltinPred("consulting", 1, PConsulting) ;
	InstallCBuiltinPred("$consult_clause", 1, PConsultClause) ;
	InstallCBuiltinPred("$$_check_consult", 1, PCheckConsult) ;
}
