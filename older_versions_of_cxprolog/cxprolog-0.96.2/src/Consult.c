/*
 *   This file is part of the CxProlog system

 *   Consult.c
 *   by A.Miguel Dias - 2005/08/27
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL

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
static QueuePt initializeQueue ;
static Bool oldReconsulting ;
static Bool silentConsulting ;
AtomPt consultFile ;
Word16 consultGen = 0 ;	/* consult generation */

Bool PredHandleConsult(PredicatePt pr, Bool propChange)
{
	if( oldConsult_flag ) {
		if( oldReconsulting && PredConsultGen(pr) != consultGen ) {
			AbolishPredicate(pr, true) ;
		/* In case the flag oldConsult_flag keeps changing... */
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
			FatalError("Installing special built-in predicate") ;
		else AddNewClause(t, true, true, false) ;
		HRestore() ;
	}
	consultFile = saveConsultFile ;
	oldReconsulting = saveOldReconsulting ;
}

void ZBasicLoadFile(CharPt fileName)
{
	StreamPt srm = FileStreamOpen(fileName, mRead, nil) ;
	ZBasicLoadStream(srm) ;
	StreamClose(srm, nil) ;
}

void ZBasicLoadStr(CharPt str)
{
	StreamPt srm = StringStreamOpen(str) ;
	ZBasicLoadStream(srm) ;
	StreamClose(srm, nil) ;
}



/* CXPROLOG C'BUILTINS */

static void PEnterConsult()
{
	register UnitPt u ;
	register PredicatePt pr ;
	register ClausePt cl, nx ;
	StreamPt consultStream = XTestStream(X0, mRead) ;
	consultFile = StreamPath(consultStream) ;
	oldReconsulting = XTestBool(X1) ;
	if( XTestBool(X2) || !infoMessages_flag ) /* force silent consulting? */
		silentConsulting = true ;
	else
		silentConsulting = silentConsulting ;	/* does not change */
	if( oldReconsulting && !oldConsult_flag )
		InternalError("PEnterConsult") ;
	ExtraPermanent(consultFile) ;
	StackPush(consultStack, MakeBool(silentConsulting)) ;
	StackPush(consultStack, X1) ;
	StackPush(consultStack, X0) ;
	QueuePut(initializeQueue, tMarkAtom) ;

/* Increments generation clock and handles overflow if it occurs */
	if( ++consultGen == 0 ) {	/* Handles overflow */
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
					doseq(cl, PredClauses(pr), nx) {
						nx = ClauseNext(cl) ;
						if( ClauseConsultFile(cl) == consultFile )
							DeleteClause(cl) ;
					}
				}
				elif( PredConsultFile(pr) == consultFile )
					AbolishPredicate(pr, false) ;
	}
	JumpNext() ;
}

static void PExitConsult()
{
	Pt t ;
	if( !StackPop(consultStack) || !StackPop(consultStack) || !StackPop(consultStack) )
		Error("Too many '$exit_consult'") ;
	if( StackTop(consultStack, &t) ) {
		consultFile	= StreamPath(XTestStream(t, mRead)) ;
		StackFromTop(consultStack, 1, &t) ;
		oldReconsulting = XTestBool(t) ;
		StackFromTop(consultStack, 2, &t) ;
		silentConsulting = XTestBool(t) ;
	}
	else {
		consultFile	= nil ;
		oldReconsulting = false ;
		silentConsulting = false ;
	}
	JumpNext() ;
}

static void PEnterInclude()
{
	StreamPt includeStream ;
	if( consultFile == nil )
		Error("This directive can only be used in consult") ;
	includeStream = XTestStream(X0, mRead) ;
	consultFile = StreamPath(includeStream) ;
	oldReconsulting = oldReconsulting ;		/* does not change */
	silentConsulting = silentConsulting ;	/* does not change */
	ExtraPermanent(consultFile) ;
	StackPush(consultStack, MakeBool(silentConsulting)) ;
	StackPush(consultStack, MakeBool(oldReconsulting)) ;
	StackPush(consultStack, X0) ;
	JumpNext() ;
}

static void PConsulting()
{
	Pt t ;
	MustBe( StackTop(consultStack, &t) && Unify(X0, t) ) ;
}

static void PConsultingIsSilent()
{
	MustBe( silentConsulting ) ;
}

static void PConsultClause()
{
	AddNewClause(X0, true, true, false) ;
	JumpNext() ;
}

static void PConsultStoreInit()
{
	if( consultFile == nil )
		Error("This directive can only be used in consult") ;
	QueuePut(initializeQueue, X0) ;
	JumpNext() ;
}

static void PConsultGetInitList()
{
	Pt t = ZGetQueueFrontSectionAsList(initializeQueue, tMarkAtom) ;
	MustBe( t != nil && Unify(X0, t) ) ;
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
	QueueClear(initializeQueue) ;
	consultFile = nil ;
	oldReconsulting = false ;
	silentConsulting = false ;
}

void ConsultInit()
{
	consultStack = StackNew() ;
	ExtraHide(consultStack) ;
	ExtraPermanent(consultStack) ;
	initializeQueue = QueueNew() ;
	ExtraHide(initializeQueue) ;
	ExtraPermanent(initializeQueue) ;
	ConsultRestart() ;

	InstallCBuiltinPred("$$_enter_consult", 3, PEnterConsult) ;
	InstallCBuiltinPred("$$_exit_consult", 0, PExitConsult) ;
	InstallCBuiltinPred("$$_enter_include", 1, PEnterInclude) ;
	InstallCBuiltinPred("$$_exit_include", 0, PExitConsult) ;
	InstallCBuiltinPred("consulting", 1, PConsulting) ;
	InstallCBuiltinPred("$$_consult_is_silent", 0, PConsultingIsSilent) ;
	InstallCBuiltinPred("$consult_clause", 1, PConsultClause) ;
	InstallCBuiltinPred("$$_consult_store_initialization", 1, PConsultStoreInit) ;
	InstallCBuiltinPred("$$_consult_get_initialization_list", 1, PConsultGetInitList) ;
	InstallCBuiltinPred("$$_check_consult", 1, PCheckConsult) ;
}
