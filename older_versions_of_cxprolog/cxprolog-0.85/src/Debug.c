/*
 *   This file is part of the CxProlog system

 *   Debug.c
 *   by A.Miguel Dias - 2000/05/05
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL

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

typedef enum { eCall, eNext, eFail, eExit, eRedo, eCut } DebugEvent ;
typedef enum { mNeutral, mSkip, mQuasiSkip, mLeap, mInterrupt } DebugMode ;

typedef struct DebugFrame /* LIVES ON LOCAL STACK */
{
	Pt exitInst ;   /* first */
	Pt redoInst ;   /* second */
	Pt retryInst ;  /* third */
	Pt /*DebugEvent*/ event ;	/* Beware of garbage collection */
	Pt /*char*/ type ;
	Pt lineN ;
	PredicatePt pred ;
	Hdl callCP ;
	Pt callC ;
	Pt callCH ;
	Hdl redoTR ; /* can move */
	Hdl redoP ;
	Pt frameN ;
	ClausePt currClause ;
	struct DebugFrame *father ;
} DebugFrame, *DebugFramePt ;

#define cDebugFramePt(p)		((DebugFramePt)(p))

static DebugEvent topLeash = eNext ;
static DebugFramePt DD ;
static DebugMode mode ;
static Pt skippingTo ;
static Size DDN ;
static Bool interruptActivated = false ;
static Size lineN ;

static void DebugReset()
{
	DD = nil ;
	DDN = 1 ;
	lineN = 1 ;
	mode = mNeutral ;
}

static void DoDebuggerRellocForStacks(Size globalOffset, Size localOffset)
{
	if( DD != nil )
		DD = cVoidPt(cPt(DD) + localOffset) ;
	if( IsLocalRef(P) )
		P = cVoidPt(cPt(P) + localOffset) ;
	if( IsLocalRef(CP) ) {
		CP = cVoidPt(cPt(CP) + localOffset) ;
	}
}

static ChoicePointPt DF2CP(DebugFramePt d)
{
	return cChoicePointPt(cPt(d) - PredArity(d->pred)) - 1 ;
}

static DebugFramePt CP2DF(ChoicePointPt b)
{
	return IsDebugCP(b) ? cDebugFramePt(b->P - 2) : nil ;
}

static CharPt EventName(DebugEvent e)
{
	switch( e ) {
		case eCall: return "call" ;
		case eNext: return "next" ;
		case eFail: return "fail" ;
		case eExit: return "exit" ;
		case eRedo: return "redo" ;
		case eCut:  return " cut" ;		
		default: Default("EventName") ;
	}
	return nil ; /* Avoids warning */
}

static DebugEvent StrToTopLeash(CharPt s)
{
	switch( s[0] ) {
		case 'o': if( EqualStr(s, "off") ) return eCall - 1 ;
		case 'l': if( EqualStr(s, "loose") ) return eCall ;
		case 'h': if( EqualStr(s, "half") ) return eNext ;
		case 't': if( EqualStr(s, "tight") ) return eFail ;
		case 'f': if( EqualStr(s, "full") ) return eExit ;
	}
	return eCall - 2 ;
}

static CharPt TopLeashToStr(DebugEvent e)
{
	switch( e ) {
		case eCall - 1: return "off" ;
		case eCall: return "loose (call)" ;
		case eNext: return "half (call, next)" ;
		case eFail: return "tight (call, next, fail)" ;
		case eExit: return "full (call, next, fail, exit)" ;
		default: Default("WriteLeash") ; return nil ;
	}
}

static void Spaces(int n)
{
	while( n-- )
		WriteStd(" ") ;
}

static CharPt GoalAsStr(DebugFramePt d)
{
	CharPt s ;
	HSave() ;
	s = TermAsStr(MakeStruct(PredFunctor(d->pred), cHdl(DF2CP(d) + 1))) ;
	HRestore() ;
	return s ;
}

static Bool FindDebugFrame(int n, DebugFramePt *res)
{
	register ChoicePointPt b ;
	DebugFramePt d ;
	Pt fn = MakeInt(n) ;
	for( b = B ; Lt(b, stacksEnd) ; b = b->B )
		if( (d = CP2DF(b)) != nil && d->frameN == fn ) {
			*res = d ;
			return true ;
		}
	WriteStd("No such box.\n") ;
	return false ;
}

static char CountRemainingClauses(DebugFramePt d)
{
	register ClausePt c ;
	register int n ;
	if( XInt(d->type) != '#' ) return XInt(d->type) ;
	for( n = 0, c = d->currClause ; c != nil && n < 10 ;
								c = ClauseNext(c), n++ ) ;
	return n <= 9 ? '0' + n : XInt(d->type) ;
}

static void WriteGoalInfo(DebugFramePt d)
{
	WriteStd("     GOAL: %s\n", GoalAsStr(d)) ;
	WriteStd("     PRED: %s ", UPredNameArity(d->pred)) ;
	if( XInt(d->type) == 'b' ) WriteStd("{builtin}\n") ;
	elif( XInt(d->type) == 'i' ) WriteStd("{imported from unit '%s'}\n",
									TermAsStr(GetImportTerm(d->pred))) ;
	elif( XInt(d->type) == 'u' ) WriteStd("{undefined in unit '%s'}\n",
									UnitSignature(CurrUnit())) ;
	else WriteStd("{has %ld clauses}\n",
									NumberOfClauses(d->pred)) ;
	WriteStd("  CONTEXT: %s\n", TermAsStr(d->callC)) ;
	WriteStd(" HCONTEXT: %s\n", TermAsStr(d->callCH)) ;
}

static Size NumberOfAncestors(DebugFramePt d)
{
	register Size n ;
	for( n = 0, d = d->father ; d != nil ; d = d->father )
		n++ ;
	return n  ;
}

static void WriteAncestors(DebugFramePt d, int m)
{
	int n ;
	for( d = d->father, n = 0 ; d != nil && n < m ; d = d->father, n++ )
		WriteStd("(%3ld) %s\n", XInt(d->frameN), GoalAsStr(d)) ;
	if( n == 0 ) WriteStd("No ancestors.\n") ;
}

static Bool DebugLine(DebugEvent e, DebugFramePt d, Bool leash)
{
	d->lineN = MakeInt(++lineN) ;
	Spaces(NumberOfAncestors(d)) ;
	WriteStd("%c%c%c (%3ld) %s: ",
		PredIsSpy(d->pred) ? '*' : ' ',
		leash ? '>' : ' ',
		CountRemainingClauses(d),
		XInt(d->frameN),
		EventName(e)
	) ;
	WriteStd("%s: ", TermAsStr(d->callC)) ;
	WriteStd("%s", GoalAsStr(d)) ;
	if( e <= topLeash || leash || PredIsSpy(d->pred) )
		{ WriteStd(" ? ") ; return true ; }
	else { WriteStd("\n") ; return false ; }
}

static Bool InterDebug(DebugEvent e, DebugFramePt d)
{
	int comm, arg ;
	Bool leash ;

	if( debug_flag == 0 ) return false ;
	d->event = MakeInt(e) ;
	switch( mode ) {
		case mNeutral: {
			leash = false ;
			break ;
		}
		case mSkip: {
			if( d->frameN != skippingTo ) return false ;
			mode = mNeutral ;
			leash = true ;
			break ;
		}
		case mQuasiSkip: {
			if( d->frameN != skippingTo && !PredIsSpy(d->pred) )
				return false ;
			mode = mNeutral ;
			leash = true ;
			break ;
		}
		case mLeap: {
			if( !PredIsSpy(d->pred) ) return false ;
			mode = mNeutral ;
			leash = true ;
			break ;
		}
		case mInterrupt: {		/* Interrupt forces leashing */
			mode = mNeutral ;
			leash = true ;
			break ;
		}
	}

	for(;;) {
		if( !DebugLine(e, d, leash) ) return false ;
		GetCharCommand(&comm, &arg) ;
		mode = mNeutral ;		/* Ignore possible interrupt on input */
		switch( comm ) {
			case '\n': {				/* creep */
				return false ;
			}
			case 'n': {					/* nodebug */
				DebugUpdateFlags(0) ;
				return false ;
			}
			case 'a': {					/* abort */
				WriteStd("Execution aborted.\n") ;
				EventRestart() ;
				return true ;
			}
			case 'e': {					/* exit */
				WriteStd("Bye.\n") ;
				EventExit() ;
				return true ;
			}
			case 'l': {					/* leap */
				mode = mLeap ;
				return false ;
			}
			case 's': {					/* skip */
				if( arg >= 0 && !FindDebugFrame(arg, &d) ) break ;
				mode = mSkip ;
				skippingTo = d->frameN ;
				return false ;
			}
			case 'q': {					/* quasi-skip */
				if( arg >= 0 && !FindDebugFrame(arg, &d) ) break ;
				mode = mQuasiSkip ;
				skippingTo = d->frameN ;
				return false ;
			}
			case 'r': {					/* retry */
				if( arg >= 0 && !FindDebugFrame(arg, &d) ) break ;
				SetChoicePoint(DF2CP(d)) ;
				RestoreState(PredArity(d->pred)) ;
				DD = d->father ;
				DDN = XInt(d->frameN) ;
				SetChoicePoint(Bf(B)) ;	/* Discard */
				if( !DebugCall(d->pred, false) )
					InternalError("InterDebug") ;
				return true ;
			}
			case 'f': {					/* fail */
				if( arg >= 0 && !FindDebugFrame(arg, &d) ) break ;
				SetChoicePoint(DF2CP(d)) ;
				d->currClause = nil ;
				P = &Fail ;		/* Fail */
				return true ;
			}
			case 'i': {					/* info */
				WriteGoalInfo(d) ;
				break ;
			}
			case 'g': {					/* ancestors */
				WriteAncestors(d, arg < 0 ? 30000 : arg ) ;
				break ;
			}
			case ':': {					/* statistics */
				StatisticsShow() ;
				break ;
			}
			case '=': {					/* debugging */
				Debugging() ;
				break ;
			}
			case '+': {					/* spy this */
				SpyOn(PredFunctor(d->pred)) ;
				break ;
			}
			case '-': {					/* nospy this */
				SpyOff(PredFunctor(d->pred)) ;
				break ;
			}
			default: {
				WriteStd("\
CxProlog debug options:\n\
      <ret> creep            l leap             h help         \n\
       s<i> skip             + spy this         i info         \n\
       q<i> quasi-skip       - nospy this    g<n> ancestors    \n\
       r<i> retry            n nodebug          = debugging    \n\
       f<i> fail             a abort            : statistics   \n\
                             e exit \n") ;
/* b   break     !@ command */
				break ;
			}
		}
	}
}

/* CP[-1] remains undefined across every predicate activated in
   debug mode. However this is not a problem as the predicate debug
   choice point always covers the environment which size would need
   to be determined using CP[-1] */

Bool DebugCall(PredicatePt pr, Bool visReq)
{
	register DebugFramePt d ;
	ChoicePointPt saveB ;

	if( debug_flag == 0 ) return false ;
	Attention() = true ;	/* keep attention alive */

	switch( mode ) {
		case mNeutral:		break ;
		case mSkip:			return false ;
		case mQuasiSkip:
		case mLeap:			if( !PredIsSpy(pr) ) return false ;
							else break ;
		case mInterrupt:	break ;
	}
	if( !PredIsTraceable(pr) ) return false ;

	if( DD == nil ) {
		if( PredIsBuiltin(pr) ) return false ;
		if( debug_flag == 1 && !PredIsSpy(pr) && mode != mInterrupt )
			return false ;
	}
	elif( XInt(DD->type) == 'b' && PredIsBuiltin(pr) ) return false ;

	d = cDebugFramePt(TopOfLocalStack()) - 1 ;	/* top */
	d->exitInst = DebugExit ;
	d->redoInst = DebugRedo ;
	d->retryInst = DebugRetry ;
	d->pred = pr ;
	d->callCP = CP ;
	d->callC = C ;
	d->callCH = CH ;
	d->lineN = MakeInt(0) ;
	d->currClause = PredClauses(pr) ;
	d->frameN = MakeInt(DDN++) ;
	d->type = MakeInt(
			  PredIsBuiltin(pr) ? 'b'
			: PredIsImported(pr) ? 'i'
			: PredIsUndefined(pr) ? 'u'
			: '#') ;	
	d->father = DD ;
	DD = d ;

	saveB = B ;
	B0 = B = DF2CP(d) ; /* ChoicePoint covers DebugFrame */
	SaveState(saveB, &(d->retryInst), PredArity(d->pred)) ;
	if( InterDebug(eCall, d) ) return true ;

	CP = &(d->exitInst) ;
	P = d->currClause == nil
			? PredCode(pr)
			: ClauseCodeSkipHeader(d->currClause) ;

	return true ;
}

void DebugCut()
{
	DebugFramePt d = cDebugFramePt(Bf(P) - 2) ;	/* top */
	d->currClause = nil ;
	InterDebug(eCut, d) ;
}

void DebugExitCode()
{
	DebugFramePt d ;	/* not top */
	d = cDebugFramePt(P - 1) ;	/* not top */
	if( InterDebug(eExit, d) ) return ;
	d->redoTR = TR ;
	d->redoP = Bf(P) ;
	Bf(P) = &(d->redoInst) ;
	P = CP = d->callCP ;	/* Proceed */
	DD = d->father ;
}

void DebugRedoCode()
{
	DebugFramePt d = cDebugFramePt(P - 2) ;	/* not top */
	saveTR = d->redoTR ;
	TrailRestore() ;
	Bf(P) = d->redoP ;
	P = &Fail ;				/* Fail */
	if( InterDebug(eRedo, d) ) return ;
}

void DebugRetryCode()
{
	DebugFramePt d = cDebugFramePt(P - 3) ;	/* top */
	RestoreState(PredArity(d->pred)) ;
	if( d->currClause != nil )
		d->currClause = ClauseNext(d->currClause) ;
	if( d->currClause == nil ) {
		if( InterDebug(eFail, d) ) return ;
		SetChoicePoint(Bf(B)) ;	/* Discard */
		P = &Fail ;				/* Fail */
		DD = nil ;
	}
	else {
		if( (XInt(d->event) == eCall
		 || XInt(d->event) == eNext) && XInt(d->lineN) == lineN )
			/* hides shallow backtracking */ ;
		elif( InterDebug(eNext, d) ) return ;
		CP = &(d->exitInst) ;
		P = ClauseCodeSkipHeader(d->currClause) ;
		DD = d ;
	}
}

void DebugUpdateFlags(int newValue)
{
	int oldValue = debug_flag ;
	debug_flag = newValue ;
	if( oldValue != newValue ) {
		if( newValue == 0 )
			if( oldValue == 1 ) WriteStd(" [debug mode off] \n") ;
			else WriteStd(" [trace mode off] \n") ;
		elif( newValue == 1 ) WriteStd(" [debug mode on] \n") ;
		else WriteStd(" [trace mode on] \n") ;
	}
	if( (oldValue == 0) != (newValue == 0) ) {
		if( newValue == 0 ) NoSpyAll() ;
		else Attention() = true ;
		DebugReset() ;
	}
}

void DebugInterrupt(int newValue) /* Called from InterruptHandle() */
{
	if( debug_flag == 0 && newValue > 0 )
		interruptActivated = true ;
	DebugUpdateFlags(newValue) ;
	mode = mInterrupt ;
}

void DebugRestart()		/* Activated on cxprolog restart */
{
	DebugReset() ;
	if( interruptActivated ) {	/* Debug off, if interrupt active */
		DebugUpdateFlags(0) ;
		interruptActivated = false ;
	}
}

void Debugging()
{
	VersionShow() ;
	Write("Debugging:\n") ;

	if( debug_flag == 0 ) {
		WriteStd("    ") ;
		WriteStd("Debug mode is off.\n") ;
	}
	else {
		WriteStd("    ") ;
		WriteStd("Debug mode is on.\n") ;
		WriteStd("    ") ;
		WriteSpyPoints() ;
		WriteStd("    ") ;
		WriteStd("Leashing set to %s.\n", TopLeashToStr(topLeash)) ;
	}
}

void DebuggerRellocForStacks(Size globalOffset, Size localOffset)
{
	DoDebuggerRellocForStacks(globalOffset, localOffset) ;
}


/* CXPROLOG C'BUILTINS */

static void PDebugging()
{
	Debugging() ;
	JumpNext()
}

static void PLeash()
{
	DebugEvent e ;
	if( (e = StrToTopLeash(XTestAtomName(X0))) < eCall - 1 )
		Error("Invalid leash specification") ;
	topLeash = e ;
	WriteStd("Leashing set to %s.\n", TopLeashToStr(topLeash)) ;
	JumpNext()
}

static void PSpy()
{
	ForEachInSpec(X0, SpyOn) ;
	if( debug_flag == 0 )
		DebugUpdateFlags(1) ;
	JumpNext()
}

static void PNoSpy()
{
	ForEachInSpec(X0, SpyOff) ;
	JumpNext()
}

static void PNoSpyAll()
{
	NoSpyAll() ;
	JumpNext()
}

void DebugInit()
{
	DebugReset() ;
	InstallCBuiltinPred("debugging", 0, PDebugging) ;
	InstallCBuiltinPred("leash", 1, PLeash) ;
	InstallCBuiltinPred("spy", 1, PSpy) ;
	InstallCBuiltinPred("nospy", 1, PNoSpy) ;
	InstallCBuiltinPred("nospyall", 0, PNoSpyAll) ;

	PredIsTraceable(LookupPredicateByName(",", 2)) = false ;
}
