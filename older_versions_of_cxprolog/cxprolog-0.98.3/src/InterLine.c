/*
 *   This file is part of the CxProlog system

 *   InterLine.c
 *   by A.Miguel Dias - 2007/09/01
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


/* PROMPTS */

#define MAX_PROMPTS	8
static Str prompts[MAX_PROMPTS] = {} ;
static int nPrompts = 0 ;
static Str AUTO_POP_MARK = "AUTO_POP_MARK";

static void promptsReset(void) {
	nPrompts = 0;
}

static Bool promptsEmpty(void) {
	return nPrompts == 0;
}

static void promptsPushOne(Str prompt) {
/*	Dot("[%d]", nPrompts); */
	if( nPrompts == MAX_PROMPTS )
		FatalError("Prompts stack overflow") ;	
	if( prompt == nil ) prompt = "" ; 
	prompts[nPrompts++] = prompt ;
}

static void promptsPush(Str prompt, Bool onlyOnce) {
	promptsPushOne(prompt);
	if( onlyOnce )
		promptsPushOne(AUTO_POP_MARK);
}

static Str promptsPop(void) {
	if( nPrompts == 0 )
		FatalError("Prompts stack underflow") ;
	return prompts[--nPrompts] ;
}

static Str promptsTop(void) {
	if( nPrompts == 0 )
		FatalError("Prompts stack underflow") ;
	if( prompts[nPrompts-1] == AUTO_POP_MARK ) {
		promptsPop() ;
		return promptsPop() ;
	}
	else {
		return prompts[nPrompts-1] ;
	}
}


/*   */

static Bool atTopLevelRead = false ;

static Str InterLineGetPrompt(void)
{
	return promptsEmpty() ? "|: " : promptsTop();
}

void InterLineSetPrompts(Str main, Str cont) {
	atTopLevelRead = main != nil && cont != nil ;
	if( main == nil && cont == nil ) {
		promptsTop(); /* auto pop */
		promptsPop();
	}
	else {
		if( cont != nil )
			promptsPush(cont, false) ;
		if( main != nil )
			promptsPush(main, true) ;
	}
}

void InterLineWritePrompt(Bool forceSecondPrompt)
{
	if( forceSecondPrompt )
		InterLineGetPrompt() ;
	StreamPutStr(userOut, InterLineGetPrompt()) ;
	StreamFlush(userOut) ;	// In case of socket stream, etc.
}

Bool InterLineItAtTopLevelRead(void)
{
	return atTopLevelRead ;
}

static void InterLinePromptInit(void)
{
	promptsReset();
	atTopLevelRead = false ;
}



/* INTERACTIVE COMMAND LINE */

#if OS_UNIX && USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>

#if RL_READLINE_VERSION < 0x400
#	error "Required library: readline-4.0 or newer."
#endif

static CharPt line, linePt ;

static void r(void)
{
	line = readline(InterLineGetPrompt()) ;
}

static Bool InterLineMore(void)
{
	/* Not sure if locale should be changed. @@@ */
	RunProtected(r) ;
	if( line == nil )
		return false ;	/* EOF */
	linePt = atTopLevelRead ? ClearTermText(line, false) : line ;
	if( !atTopLevelRead  || linePt[0] == '\0' || linePt[1] == '\0' || (linePt[2] == '\0' && linePt[1] != '.') )
		/* Does not insert in history */ ;
	else {
		Bool isRepetition = history_length > 0
					&& strcmp(linePt, history_get(history_length)->line) == 0 ;
		if( !isRepetition )
			add_history(linePt) ;
	}
	return true ;
}

void InterLineChangedUserStreams(void)
{
	if( StreamKind(userIn) == interactiveFileStream
		&& StreamKind(userOut) == textFileStream ) {
		rl_instream = StreamFILE(userIn) ;
		rl_outstream = StreamFILE(userOut) ;
	}
}

static void InterLinePrepareForNextLine(void)
{
	if( line != nil )
		free(line) ;
	line = nil ;
}

void InterLineFinish(void)
{		/* Save history for next session */
	write_history(WithPreferencesDirPath("history", nil)) ;
}

static void InterLineInitialize(void)
{
	line = nil ;
	InterLinePrepareForNextLine() ;
	rl_readline_name = "cxprolog" ;		/* For cond. parsing of "~/.inputrc". */
	rl_inhibit_completion = 1 ;			/* Disable completion */
	using_history() ;					/* Initialize history */
										/* Load history from prev. session */
	read_history(WithPreferencesDirPath("history", nil)) ;
}

WChar InterLineGet(void)
{
	if( line == nil && !InterLineMore() )
		return EOF ;
	else {
		WChar c = StrGetChar(&linePt, SystemEncoding()) ;
		if( c == '\0') {
			InterLinePrepareForNextLine() ;
			return '\n' ;
		}
		else
			return c ;
	}
}

WChar InterLinePeek(void)
{
	if( line == nil && !InterLineMore() )
		return EOF ;
	else {
		CharPt save = linePt ;
		WChar c = StrGetChar(&linePt, SystemEncoding()) ;
		linePt = save ;
		return c == '\0' ? '\n' : c ;
	}

}

Str InterLineVersion(void)
{
	return GStrFormat("lib=%x, inc=%x, loc=%d",
			rl_readline_version, RL_READLINE_VERSION, USE_READLINE) ;
}

#else

static WChar cc = '\n' ;

void InterLineChangedUserStreams(void)
{
	/* Nothing */
}

static void InterLinePrepareForNextLine(void)
{
	cc = '\n' ;
}

void InterLineFinish(void)
{
	/* Do nothing */
}

static void InterLineInitialize(void)
{
	InterLinePrepareForNextLine() ;
}

static void r(void)
{
	if( cc == '\n' )
		InterLineWritePrompt(false) ;
	cc = FileGetChar(StreamChannel(userIn)) ;
}

WChar InterLineGet(void)
{
	for(;;) {
		RunProtected(r) ;
		if( cc < ' ' ) /* this works even with raw input or sockets */
			switch( cc ) {
				case EOF:
					if( InterruptHandle() ) continue ;
					return EOF ;
				case  0:
					continue ;
				case 10:
				case 13:
					return '\n' ;
				case  4: /* CNTL-D */
				case 26: /* CNTL-Z */
					do {	/* skip rest of line */
						cc = FileGetChar(StreamChannel(userIn)) ;
					} while( cc != '\n' ) ;
					return EOF ;
			}
		return cc ;
	}
}

WChar InterLinePeek(void)
{
	WChar c = FilePeekChar(StreamChannel(userIn))  ;
	if( c < ' ' ) /* this works even with raw input or sockets */
		switch( c ) {
			case 10:
			case 13:
				return '\n' ;
			case EOF:
			case  4: /* CNTL-D */
			case 26: /* CNTL-Z */
				return EOF ;
		}
	return c ;
}

Str InterLineVersion(void)
{
	return "none" ;
}
#endif

WChar InterLineGetCommand(Str prompt, int *arg)
{
	WChar c, res ;
	int n ;
	Bool hasArg ;
	InterruptOff() ;
	StreamFlush(userOut) ;
	StreamFlush(userErr) ;
	InterLinePrepareForNextLine() ;
	InterLineSetPrompts(nil, prompt) ;
/* Get command */
	while( (c = InterLineGet()) <= ' ' && c != '\n' && c != EOF ) ;
	InterLineSetPrompts(nil, nil) ;
	res = InRange(c,'A','Z') ? (c - 'A' + 'a') : c ;
/* Skip blanks */
	if( c != '\n' && c != EOF )
		while( (c = InterLineGet()) <= ' ' && c != '\n' && c != EOF ) ;
/* Read argument */
	hasArg = false ;
	n = 0 ;
	while( c != '\n' && c != EOF ) {
		if( InRange(c, '0', '9') ) {
			hasArg = true ;
			n = n * 10 + c - '0' ;
		}
		else {
			hasArg = false ;
			break ;
		}
		c = InterLineGet() ;
	}
	if( arg != nil )
		*arg = hasArg ? n : -1 ;
	while( c != '\n' && c != EOF )
		c = InterLineGet() ;
	InterruptOn() ;
	return res ;
}

void InterLineRestart(void)
{
	InterLinePromptInit();
	InterLinePrepareForNextLine() ;
}


/* CXPROLOG C'BUILTINS */

static void PPrompt(void)
{
	if( Drf(X0) == tEmptyAtom )
		InterLineSetPrompts(nil, nil) ;
	else {
		AtomPt a0 = XTestAtom(X0) ;
		ExtraPermanent(a0) ;	/* Ensures the prompt is not gc */
		InterLineSetPrompts(nil, AtomName(a0)) ;
	}
	JumpNext() ;
}


/* INIT */

void InterLineInit(void)
{
	InterLineInitialize() ;
	InterLineRestart() ;

	InstallCBuiltinPred("prompt", 1, PPrompt) ;
}
