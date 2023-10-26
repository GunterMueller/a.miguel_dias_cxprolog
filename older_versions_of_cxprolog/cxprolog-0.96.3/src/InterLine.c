/*
 *   This file is part of the CxProlog system

 *   InterLine.c
 *   by A.Miguel Dias - 2007/09/01
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
#include <setjmp.h>

Bool interactiveSession, useInteractiveStreams ;


/* PROMPTS */

static CharPt linePrompt, saveLinePrompt ;
static Bool linePromptHasPrioritary ;
static CharPt topLevelPrompt, topLevelContPrompt  ;
static Bool atTopLevel ;

static CharPt InterLineGetPrompt(void)
{
	CharPt prompt ;
    if( !InteractiveSession() )
        return "" ;
	if( !linePromptHasPrioritary && atTopLevel ) {
		prompt = topLevelPrompt != nil ? topLevelPrompt : topLevelContPrompt ;
		topLevelPrompt = nil ;
		return prompt ;
	}
	return linePrompt ;
}

void InterLineBeginTopPrompt(CharPt main, CharPt cont) {
	topLevelPrompt = main ;
	topLevelContPrompt = cont ;
	atTopLevel = true ;
	if( InteractiveSession() && !UseInteractiveStreams() )
		StreamPutStr(userOut, topLevelPrompt) ;
}

void InterLineEndTopPrompt() {
	atTopLevel = false ;
}

void InterLineBeginLinePrompt(CharPt temp)
{
	saveLinePrompt = linePrompt ;
	linePrompt = temp ;
	linePromptHasPrioritary = true ;
	if( InteractiveSession() && !UseInteractiveStreams() )
		StreamPutStr(userOut, linePrompt) ;
}

void InterLineEndLinePrompt()
{
	linePrompt = saveLinePrompt ;
	linePromptHasPrioritary = false ;
}

void InterLinePromptInit()
{
	linePrompt = "|: " ;
	atTopLevel = false ;
	linePromptHasPrioritary = false ;
}


/* INTERACTIVE COMMAND LINE */

static CharPt line, linePt ;

#if USE_READLINE
#include <wchar.h>
#include <readline/readline.h>
#include <readline/history.h>

#if RL_READLINE_VERSION < 0x400
#	error "Required library: readline-4.0 or newer."
#endif

static Bool InterLineMore(void)
{
	sigjmp_buf interLineJB ;
	register CharPt s ;

	if( line != linePt && linePt[-1] == '\n' ) {
		do {
			if( sigsetjmp(interLineJB, 1) == 0 ) {
				InterruptCatch(&interLineJB) ;
				line = readline(InterLineGetPrompt()) ;
				InterruptCatch(nil) ;				
			}
		} while( InterruptHandle() ) ;
		if( line == nil )
			return false ;	/* EOF */
	/* Skip blanks at begin */
		for( s = line ; BasicCharType(*cUCharPt(s)) == _BL ; s++ )
			;
		linePt = s ;
	/* Delete blanks at end */
		for( s = linePt + strlen(linePt) - 1
			; linePt <= s && BasicCharType(*cUCharPt(s)) == _BL
			; s-- )
				;
		s[1] = '\0' ;	
	}
	if( *linePt == '\0' ) {	/* Inject '\n' to make the term reader happy */
		free(line) ;
		linePt = line = "\n" ;
	}
	else {
		Bool isRepetition = history_length > 0
					&& strcmp(linePt, history_get(history_length)->line) == 0 ;
		if( !isRepetition )
			add_history(linePt) ;
	}
	return true ;
}

void InterLineChangedUserStreams()
{
	rl_instream = StreamFILE(userIn) ;
	rl_outstream = StreamFILE(userOut) ;
}

static void InterLineReinitialize(void)
{
	line = "\n" ;
	linePt = line + 1 ;
}

static void InterLineInitialize(void)
{
	InterLineReinitialize() ;
 	rl_readline_name = "cxprolog" ;		/* For cond. parsing of "~/.inputrc". */
	rl_inhibit_completion = 1 ;			/* Disable completion */
	using_history() ;					/* Initialize history */
}

WChar InterLineGet()
{
	if( *linePt == '\0' && !InterLineMore() )
		return EOF ;
	else {
		WChar c ;
		int len = mbrtowc((wchar_t *)&c, linePt, MB_LEN_MAX, nil) ;
		if( len <= 0 ) {
			if( len == 0 ) InternalError("InterLineGet") ;
			else Error("Cannot convert to wide character (InterLineGet)") ;
		}			
		linePt += len ;
		return c ;
	}
}

WChar InterLinePeek()
{	
	if( *linePt == '\0' && !InterLineMore() )
		return EOF ;
	else {
		WChar c ;
		int len = mbrtowc((wchar_t *)&c, linePt, MB_LEN_MAX, nil) ;
		if( len <= 0 ) {
			if( len == 0 ) InternalError("InterLinePeek") ;
			else Error("Cannot convert to wide character (InterLinePeek)") ;
		}			
		return c ;
	}
}

#else

#define lineSize		64

static Bool InterLineMore(void)
{
	if( line != linePt && linePt[-1] == '\n' )
		StreamPutStr(userOut, InterLineGetPrompt()) ;
	if( FileGetCharStrInteractive(StreamChannel(userIn), line, lineSize) == nil ) /* ??? userIn */
		return false ;	/* EOF */
	linePt = line ;
	return true ;
}

void InterLineChangedUserStreams()
{
	/* Nothing */
}

static void InterLineReinitialize(void)
{
	strcpy(line, "\n") ;
	linePt = line + 1 ;
}

static void InterLineInitialize(void)
{
	line = Allocate(Words(lineSize), false) ;
	InterLineReinitialize() ;
}


WChar InterLineGet()
{
	if( *linePt == '\0' && !InterLineMore() )
		return EOF ;
	return CharDecode(linePt) ;
}

WChar InterLinePeek()
{	
	if( *linePt == '\0' && !InterLineMore() )
		return EOF ;
	return CharFirst(linePt) ;
}

#endif

Bool InterLineGetSingleChar(WChar *c)
{
	if( !InteractiveSession() )
		return false ;
	if( !SetRawInput(StreamFILE(userIn)) ) /* ??? userIn */
		return false ;
	*c = FileGetCharInteractive(StreamChannel(userIn)) ; /* ??? userIn */
	UnsetRawInput() ;
	return true ;		
}

void InterLineRestart()
{
	InterLinePromptInit() ;
	InterLineReinitialize() ;
}



/* CXPROLOG C'BUILTINS */

static void PPrompt(void)
{
	AtomPt a1 ;
	Ensure( UnifyWithAtomic(X0, MakeAtom(linePrompt)) ) ;
	a1 = XTestAtom(X1) ;
	ExtraPermanent(a1) ;	/* Ensures the prompt is not gc */
	linePrompt = AtomName(a1) ;
	JumpNext() ;
}



/* INIT */

void InterLineInit2()
{
	InstallCBuiltinPred("prompt", 2, PPrompt) ;
}

void InterLineInit()
{
	InterLinePromptInit() ;
	InterLineInitialize() ;
	interactiveSession = OSIsATty(fileno(stdin)) && OSIsATty(fileno(stdout)) ;
	useInteractiveStreams = true ;	/* false for debugging */
}
