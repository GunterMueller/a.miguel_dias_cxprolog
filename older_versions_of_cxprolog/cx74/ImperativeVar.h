/*
 *   This file is part of the CxProlog system

 *   ImperativeVar.h
 *   by A.Miguel Dias - 2000/04/02
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _ImperativeVar_
#define _ImperativeVar_

typedef struct ImperativeVar
{
	struct ImperativeVar *nextHash ;/* Next imperative var in the hash chain */
	AtomPt atom ;					/* Variable name */
	Pt value ;						/* Value of imperative variable */
	struct Unit *unit ;				/* Owner unit */
	Bool isBuiltin ;				/* ivar is builtin, cannot be locally redefined */
} ImperativeVar, *ImperativeVarPt ;

#define cImperativeVarPt(p)			((ImperativeVarPt)(p))

#define IVarIsBuiltin(iv)			(iv)->isBuiltin

void DefineImperativeVar(AtomPt atom) ;
Bool IsDefinedImperativeVar(AtomPt atom) ;
void UndefineImperativeVar(AtomPt atom) ;
void ImperativeVarSet(AtomPt atom, Pt value) ;
Pt ImperativeVarGet(AtomPt atom) ;
ImperativeVarPt FirstImperativeVar(void) ;
ImperativeVarPt NextImperativeVar(ImperativeVarPt iv) ;
void InitImperativeVars(void) ;

#endif
