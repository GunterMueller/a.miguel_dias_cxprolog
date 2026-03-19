/*
 *   This file is part of the CxProlog system

 *   Operator.h
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2003 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Operator_
#define _Operator_

#define maxPrec		1200	/* Max. operator precedence.*/
#define subPrec		 999	/* Max. prec. for subterms. */
#define commaPrec	1000
#define barPrec		1100

void SetLocalOps(Bool b) ;
Bool CheckLocalOps(void) ;
int Prefix(AtomPt atom, int *rp) ;
int Infix(AtomPt atom, int *lp, int *rp) ;
int Postfix(AtomPt atom, int *lp) ;
Bool ExclusivelyPrefix(AtomPt atom) ;
void ResetOps(void) ;
void DefineOp(int p, CharPt type, Pt ops) ;
void OpsInit(void) ;

#endif
