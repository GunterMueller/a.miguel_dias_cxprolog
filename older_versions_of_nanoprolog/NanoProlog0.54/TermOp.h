/*
 *   This file is part of the NanoProlog system

 *   TermOp.h
 *   by A.Miguel Dias - 92/2/23
 *   GLOC - Grupo de Logica Computacional
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990 A.Miguel Dias, GLOC, DI/FCT/UNL

 *   NanoProlog is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.

 *   NanoProlog is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.

 *   You should have received a copy of the GNU General Public License
 *   along with NanoProlog; see the file COPYING.  If not, write to
 *   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* HISTORY:

 931117: release of version 0.5

*/

#define maxPrec		1200	/* Max. operator precedence.*/
#define subPrec		 999	/* Max. prec. for subterms. */
#define commaPrec	1000
#define barPrec		1100

typedef struct Operator
{
	struct Operator *next ;
	AtomPt atom ;
	short prePriority, inPriority, posPriority ;
	Bool preAssoc, inLAssoc, inRAssoc, posAssoc ;
} Operator, *OperatorPt ;

#define cOperatorPt(x)				((OperatorPt)(x))

#define OperatorNext(o)				(o)->next
#define OperatorAtom(o)				(o)->atom
#define OperatorPrePriority(o)		(o)->prefixPriority
#define OperatorInPriority(o)		(o)->infixPriority
#define OperatorPosPriority(o)		(o)->posfixPriority

OperatorPt FirstOperator(void) ;
OperatorPt NextOperator(OperatorPt o) ;
int Prefix(AtomPt atom, int *rp) ;
int Infix(AtomPt atom, int *lp, int *rp) ;
int Postfix(AtomPt atom, int *lp) ;
Bool ExclusivelyPrefix(AtomPt atom) ;
void SetupInitialAtomPriorities(void) ;
Bool DefineOperator(int p, CharPt type, Pt ops) ;

