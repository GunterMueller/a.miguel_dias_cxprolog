/*
 *   This file is part of the CxProlog system

 *   Operator.h
 *   by A.Miguel Dias - 1992/02/23
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2006 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Operator_
#define _Operator_

#define maxPrec		1200	/* Max. operator precedence.*/
#define subPrec		 999	/* Max. prec. for subterms. */
#define commaPrec	1000
#define barPrec		1100

typedef struct Operator
{
	struct Operator *next ;
	AtomPt atom ;
	struct Unit *unit ;
	short prePriority, inPriority, posPriority ;
	Bool preAssoc, inLAssoc, inRAssoc, posAssoc ;
	Bool isBuiltin, forceParenthesis ;
} Operator, *OperatorPt ;

#define cOperatorPt(x)					((OperatorPt)(x))

#define OperatorAtom(op)				(op)->atom
#define OperatorName(op)				AtomName((op)->atom)
#define OperatorUnit(op)				(op)->unit
#define OperatorPrePriority(op)			(op)->prefixPriority
#define OperatorInPriority(op)			(op)->infixPriority
#define OperatorPosPriority(op)			(op)->posfixPriority
#define OperatorParenthesised(op)		(op)->forceParenthesis
#define OperatorIsBuiltin(op)			(op)->isBuiltin

void SetLocalOperators(Bool b) ;
Bool CheckLocalOperators(void) ;
int Prefix(AtomPt atom, int *rp) ;
int Infix(AtomPt atom, int *lp, int *rp) ;
int Postfix(AtomPt atom, int *lp) ;
void ResetOperators(void) ;
void DefineOperator(int p, CharPt type, Pt ops) ;
void OperatorsInit(void) ;

#endif
