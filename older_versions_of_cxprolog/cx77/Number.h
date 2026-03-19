/*
 *   This file is part of the CxProlog system

 *   Number.h
 *   by A.Miguel Dias - 2001/02/27
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2001 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Number_
#define _Number_


/* INT */

#define maxBigInt			cBigInt((~cBigUInt(0))>>1)
#define minBigInt			(-maxBigInt - 1)

#define IsNat(t)			( IsInt(t) && XInt(t) >= 0 )
#define IsPos(t)			( IsInt(t) && XInt(t) > 0 )
#define IsByte(t)			( IsNat(t) && XInt(t) <= 255 )

Pt MakeInt(BigInt n) ;
BigInt XInt(Pt t) ;

Size BigIntTableTotalSize(void) ;
Size BigIntTableUsed(void) ;
int CompareInt(BigInt i1, BigInt i2) ;


/* REAL */

Pt MakeReal(double r) ;
double XReal(Pt p) ;
int CompareReal(double r1, double r2) ;


/* NUMBER */

Bool Narrow(double f, BigInt *i) ;
CharPt XNumberAsStr(Pt t) ;
int CompareNumber(Pt t1, Pt t2) ;
void InitNumbers(void) ;

#endif