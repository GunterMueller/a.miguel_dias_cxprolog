/*
 *   This file is part of the CxProlog system

 *   Number.h
 *   by A.Miguel Dias - 2001/02/27
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2004 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Number_
#define _Number_


/* INT */

#define maxInt				cPInt(maxWord>>4)
#define minInt				(-maxInt - 1)

#define IsNat(t)			( IsInt(t) && XInt(t) >= 0 )
#define IsPos(t)			( IsInt(t) && XInt(t) > 0 )
#define IsCode(t)			( IsInt(t) && InRange(XInt(t), -1, 255) )
#define IsByte(t)			( IsNat(t) && XInt(t) <= 255 )

Pt MakeInt(PInt n) ;
PInt XInt(Pt t) ;
int CompareInt(PInt i1, PInt i2) ;
Pt MakeCode(int c) ;
int XCode(Pt t) ;
Pt MakeChar(int c) ;
Pt MakeByte(int c) ;
int XByte(Pt t) ;


/* FLOAT */

Pt MakeFloat(PFloat r) ;
PFloat XFloat(Pt p) ;
PFloat XAsFloat(Pt p) ;
int CompareFloat(PFloat r1, PFloat r2) ;


/* NUMBER */

#define infFloat	(HUGE_VAL)

extern int intSize, floatSize ;
extern CharPt floatFormat, intFormat ;

CharPt XNumberAsStr(Pt t) ;
Pt NumberFromStr(CharPt s) ;
int CompareNumber(Pt t1, Pt t2) ;
void NumbersInit(void) ;
void NumbersInit2(void) ;

#endif
