/*
 *   This file is part of the CxProlog system

 *   Number.h
 *   by A.Miguel Dias - 2001/02/27
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Number_
#define _Number_


/* BOOL, CHAR, CODE & BYTE */

Pt MakeBool(Bool b) ;
Pt MakeChar(int c) ;
int XChar(Pt t) ;
Pt MakeCode(int c) ;
int XCode(Pt t) ;
Pt MakeByte(int c) ;
int XByte(Pt t) ;


/* INT */

#define maxInt				cPInt(maxWord>>4)
#define minInt				(-maxInt - 1)

#define IsNat(t)			( IsInt(t) && XInt(t) >= 0 )
#define IsPos(t)			( IsInt(t) && XInt(t) > 0 )
#define IsCode(t)			( IsNat(t) && CharValidCode(XInt(t)) )
#define IsByte(t)			( IsNat(t) && XInt(t) <= 255 )

Pt MakeInt(PInt n) ;
Pt MakeInt64(Int64 n) ;
PInt XInt(Pt t) ;
PInt XAsInt(Pt t) ;
Int64 XAsInt64(Pt t) ;
int CompareInt(PInt i1, PInt i2) ;


/* FLOAT */

Pt MakeFloat(PFloat r) ;
PFloat XFloat(Pt t) ;
PFloat XAsFloat(Pt t) ;
int CompareFloat(PFloat r1, PFloat r2) ;
int CalculateFloatSize(void) ;
void FloatDisplayPrecUpdateFlag(int newValue) ;


/* NUMBER */

extern int intSize, floatSize ;

CharPt XNumberAsStr(Pt t) ;
Pt NumberFromStr(CharPt s) ;
int CompareNumber(Pt t1, Pt t2) ;
void NumbersInit(void) ;
void NumbersInit2(void) ;

#endif
