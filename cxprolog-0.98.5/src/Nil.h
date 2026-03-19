/*
 *   This file is part of the CxProlog system

 *   Nil.h
 *   by A.Miguel Dias - 2010/06/22
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2025 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Nil_
#define _Nil_

#define IsNilSpecial(t)	( (t) == tNilAtom && nilIsSpecial_flag )

void NilInit(void) ;

#endif
