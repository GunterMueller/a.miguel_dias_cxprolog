/*
 *   This file is part of the CxProlog system

 *   Boot.h
 *   by A.Miguel Dias - 2003/06/11
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2007 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Boot_
#define _Boot_

void YourPrologue(void) ;
void YourExtensions(void) ;
CharPt PrefixDirPath(CharPt path, CharPt name, CharPt extension) ;
CharPt CurrentDirPath(CharPt path, CharPt name, CharPt extension) ;
void Boot(int argc, CharPt argv[]) ;
void BootInit(void) ;

#endif
