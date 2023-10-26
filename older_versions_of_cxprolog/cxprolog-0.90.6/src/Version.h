/*
 *   This file is part of the CxProlog system

 *   Version.h
 *   by A.Miguel Dias - 2004/06/28
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2005 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Version_
#define _Version_

void SetVersion(CharPt a, CharPt v, CharPt s, CharPt r) ;
void GetVersion(CharPt *a, CharPt *v, CharPt *s, CharPt *r) ;
CharPt GetVersionString(void) ;
void ShowVersion(void) ;
void VersionInit(void) ;

#endif
