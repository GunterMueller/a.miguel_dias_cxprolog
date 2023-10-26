/*
 *   This file is part of the CxProlog system

 *   Compiler.h
 *   by A.Miguel Dias - 1989/11/25
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2000 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Compiler_
#define _Compiler_

void InitCompiler(void) ;
void Compiler(Pt head, Pt body, Hdl *cd, long *size) ;

#endif
