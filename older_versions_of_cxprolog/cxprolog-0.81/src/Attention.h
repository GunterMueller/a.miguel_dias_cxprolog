/*
 *   This file is part of the CxProlog system

 *   Attention.h
 *   by A.Miguel Dias - 2001/04/24
 *   CITI - Centro de Informatica e Tecnologias da Informacao
 *   Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *   Copyright (C) 1990-2002 A.Miguel Dias, CITI, DI/FCT/UNL
 */

#ifndef _Attention_
#define _Attention_

#define Attention()				(attention)
#define Interrupted()			(interrupted)
#define Booting()				(booting)

extern Bool attention, interrupted, booting ;

Bool InterruptHandle(void) ;
Bool AttentionHandle(PredicatePt pr) ;

#endif
