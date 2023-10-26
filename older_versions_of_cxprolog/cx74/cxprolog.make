#   File:       cxprolog.make
#   Target:     cxprolog
#   Created:    Sunday, October 8, 2000 09:53:55 PM


MAKEFILE        = cxprolog.make
�MondoBuild�    = {MAKEFILE}  # Make blank to avoid rebuilds when makefile is modified

ObjDir          = Miguel:Work: cx:obj:
Includes        = 

Sym-PPC         = -sym off

PPCCOptions     = {Includes} {Sym-PPC} 


### Source Files ###

SrcFiles        =  �
				  Atom.c �
				  Arith.c �
				  Clause.c �
				  Clock.c �
				  CodeGen.c �
				  Compiler.c �
				  CxProlog.c �
				  Debug.c �
				  Dictionary.c �
				  FileSys.c �
				  Flags.c �
				  HashTable.c �
				  ImperativeVar.c �
				  Index.c �
				  ListCode.c �
				  Machine.c �
				  Memory.c �
				  Mesg.c �
				  Operator.c �
				  Predicate.c �
				  Queue.c �
				  Stream.c �
				  Term.c �
				  TermChars.c �
				  TermRead.c �
				  TermWrite.c �
				  Thread.c �
				  Unify.c �
				  Unit.c �
				  UnixServices.c �
				  Util.c �
				  VarDictionary.c �
				  YourExtensions.c


### Object Files ###

ObjFiles-PPC    =  �
				  "{ObjDir}Atom.c.x" �
				  "{ObjDir}Arith.c.x" �
				  "{ObjDir}Clause.c.x" �
				  "{ObjDir}Clock.c.x" �
				  "{ObjDir}CodeGen.c.x" �
				  "{ObjDir}Compiler.c.x" �
				  "{ObjDir}CxProlog.c.x" �
				  "{ObjDir}Debug.c.x" �
				  "{ObjDir}Dictionary.c.x" �
				  "{ObjDir}FileSys.c.x" �
				  "{ObjDir}Flags.c.x" �
				  "{ObjDir}HashTable.c.x" �
				  "{ObjDir}ImperativeVar.c.x" �
				  "{ObjDir}Index.c.x" �
				  "{ObjDir}ListCode.c.x" �
				  "{ObjDir}Machine.c.x" �
				  "{ObjDir}Memory.c.x" �
				  "{ObjDir}Mesg.c.x" �
				  "{ObjDir}Operator.c.x" �
				  "{ObjDir}Predicate.c.x" �
				  "{ObjDir}Queue.c.x" �
				  "{ObjDir}Stream.c.x" �
				  "{ObjDir}Term.c.x" �
				  "{ObjDir}TermChars.c.x" �
				  "{ObjDir}TermRead.c.x" �
				  "{ObjDir}TermWrite.c.x" �
				  "{ObjDir}Thread.c.x" �
				  "{ObjDir}Unify.c.x" �
				  "{ObjDir}Unit.c.x" �
				  "{ObjDir}UnixServices.c.x" �
				  "{ObjDir}Util.c.x" �
				  "{ObjDir}VarDictionary.c.x" �
				  "{ObjDir}YourExtensions.c.x"


### Libraries ###

LibFiles-PPC    =  �
				  "{PPCLibraries}PPCSIOW.o" �
				  "{SharedLibraries}InterfaceLib" �
				  "{SharedLibraries}StdCLib" �
				  "{SharedLibraries}MathLib" �
				  "{PPCLibraries}StdCRuntime.o" �
				  "{PPCLibraries}PPCCRuntime.o" �
				  "{PPCLibraries}PPCToolLibs.o"


### Default Rules ###

.c.x  �  .c  {�MondoBuild�}
	{PPCC} {depDir}{default}.c -o {targDir}{default}.c.x {PPCCOptions}


### Build Rules ###

cxprolog  ��  {ObjFiles-PPC} {LibFiles-PPC} {�MondoBuild�}
	PPCLink �
		-o {Targ} �
		{ObjFiles-PPC} �
		{LibFiles-PPC} �
		{Sym-PPC} �
		-mf -d �
		-t 'APPL' �
		-c 'siow'


cxprolog  ��  "{RIncludes}"SIOW.r {�MondoBuild�}
	Rez "{RIncludes}"SIOW.r -o {Targ} -append


### Required Dependencies ###

"{ObjDir}Atom.c.x"  �  Atom.c
"{ObjDir}Arith.c.x"  �  Arith.c
"{ObjDir}Clause.c.x"  �  Clause.c
"{ObjDir}Clock.c.x"  �  Clock.c
"{ObjDir}CodeGen.c.x"  �  CodeGen.c
"{ObjDir}Compiler.c.x"  �  Compiler.c
"{ObjDir}CxProlog.c.x"  �  CxProlog.c
"{ObjDir}Debug.c.x"  �  Debug.c
"{ObjDir}Dictionary.c.x"  �  Dictionary.c
"{ObjDir}FileSys.c.x"  �  FileSys.c
"{ObjDir}Flags.c.x"  �  Flags.c
"{ObjDir}HashTable.c.x"  �  HashTable.c
"{ObjDir}ImperativeVar.c.x"  �  ImperativeVar.c
"{ObjDir}Index.c.x"  �  Index.c
"{ObjDir}ListCode.c.x"  �  ListCode.c
"{ObjDir}Machine.c.x"  �  Machine.c
"{ObjDir}Memory.c.x"  �  Memory.c
"{ObjDir}Mesg.c.x"  �  Mesg.c
"{ObjDir}Operator.c.x"  �  Operator.c
"{ObjDir}Predicate.c.x"  �  Predicate.c
"{ObjDir}Queue.c.x"  �  Queue.c
"{ObjDir}Stream.c.x"  �  Stream.c
"{ObjDir}Term.c.x"  �  Term.c
"{ObjDir}TermChars.c.x"  �  TermChars.c
"{ObjDir}TermRead.c.x"  �  TermRead.c
"{ObjDir}TermWrite.c.x"  �  TermWrite.c
"{ObjDir}Thread.c.x"  �  Thread.c
"{ObjDir}Unify.c.x"  �  Unify.c
"{ObjDir}Unit.c.x"  �  Unit.c
"{ObjDir}UnixServices.c.x"  �  UnixServices.c
"{ObjDir}Util.c.x"  �  Util.c
"{ObjDir}VarDictionary.c.x"  �  VarDictionary.c
"{ObjDir}YourExtensions.c.x"  �  YourExtensions.c


### Optional Dependencies ###
### Build this target to generate "include file" dependencies. ###

Dependencies  �  $OutOfDate
	MakeDepend �
		-append {MAKEFILE} �
		-ignore "{CIncludes}" �
		-objdir "{ObjDir}" �
		-objext .x �
		{Includes} �
		{SrcFiles}


#*** Dependencies: Cut here ***
# These dependencies were produced at 21:59:22 on 08 de out de 2000 by MakeDepend

'Miguel:Work: cx:obj:Atom.c.x'	�  �
	:Atom.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Arith.c.x'	�  �
	:Arith.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Clause.c.x'	�  �
	:Clause.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Clock.c.x'	�  �
	:Clock.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:CodeGen.c.x'	�  �
	:CodeGen.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Compiler.c.x'	�  �
	:Compiler.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:CxProlog.c.x'	�  �
	:CxProlog.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Debug.c.x'	�  �
	:Debug.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Dictionary.c.x'	�  �
	:Dictionary.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:FileSys.c.x'	�  �
	:FileSys.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Flags.c.x'	�  �
	:Flags.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:HashTable.c.x'	�  �
	:HashTable.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:ImperativeVar.c.x'	�  �
	:ImperativeVar.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Index.c.x'	�  �
	:Index.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:ListCode.c.x'	�  �
	:ListCode.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Machine.c.x'	�  �
	:Machine.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Memory.c.x'	�  �
	:Memory.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Mesg.c.x'	�  �
	:Mesg.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Operator.c.x'	�  �
	:Operator.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Predicate.c.x'	�  �
	:Predicate.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Queue.c.x'	�  �
	:Queue.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Stream.c.x'	�  �
	:Stream.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Term.c.x'	�  �
	:Term.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:TermChars.c.x'	�  �
	:TermChars.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:TermRead.c.x'	�  �
	:TermRead.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:TermWrite.c.x'	�  �
	:TermWrite.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Thread.c.x'	�  �
	:Thread.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Unify.c.x'	�  �
	:Unify.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Unit.c.x'	�  �
	:Unit.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:UnixServices.c.x'	�  �
	:UnixServices.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:Util.c.x'	�  �
	:Util.c �
	:Util.h

'Miguel:Work: cx:obj:VarDictionary.c.x'	�  �
	:VarDictionary.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

'Miguel:Work: cx:obj:YourExtensions.c.x'	�  �
	:YourExtensions.c �
	:CxProlog.h �
	:Util.h �
	:Stream.h �
	:Mesg.h �
	:Memory.h �
	:Atom.h �
	:HashTable.h �
	:Term.h �
	:ImperativeVar.h �
	:Clock.h �
	:Arith.h �
	:TermChars.h �
	:VarDictionary.h �
	:Operator.h �
	:TermRead.h �
	:TermWrite.h �
	:Clause.h �
	:Predicate.h �
	:Unit.h �
	:Unify.h �
	:Flags.h �
	:Machine.h �
	:Thread.h �
	:ListCode.h �
	:Debug.h �
	:CodeGen.h �
	:Index.h �
	:Compiler.h �
	:UnixServices.h �
	:FileSys.h �
	:Queue.h �
	:Dictionary.h

