#   File:       cxprolog.make
#   Target:     cxprolog
#   Created:    Sunday, October 8, 2000 09:53:55 PM


MAKEFILE        = cxprolog.make
¥MondoBuild¥    = {MAKEFILE}  # Make blank to avoid rebuilds when makefile is modified

ObjDir          = Miguel:Work: cx:obj:
Includes        = 

Sym-PPC         = -sym off

PPCCOptions     = {Includes} {Sym-PPC} 


### Source Files ###

SrcFiles        =  ¶
				  Atom.c ¶
				  Arith.c ¶
				  Clause.c ¶
				  Clock.c ¶
				  CodeGen.c ¶
				  Compiler.c ¶
				  CxProlog.c ¶
				  Debug.c ¶
				  Dictionary.c ¶
				  FileSys.c ¶
				  Flags.c ¶
				  HashTable.c ¶
				  ImperativeVar.c ¶
				  Index.c ¶
				  ListCode.c ¶
				  Machine.c ¶
				  Memory.c ¶
				  Mesg.c ¶
				  Operator.c ¶
				  Predicate.c ¶
				  Queue.c ¶
				  Stream.c ¶
				  Term.c ¶
				  TermChars.c ¶
				  TermRead.c ¶
				  TermWrite.c ¶
				  Thread.c ¶
				  Unify.c ¶
				  Unit.c ¶
				  UnixServices.c ¶
				  Util.c ¶
				  VarDictionary.c ¶
				  YourExtensions.c


### Object Files ###

ObjFiles-PPC    =  ¶
				  "{ObjDir}Atom.c.x" ¶
				  "{ObjDir}Arith.c.x" ¶
				  "{ObjDir}Clause.c.x" ¶
				  "{ObjDir}Clock.c.x" ¶
				  "{ObjDir}CodeGen.c.x" ¶
				  "{ObjDir}Compiler.c.x" ¶
				  "{ObjDir}CxProlog.c.x" ¶
				  "{ObjDir}Debug.c.x" ¶
				  "{ObjDir}Dictionary.c.x" ¶
				  "{ObjDir}FileSys.c.x" ¶
				  "{ObjDir}Flags.c.x" ¶
				  "{ObjDir}HashTable.c.x" ¶
				  "{ObjDir}ImperativeVar.c.x" ¶
				  "{ObjDir}Index.c.x" ¶
				  "{ObjDir}ListCode.c.x" ¶
				  "{ObjDir}Machine.c.x" ¶
				  "{ObjDir}Memory.c.x" ¶
				  "{ObjDir}Mesg.c.x" ¶
				  "{ObjDir}Operator.c.x" ¶
				  "{ObjDir}Predicate.c.x" ¶
				  "{ObjDir}Queue.c.x" ¶
				  "{ObjDir}Stream.c.x" ¶
				  "{ObjDir}Term.c.x" ¶
				  "{ObjDir}TermChars.c.x" ¶
				  "{ObjDir}TermRead.c.x" ¶
				  "{ObjDir}TermWrite.c.x" ¶
				  "{ObjDir}Thread.c.x" ¶
				  "{ObjDir}Unify.c.x" ¶
				  "{ObjDir}Unit.c.x" ¶
				  "{ObjDir}UnixServices.c.x" ¶
				  "{ObjDir}Util.c.x" ¶
				  "{ObjDir}VarDictionary.c.x" ¶
				  "{ObjDir}YourExtensions.c.x"


### Libraries ###

LibFiles-PPC    =  ¶
				  "{PPCLibraries}PPCSIOW.o" ¶
				  "{SharedLibraries}InterfaceLib" ¶
				  "{SharedLibraries}StdCLib" ¶
				  "{SharedLibraries}MathLib" ¶
				  "{PPCLibraries}StdCRuntime.o" ¶
				  "{PPCLibraries}PPCCRuntime.o" ¶
				  "{PPCLibraries}PPCToolLibs.o"


### Default Rules ###

.c.x  Ä  .c  {¥MondoBuild¥}
	{PPCC} {depDir}{default}.c -o {targDir}{default}.c.x {PPCCOptions}


### Build Rules ###

cxprolog  ÄÄ  {ObjFiles-PPC} {LibFiles-PPC} {¥MondoBuild¥}
	PPCLink ¶
		-o {Targ} ¶
		{ObjFiles-PPC} ¶
		{LibFiles-PPC} ¶
		{Sym-PPC} ¶
		-mf -d ¶
		-t 'APPL' ¶
		-c 'siow'


cxprolog  ÄÄ  "{RIncludes}"SIOW.r {¥MondoBuild¥}
	Rez "{RIncludes}"SIOW.r -o {Targ} -append


### Required Dependencies ###

"{ObjDir}Atom.c.x"  Ä  Atom.c
"{ObjDir}Arith.c.x"  Ä  Arith.c
"{ObjDir}Clause.c.x"  Ä  Clause.c
"{ObjDir}Clock.c.x"  Ä  Clock.c
"{ObjDir}CodeGen.c.x"  Ä  CodeGen.c
"{ObjDir}Compiler.c.x"  Ä  Compiler.c
"{ObjDir}CxProlog.c.x"  Ä  CxProlog.c
"{ObjDir}Debug.c.x"  Ä  Debug.c
"{ObjDir}Dictionary.c.x"  Ä  Dictionary.c
"{ObjDir}FileSys.c.x"  Ä  FileSys.c
"{ObjDir}Flags.c.x"  Ä  Flags.c
"{ObjDir}HashTable.c.x"  Ä  HashTable.c
"{ObjDir}ImperativeVar.c.x"  Ä  ImperativeVar.c
"{ObjDir}Index.c.x"  Ä  Index.c
"{ObjDir}ListCode.c.x"  Ä  ListCode.c
"{ObjDir}Machine.c.x"  Ä  Machine.c
"{ObjDir}Memory.c.x"  Ä  Memory.c
"{ObjDir}Mesg.c.x"  Ä  Mesg.c
"{ObjDir}Operator.c.x"  Ä  Operator.c
"{ObjDir}Predicate.c.x"  Ä  Predicate.c
"{ObjDir}Queue.c.x"  Ä  Queue.c
"{ObjDir}Stream.c.x"  Ä  Stream.c
"{ObjDir}Term.c.x"  Ä  Term.c
"{ObjDir}TermChars.c.x"  Ä  TermChars.c
"{ObjDir}TermRead.c.x"  Ä  TermRead.c
"{ObjDir}TermWrite.c.x"  Ä  TermWrite.c
"{ObjDir}Thread.c.x"  Ä  Thread.c
"{ObjDir}Unify.c.x"  Ä  Unify.c
"{ObjDir}Unit.c.x"  Ä  Unit.c
"{ObjDir}UnixServices.c.x"  Ä  UnixServices.c
"{ObjDir}Util.c.x"  Ä  Util.c
"{ObjDir}VarDictionary.c.x"  Ä  VarDictionary.c
"{ObjDir}YourExtensions.c.x"  Ä  YourExtensions.c


### Optional Dependencies ###
### Build this target to generate "include file" dependencies. ###

Dependencies  Ä  $OutOfDate
	MakeDepend ¶
		-append {MAKEFILE} ¶
		-ignore "{CIncludes}" ¶
		-objdir "{ObjDir}" ¶
		-objext .x ¶
		{Includes} ¶
		{SrcFiles}


#*** Dependencies: Cut here ***
# These dependencies were produced at 21:59:22 on 08 de out de 2000 by MakeDepend

'Miguel:Work: cx:obj:Atom.c.x'	Ä  ¶
	:Atom.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Arith.c.x'	Ä  ¶
	:Arith.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Clause.c.x'	Ä  ¶
	:Clause.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Clock.c.x'	Ä  ¶
	:Clock.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:CodeGen.c.x'	Ä  ¶
	:CodeGen.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Compiler.c.x'	Ä  ¶
	:Compiler.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:CxProlog.c.x'	Ä  ¶
	:CxProlog.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Debug.c.x'	Ä  ¶
	:Debug.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Dictionary.c.x'	Ä  ¶
	:Dictionary.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:FileSys.c.x'	Ä  ¶
	:FileSys.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Flags.c.x'	Ä  ¶
	:Flags.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:HashTable.c.x'	Ä  ¶
	:HashTable.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:ImperativeVar.c.x'	Ä  ¶
	:ImperativeVar.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Index.c.x'	Ä  ¶
	:Index.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:ListCode.c.x'	Ä  ¶
	:ListCode.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Machine.c.x'	Ä  ¶
	:Machine.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Memory.c.x'	Ä  ¶
	:Memory.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Mesg.c.x'	Ä  ¶
	:Mesg.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Operator.c.x'	Ä  ¶
	:Operator.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Predicate.c.x'	Ä  ¶
	:Predicate.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Queue.c.x'	Ä  ¶
	:Queue.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Stream.c.x'	Ä  ¶
	:Stream.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Term.c.x'	Ä  ¶
	:Term.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:TermChars.c.x'	Ä  ¶
	:TermChars.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:TermRead.c.x'	Ä  ¶
	:TermRead.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:TermWrite.c.x'	Ä  ¶
	:TermWrite.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Thread.c.x'	Ä  ¶
	:Thread.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Unify.c.x'	Ä  ¶
	:Unify.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Unit.c.x'	Ä  ¶
	:Unit.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:UnixServices.c.x'	Ä  ¶
	:UnixServices.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:Util.c.x'	Ä  ¶
	:Util.c ¶
	:Util.h

'Miguel:Work: cx:obj:VarDictionary.c.x'	Ä  ¶
	:VarDictionary.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

'Miguel:Work: cx:obj:YourExtensions.c.x'	Ä  ¶
	:YourExtensions.c ¶
	:CxProlog.h ¶
	:Util.h ¶
	:Stream.h ¶
	:Mesg.h ¶
	:Memory.h ¶
	:Atom.h ¶
	:HashTable.h ¶
	:Term.h ¶
	:ImperativeVar.h ¶
	:Clock.h ¶
	:Arith.h ¶
	:TermChars.h ¶
	:VarDictionary.h ¶
	:Operator.h ¶
	:TermRead.h ¶
	:TermWrite.h ¶
	:Clause.h ¶
	:Predicate.h ¶
	:Unit.h ¶
	:Unify.h ¶
	:Flags.h ¶
	:Machine.h ¶
	:Thread.h ¶
	:ListCode.h ¶
	:Debug.h ¶
	:CodeGen.h ¶
	:Index.h ¶
	:Compiler.h ¶
	:UnixServices.h ¶
	:FileSys.h ¶
	:Queue.h ¶
	:Dictionary.h

