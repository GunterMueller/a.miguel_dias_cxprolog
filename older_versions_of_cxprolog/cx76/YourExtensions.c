/*
 *   This file is part of the CxProlog system

 *   YourExtensions.c
 */

#include "CxProlog.h"

void YourPrologue()
{
	/* What you may do here is very limited. The system has not been
	   initialized yet; not even the memory manager is available.
	   Here you may:
			- Inicialize your own flags
			- Fork some processes
			- Etc.							*/
}

void YourExtensions()
{
	/* What you may do here is almost boundless. The whole system has already
	   been initialized and the abstract machine is just about to be launched.
	   Here you may:
			- Install your own extra C system predicates
									(using 'InstallCBuiltinPred')
			- Load some files (using 'BasicLoadFile')
			- Create your own data structures
			- Etc.								*/
}
