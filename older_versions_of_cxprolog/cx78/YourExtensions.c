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
	/* What you may do here is almost boundless. The whole system has
	   already been initialized and the abstract machine is just about
	   to be activated. Here you may:
			- Install your own deterministic C system predicates
			  using 'InstallCBuiltinPred'
			- Install your own non-deterministic C system predicates
			  using 'InstallNDeterCBuiltinPred'
			- Specify an alternative boot file using 'SpecifyBootFile'
			- Create your own data structures
			- Etc.								*/
}
