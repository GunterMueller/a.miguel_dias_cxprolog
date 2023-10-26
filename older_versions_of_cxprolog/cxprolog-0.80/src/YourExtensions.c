/*
 *   This file is part of the CxProlog system

 *   YourExtensions.c
 */

#include "CxProlog.h"

/*
Cautions concerning the X parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All the X parameters must be dereferenced before they are used. To dereference
a X parameter use the function Drf, defined in file "Term.c".
Exception: no need to dereference a X parameter if you pass it to a function,
such as Unify, that already dereferences it.

After dereferenced, the type of a X parameter should be checked. To check
the type of a X parameter, use the macros IsAtom, IsVar, IsInt, IsFloat,
IsStruct, IsList, IsExtra, etc., all defined in file "Term.h".

There are some handy functions available that do all this automatically
for you: they dereference the X parameter, check its type, and extract
information from it. We are referring to the "Test & Extract" family of
functions, defined in file "Term.c": XTestAtom, XTestAtomName, XTestInt,
XTestPosInt, XTestNat, XTestIntRange, XTestFloat, XTestBool, XTestOnOff,
XTestVar, XTestNonVar, XTestFunctor, XTestFunctor2.
*/

/*
Cautions concerning control flow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A deterministic builtin predicate written in C succeeds using the macro
call JumpNext() and fails using the macro DoFail().

A nondeterministic builtin predicate written in C succeeds using the
macro call JumpNext() and stops generating more alternatives using
the macro call Jump(DiscardAndFail).
*/

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
