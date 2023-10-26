/*
 *   This file is part of the CxProlog system

 *   YourExtensions.c
 */

#include "CxProlog.h"

/*
CAUTIONARY NOTES CONCERNING DEFINING BUILTIN PREDICATES WRITTEN IN C

Cautions concerning the X parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All the X parameters must be dereferenced before they are used. To dereference
a X parameter use the function Drf(.), defined in file "Term.c".
Exception: there is no need to dereference a X parameter if you pass it
to a function, such as Unify, that already dereferences it.

After dereferenced, the type of a X parameter should be checked. To check
the type of a X parameter, use the macros IsAtom(.), IsVar(.), IsInt(.),
IsFloat(.), IsStruct(.), IsList(.), IsExtra(.), etc., all defined in
file "Term.h".

Once you know the type of the term in the X parameter, you are able to
extract information from it, using the macros XStructArity(.), XStructName(.),
etc., defined in file "Term.h".

There are some handy functions available that do all this automatically
for you, when there is a single fixed expected type for some X parameter:
they dereference the X parameter, check its type, and extract
information from it. We are referring to the "Test & Extract" family of
functions, defined in file "Term.c": XTestAtom, XTestAtomName, XTestInt,
XTestPosInt, XTestNat, XTestIntRange, XTestFloat, XTestBool, XTestOnOff,
XTestVar, XTestNonVar, XTestFunctor, XTestFunctor2.

Cautions concerning control flow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A deterministic builtin predicate written in C succeeds using the macro
call JumpNext() and fails using the macro DoFail().

A nondeterministic builtin predicate written in C succeeds using the
macro call JumpNext() and stops generating more alternatives using
the macro call Jump(DiscardAndFail).

Cautions concerning the automatic grow feature
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Any builtin predicate that creates a new term, probably using the facilities
provided by file "Term.c", may generate a stack overflow, provided the
new term is large enough. If this concerns you and if you want to
take advantage of the automatic growing stacks of CxProlog, this is what you
should do:

1- For starters, the system ensures that there are at least 512 words
available in the stacks at the start of each C-builtin: if this in enough for a
your specific predicate, nothing particular needs to be done. The vast majority
of the CxProlog builtins are in this category.

2- If 512 words are not enough but you can estimate upon predicate entry the
size of the new terms, then you call the function ZEnsureFreeSpaceOnStacks(.,.)
at the beginning of the predicate. This functions reserve the space needed,
causing the stacks to grow immediately, if necessary. The CxProlog builtins in
this category are: os_args/1, free_vars/2, name/2, varnames/1, sort/2,
msort/2, keysort/2.

3- If it is hard to estimate upon predicate entry the space needed for the new
terms, but if these terms are to be created via the Z-functions - ZPushTerm,
ZReadTerm or ZReadTermFromStr - then the Z-functions already make the stacks
grow for you. However, be careful because the stacks are moved to a different
area of memory when they grow. Therefore, a pointer to a term in the
global stack can easily become a dangling pointer after such relocation.

You overcome this by avoiding storing pointers to terms in the globals stack,
or by using the special Z register, which contents is automatically relocated
along with the stacks.
The CxProlog builtins in this category are:
dict_get/3, dict_as_list/2, =:/2, current_ivar/2, clause/2, retract/1,
imported_predicate/2, queue_get/2, queue_peek/2, queue_as_list/2,
stack_pop/2, stack_top/2, stack_as_list/2, copy_term/2, copy_term/3,
read/1, read/2, read_tokens/1, read_tokens/2,
atom_term/2, atom_termq/2, array_get/3, array_as_list/2.

*/

void YourPrologue()
{
	/* What you may do here is very limited. The system has not been
	   initialized yet; not even the memory manager is available.
	   Here you may:
			- Initialize your own flags
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
