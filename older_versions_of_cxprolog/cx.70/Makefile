OBJS = Arith.o Atom.o Builtins.o BuiltinsMore.o Clause.o Clock.o\
		CodeGen.o Compiler.o Debug.o ImperativeVar.o HashTable.o Index.o\
		Machine.o ListCode.o Mesg.o CxProlog.o Operator.o\
		Predicate.o Space.o Stream.o Term.o TermChars.o TermExtra.o\
		TermRead.o TermWrite.o Thread.o Unify.o Unit.o Util.o VarDictionary.o

CC = gcc 

CFLAGS = -O

%.o: %.c
	$(CC) $(CFLAGS) $(DEFINES) -c $<

%.s: %.c
	$(CC) $(CFLAGS) $(DEFINES) -S $<

cxprolog: $(OBJS)
	$(CC) $(CFLAGS) -o cxprolog $(OBJS) -lm

pack:
	tar cv cx.70 > CxProlog.70 ; gzip CxProlog.70

cgi: $(OBJS)
	$(CC) $(CFLAGS) -o NanoCGI.cgi NanoCGI.c
