OBJS = Arith.o Compiler.o Index.o Interpreter.o \
	Heap.o ListCode.o Module.o NanoProlog.o Predicates.o \
	System.o SystemMore.o Stream.o TermChars.o TermOp.o TermRead.o \
	TermExtra.o TermWrite.o Term.o Thread.o Unify.o Util.o

CC = gcc 

CFLAGS = -O

%.o: %.c
	$(CC) $(CFLAGS) $(DEFINES) -c $<

%.s: %.c
	$(CC) $(CFLAGS) $(DEFINES) -S $<

nano: $(OBJS)
	$(CC) $(CFLAGS) -o nano $(OBJS) -lm

cgi: $(OBJS)
	$(CC) $(CFLAGS) -o NanoCGI.cgi NanoCGI.c
