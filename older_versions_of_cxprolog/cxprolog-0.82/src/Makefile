NAMES = Arith Atom Attention Buffer Clause Clock CmdLine        \
	    CodeGen Compiler CxProlog Debug Dict Disassembler Extra \
		FileSys Flags GCollection HashTable Instructions IVar   \
		Index Machine Memory Mesg Number Operator OSMac OSUnix  \
		OSUnknown Predicate Process Queue Socket Stack Stream   \
		Stream2 Term TermChars TermRead TermWrite Thread Unify  \
		Unit Util VarDictionary YourExtensions

PREFIX = /usr
CC = gcc
CFLAGS = -O
WFLAGS = -O -Wall
DEFINES =

APP = cxprolog
VERSION = 0.82
OBJ_DIR = obj

define OBJS
	$(addprefix $(OBJ_DIR)/,                                    \
		$(addsuffix .o, $(foreach file, $(NAMES), $(file)))     \
	)
endef

$(APP): $(OBJ_DIR) $(OBJS)
	$(CC) $(CFLAGS) -o $(APP) $(OBJS) -lm

$(OBJ_DIR)/%.o: %.c
	$(CC) $(CFLAGS) $(DEFINES) -c $< -o $@

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

clear:
	rm -rf $(OBJ_DIR)
	rm -f $(APP) core ../pl/CxCore.pl ../_CxP

test:
	./$(APP) -cx_test

install:
	cp $(APP) $(PREFIX)/bin/$(APP)

pack:
	cd ../.. ;                                                \
	tar cv $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tar ;    \
	gzip $(APP)-$(VERSION).src.tar
