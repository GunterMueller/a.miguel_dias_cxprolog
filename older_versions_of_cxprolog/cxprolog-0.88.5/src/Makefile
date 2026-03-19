NAMES = Array Arith Atom Attention Boot Buffer CallProlog Clause	\
	    Clock CmdLine CodeGen Compiler CxProlog Debug Dict			\
		Disassembler Exception Extra FileSys Flags GCollection		\
		Instructions IVar Index Machine Memory Mesg Net Number		\
		Operator OSMac OSUnix OSUnknown Predicate Process Queue		\
		Scratch Stack Stream SysTrace Term TermChars TermRead		\
		TermWrite Thread Unify Unit Util VarDictionary				\
		YourExtensions

PREFIX = /usr/local
CC = gcc
CFLAGS = -O
WFLAGS = -O -Wall
DEFINES =

APP = cxprolog
VERSION = 0.88.5
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

clean:
	rm -rf $(OBJ_DIR)
	rm -f $(APP) core ../pl/CxCore.pl *.bak ../*.bak ../pl/*.bak

test:
	./$(APP) -check

install:
	cp $(APP) $(PREFIX)/bin/$(APP)

pack:
	cd ../.. ;                                                \
	tar cv $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tar ;    \
	gzip $(APP)-$(VERSION).src.tar
