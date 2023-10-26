NAMES = Array Arith Atom Attention Boot Buffer CallProlog Clause	\
	    Clock CmdLine CodeGen Compiler CxProlog Debug Dict			\
		Disassembler Exception Extra FileSys Flags GCollection		\
		Instructions IVar Index Machine Memory Mesg Net Number		\
		Operator OSMac OSUnix OSUnknown Predicate Process Queue		\
		Scratch Stack Stream SysTrace Term TermChars TermRead		\
		TermWrite Thread Unify Unit Util VarDictionary Version		\
		YourExtensions

PREFIX = /usr/local
CC = gcc
CFLAGS = -Werror -Wall -O1
DEFINES =

APP = cxprolog
VERSION = 0.89
OBJ_DIR = obj

define OBJS
	$(addprefix $(OBJ_DIR)/,                                    \
		$(addsuffix .o, $(foreach file, $(NAMES), $(file)))     \
	)
endef

$(APP): $(OBJ_DIR) $(OBJS)
	$(CC) $(CFLAGS) -o $(APP) $(OBJS) -lm

$(OBJ_DIR)/%.o: %.c Configure.h
	$(CC) $(CFLAGS) $(DEFINES) -c $< -o $@

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

Configure.h: Configure.c
	$(CC) -o Configure Configure.c
	./Configure
	rm -f Configure

clean:
	rm -rf $(OBJ_DIR)
	rm -f $(APP)
	rm -f Configure Configure.h
	rm -f core ../pl/CxCore.pl *.bak ../*.bak ../pl/*.bak

test: $(APP)
	./$(APP) -check

install: $(APP)
	cp $(APP) $(PREFIX)/bin/$(APP)

dist: clean
	cd ../.. ;                                                \
	tar cvz $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tgz
