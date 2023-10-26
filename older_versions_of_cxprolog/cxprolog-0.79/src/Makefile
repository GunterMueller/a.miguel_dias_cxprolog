NAMES = Arith Atom Buffer Clause Clock CodeGen Compiler         \
        CxProlog Debug Dictionary Disassembler FileSys Flags    \
        GCollection HashTable Interrupt IVar Index  Machine     \
        Memory Mesg    Number Operator Predicate Queue Stack    \
        Stream Term TermChars TermRead TermWrite Thread Unify   \
        Unit UnixServices Util VarDictionary YourExtensions

PREFIX = /usr
CC = gcc
CFLAGS = -O
DEFINES =

APP = cxprolog
VERSION = 0.79
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
	rm -f $(APP) core ../pl/CxCore.pl ../_CxP

test:
	./$(APP) -cx_test

install:
	cp $(APP) $(PREFIX)/bin/$(APP)

pack:
	cd ../.. ;                                                  \
	tar cv $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tar ;      \
	gzip $(APP)-$(VERSION).src.tar
