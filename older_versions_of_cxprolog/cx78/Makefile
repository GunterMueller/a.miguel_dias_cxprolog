NAMES = Arith Atom Buffer Clause Clock CodeGen Compiler			\
		CxProlog Debug Dictionary Disassembler FileSys Flags	\
		HashTable IVar Index  Machine Memory Mesg Number		\
		Operator Predicate Queue Stack Stream Term TermChars	\
		TermRead TermWrite Thread Unify Unit UnixServices		\
		Util VarDictionary YourExtensions

OBJ_DIR = obj

define OBJS
    $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(foreach file, $(NAMES), \
	$(file))))
endef

APP=cxprolog
NICK=cx
VERSION=78

CC = gcc 
CFLAGS = -O

$(APP): $(OBJ_DIR) $(OBJS)
	$(CC) $(CFLAGS) -o $(APP) $(OBJS) -lm

$(OBJ_DIR)/%.o: %.c
	$(CC) $(CFLAGS) $(DEFINES) -c $< -o $@

$(OBJ_DIR):
	mkdir obj

clear:
	rm -rf obj
	rm -f core
	rm -f cxprolog
	rm -f cxprolog.make
	rm -f CxCore.pl

pack:
	cd .. ;				\
	tar cv $(NICK)$(VERSION) > $(APP).$(VERSION).tar ; \
	gzip $(APP).$(VERSION).tar
