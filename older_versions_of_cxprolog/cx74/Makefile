NAMES = Arith Atom Clause Clock CodeGen Compiler CxProlog\
		Debug FileSys Flags HashTable ImperativeVar Index ListCode\
		Machine Memory Mesg  Operator Predicate Queue Stack Stream\
		Term TermChars TermRead TermWrite Thread Unify\
		Unit UnixServices Util VarDictionary Dictionary\
		YourExtensions

OBJ_DIR = obj

define OBJS
    $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(foreach file, $(NAMES), \
	$(file))))
endef

APP=cxprolog
NICK=cx
VERSION=74

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

pack:
	cd .. ;				\
	tar cv $(NICK)$(VERSION) > $(APP).$(VERSION).tar ; \
	gzip $(APP).$(VERSION).tar
