NAMES = Arith Atom Builtins Clause Clock\
		CodeGen Compiler Debug ImperativeVar HashTable Index\
		Machine Memory ListCode Mesg CxProlog Operator\
		Predicate Stream Term TermChars TermExtra\
		TermRead TermWrite Thread Unify Unit Util VarDictionary\
		YourExtensions

OBJ_DIR = obj

define OBJS
    $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(foreach file, $(NAMES), \
	$(file))))
endef

APP=cxprolog
NICK=cx
VERSION=71

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

pack:
	cd .. ;				\
	tar cv $(NICK)$(VERSION) > $(APP).$(VERSION).tar ; \
	gzip $(APP).$(VERSION).tar
