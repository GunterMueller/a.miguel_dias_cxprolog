
# For JAVA SUPPORT you need to uncomment the TWO following lines:
#   JAVA_LIB= -L/usr/java/j2/jre/lib/i386/client/ -ljvm
#   JAVA_DEFINE = -DUSE_JAVA
#
# For JAVA SUPPORT you also need to define LD_LIBRARY_PATH and CLASSPATH
# inside '.bashrc'. For example:
#   export LD_LIBRARY_PATH=/usr/java/j2/jre/lib/i386/:/usr/java/j2/jre/lib/i386/client/
#   export CLASSPATH=/usr/local/lib/cxprolog:.
#
# For JAVA SUPPORT you finally need to install cxprolog using 'make install'.
#
# Note: you may need to adjust some of the previous path names.

# For GUI SUPPORT uncomment ONE of the following line:
#   GUI_DEFINE = -DGUI_JAVA
#   GUI_DEFINE = -DGUI_WXWIDGETS



PREFIX = /usr/local
APP = cxprolog
VERSION = 0.90.6
CC = gcc
LD=ld
CFLAGS = -Wall -O1
DEFINES =
LIBS= -lm
OBJ_DIR = obj

NAMES = Array Arith Atom Attention Boot Buffer CallProlog Clause	\
		Clock CmdLine CodeGen Compiler CxProlog Debug Dict			\
		Disassembler Exception Extra FileSys Flags GCollection		\
		GuiJava GuiUnknown GuiWXWidgets Instructions IVar Index		\
		Java Locale Machine Memory Mesg Net Number Operator			\
		OSMac OSUnix OSUnknown Predicate Process Queue Scratch		\
		Stack Stream String SysTrace Term TermChars TermRead		\
		TermWrite Thread Unify Unit Util Utf8 VarDictionary			\
		Version WXWidgets YourExtensions

define OBJS
	$(addprefix $(OBJ_DIR)/,										\
		$(addsuffix .o, $(foreach file, $(NAMES), $(file)))			\
	)
endef

$(APP): $(OBJ_DIR) $(OBJS)
	$(CC) -o $(APP) $(CFLAGS) $(OBJS) $(LIBS) $(JAVA_LIB)

lib: $(OBJ_DIR) $(OBJS)
	$(LD) -o libcxprolog.so -shared $(OBJS) -lc $(LIBS) $(JAVA_LIB)
	
shared: lib
	gcc -o cxprolog_shared -L. -lcxprolog

$(OBJ_DIR)/%.o: %.c Configure.h
	$(CC) $(CFLAGS) -DPREFIX=\"$(PREFIX)\" $(DEFINES) $(JAVA_DEFINE) $(GUI_DEFINE) -c $< -o $@

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

Configure.h: Configure.c
	$(CC) -o Configure Configure.c
	./Configure

clean:
	rm -rf $(OBJ_DIR)
	rm -f $(APP) libcxprolog.so cxprolog_shared
	rm -f Configure Configure.h
	rm -f core ../pl/CxCore.pl *.bak ../*.bak ../pl/*.bak

test: $(APP)
	./$(APP) -check

install: $(APP)
	cp $(APP) $(PREFIX)/bin/$(APP)
	cp -r ../lib $(PREFIX)/lib/cxprolog
	mkdir -p $(PREFIX)/share/cxprolog
	cp ../C* ../I* ../M* ../R* $(PREFIX)/share/cxprolog
	cp -r ../examples $(PREFIX)/share/cxprolog/

dist: clean
	cd ../.. ;												\
	tar cvz $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tgz
