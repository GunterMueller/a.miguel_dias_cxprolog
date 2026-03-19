##############################################################################
#     CxProlog Makefile
##############################################################################

##############################################################################
# USER: For JAVA SUPPORT, uncomment the following line:
#
#	USE_JAVA=y
#
# You also need to define CLASSPATH inside your '~/.bashrc'. At least include
# the two directories of this example:
#
#   export CLASSPATH=/usr/local/lib/cxprolog:.
#
# Finally, you need to install cxprolog using 'make install', to make sure
# that certain Java classes are installed in certain places.
##############################################################################

##############################################################################
# USER: For WXWIDGETS SUPPORT, uncomment the following line:
#
#	USE_WXWIDGETS=y
#
# Nothing more is required.
# You can activate both, Java and WXWidgets.
##############################################################################

##############################################################################
# USER: For PLATFORM INDEPENDENT GUI SUPPORT uncomment ONE of these:
# (* not yet implemented *)
#	USE_GUI = java
#	USE_GUI = wxwidgets
##############################################################################

##############################################################################
# USER: For ALTERNATIVE CONTEXTS SEMANTICS uncomment ONE of these:
# (* not yet implemented *)
#	USE_CTX=0
#	USE_CTX=1
#	USE_CTX=2
##############################################################################

##############################################################################
# USER: For ADDICIONAL object files, libraries and options concerned with
# YOUR OWN EXTENSIONS, define extension stuff here:
#
EXT_OBJ_DIR =
EXT_CFLAGS =
EXT_DEFINES =
EXT_LIBS =
EXT_NAMES =
#
# Example:
#	EXT_OBJ_DIR = obj/ext
#	EXT_CFLAGS = -I/usr/include
#	EXT_DEFINES = -DUSE_X=2
#	EXT_LIBS = -L/usr/lib -lcrypt
#	EXT_NAMES = ext/file1 ext/file2
##############################################################################


##############################################################################
# Base configuration
##############################################################################

PREFIX = /usr/local/
APP = cxprolog
VERSION = 0.93
CC = gcc
CPP = g++
LD=ld
CFLAGS = -Wall -O1
DEFINES = -DPREFIX=\"$(PREFIX)\"
LIBS= -lm
OBJ_DIR = obj
DEBUG =
NAMES = Array Arith Atom Attention Boot Buffer CallProlog			\
		Character Clause Clock CmdLine CodeGen Compiler				\
		Contexts1 Contexts2 Consult CxProlog Debug Dict				\
		Disassembler Exception Extra FileSys Flags GCollection		\
		GuiEvent GuiJava GuiWXWidgets GuiUnknown Instructions IVar	\
		Index Java Locale Machine Memory Mesg Net Number Operator	\
		OSMac OSUnix OSUnknown Predicate PredicateProperty Process	\
		Queue Scratch Stack Stream StreamProperty String SysTrace	\
		Term TermRead TermWrite Thread Unify Unit Util Utf8			\
		VarDictionary Version WXWidgets WXWidgetsDemo YourExtensions


##############################################################################
# Computed configuration
##############################################################################

define OBJS
	$(addprefix $(OBJ_DIR)/,												\
		$(addsuffix .o, $(foreach file, $(NAMES) $(EXT_NAMES), $(file)))	\
	)
endef

ZFLAGS = $(DEBUG) $(CFLAGS) $(EXT_CFLAGS)
ZDEFINES = $(DEFINES) $(EXT_DEFINES)
ZLIBS = $(LIBS) $(EXT_LIBS)

ifeq ($(USE_JAVA),y)
	ZDEFINES += -DUSE_JAVA $(shell sh java-config --cxxflags)
	ZLIBS += $(shell sh java-config --libs)
	JDEP = ../lib/CxProlog.class
endif

ifeq ($(USE_WXWIDGETS),y)
	ZDEFINES += -DUSE_WXWIDGETS $(shell wx-config --cxxflags)
	ZLIBS += $(shell wx-config --libs)
endif

ifeq ($(USE_GUI),java)
	ZDEFINES += -DGUI_JAVA
else ifeq ($(USE_GUI),wxwidgets)
	ZDEFINES += -DGUI_WXWIDGETS
endif

ifdef USE_CTX
	ZDEFINES += -DCONTEXTS=$(USE_CTX)
endif


##############################################################################
# Build CxProlog
##############################################################################

$(APP): $(JDEP) $(OBJ_DIR) $(OBJS)
	$(CPP) -o $(APP) $(OBJS) $(ZLIBS)

$(OBJ_DIR)/%.o: %.c Configure.h
	$(CC) $(ZFLAGS) $(ZDEFINES) -c $< -o $@

$(OBJ_DIR)/%.o: %.cpp
	$(CPP) $(ZFLAGS) $(ZDEFINES) -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR) $(EXT_OBJ_DIR)

$(JDEP): ../lib/CxProlog.java
	javac ../lib/*.java ../examples/*/*.java

Configure.h: Configure.c
	$(CC) -o Configure Configure.c
	./Configure


##############################################################################
# Build CxProlog as a shared library
##############################################################################

W = -Wl,
ZLIBS2 = $(subst -pthread,,$(subst $(W),,$(ZLIBS)))

libcxprolog.so: $(JDEP) $(OBJ_DIR) $(OBJS)
	$(LD) -o libcxprolog.so -shared $(OBJS) -lc $(ZLIBS2)

	# USE_WXWIDGETS must be off for this to work
cxprolog_shared: libcxprolog.so
	$(CPP) -o cxprolog_shared -L. -lcxprolog

lib: libcxprolog.so

shared: cxprolog_shared

##############################################################################
# Utilities
##############################################################################

clean:
	rm -rf $(OBJ_DIR)
	rm -f $(APP) libcxprolog.so cxprolog_shared
	rm -f Configure Configure.h
	rm -f core ../pl/CxCore.pl *.bak ../*.bak ../pl/*.bak
	rm -f ../examples/*/*.class ../lib/*.class

test: $(APP)
	./$(APP) -check

install: $(APP)
	cp $(APP) $(PREFIX)/bin/
	rm -rf $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	mkdir -p $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	cp -a ../lib/* $(PREFIX)/lib/cxprolog
	cp -a ../examples ../C* ../I* ../M* ../R* $(PREFIX)/share/cxprolog

dist: clean
	cd ../.. ; tar cvz $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tgz


##############################################################################
# THE END
##############################################################################
