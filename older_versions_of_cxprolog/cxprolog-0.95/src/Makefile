##############################################################################

    ######################################################################
  ############                                                 #############
############                  CXPROLOG MAKEFILE                  #############
  ############                                                 #############
    ######################################################################

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


    ######################################################################
  ############                                                 #############
############            USER AREA (FOR CONFIGURATION)            #############
  ############                                                 #############
    ######################################################################

##############################################################################
# For JAVA SUPPORT, uncomment the following line:
#
#	USE_JAVA=y
#
# But this is not enough. You also need to define CLASSPATH inside your
#  '~/.bashrc' file. At least include the two directories of this example:
#
#   export CLASSPATH=/usr/local/lib/cxprolog:.
#
# Finally, you need to install cxprolog using 'make install'. This ensures
# that certain Java classes that CxProlog depends on are installed in the
# proper places. If you don't install, the Java component will not run!
##############################################################################

##############################################################################
# For WXWIDGETS SUPPORT, uncomment the following line:
#
#	USE_WXWIDGETS=y
#
# Nothing more is required.
# You are allowed to activate Java and WXWidgets at the same time.
##############################################################################

##############################################################################
# For PLATFORM INDEPENDENT GUI SUPPORT uncomment ONE of these:
# (* not yet implemented *)
#	USE_GUI = java
#	USE_GUI = wxwidgets
##############################################################################

##############################################################################
# For ALTERNATIVE CONTEXTS SEMANTICS uncomment ONE of these (default is 1):
#	USE_CTX=1
#	USE_CTX=2
#	USE_CTX=3
#	USE_CTX=4
# Some of these alternatives correspond to internal projects and, therefore,
# are not available in the public distribution of CxProlog.
##############################################################################

##############################################################################
# For ADDICIONAL object files, libraries and options concerned with
# YOUR PROLOG EXTENSIONS, define your own stuff here:
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
# The DIRECTORY where CxProlog is INSTALLED by "make install":
#
PREFIX = /usr/local/
##############################################################################



    ######################################################################
  ############                                                 #############
############   RESERVED AREA -  DON'T CHANGE ANYTHING BELOW!!!   #############
  ############                                                 #############
    ######################################################################

##############################################################################
# Base configuration
##############################################################################

APP = cxprolog
VERSION = 0.95
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
		Contexts1 Contexts2 Contexts3 Contexts4 Consult CxProlog	\
		Debug Dict Disassembler Exception Extra File FileSys Flags	\
		GCollection	Gui GuiEvent Instructions IVar Index Java		\
		JavaAddOns Locale Machine Memory Mesg Net Number Operator	\
		OSMac OSUnix OSUnknown Predicate PredicateProperty Process	\
		Queue Scratch Stack Stream StreamProperty String SysTrace	\
		Term TermRead TermWrite Thread Unify Unit Util Utf8			\
		VarDictionary Version WxWidgets WxWidgetsAddOns				\
		WxWidgetsDemo YourExtensions


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
	ZDEFINES += -DUSE_JAVA $(shell java-config --cxxflags)
	ZLIBS += $(shell java-config --libs)
	JDEP = ../lib/CxProlog.class
endif

ifeq ($(USE_WXWIDGETS),y)
	ZDEFINES += -DUSE_WXWIDGETS $(shell wx-config --cxxflags)
	ZLIBS += $(shell wx-config --libs)
endif

ifeq ($(USE_GUI),java)
	ZDEFINES += -DGUI_JAVA
endif
ifeq ($(USE_GUI),wxwidgets)
	ZDEFINES += -DGUI_WXWIDGETS
endif

ifdef USE_CTX
	ZDEFINES += -DCONTEXTS=$(USE_CTX)
endif


##############################################################################
# Build CxProlog as an application
##############################################################################

$(APP): $(JDEP) $(OBJ_DIR) $(OBJS)
	$(CPP) -o $(APP) $(OBJS) $(ZLIBS)

$(OBJ_DIR)/%.o: %.c
	$(CC) $(ZFLAGS) $(ZDEFINES) -c $< -o $@

$(OBJ_DIR)/%.o: %.cpp
	$(CPP) $(ZFLAGS) $(ZDEFINES) -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR) $(EXT_OBJ_DIR)

$(JDEP): ../lib/cxprolog/CxProlog.java
	javac ../lib/cxprolog/*.java ../examples/java/*/*.java


##############################################################################
# Build CxProlog as a shared library
##############################################################################

W = -Wl,
ZLIBS2 = $(subst -pthread,,$(subst $(W),,$(ZLIBS)))

libcxprolog.so: $(JDEP) $(OBJ_DIR) $(OBJS)
	$(LD) -o libcxprolog.so -shared $(OBJS) -lc $(ZLIBS2)

	# USE_WXWIDGETS must be off, for this to work
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
	rm -f ../examples/*/*.class ../lib/*/*.class

test: $(APP)
	./$(APP) -check

install: $(APP)
	cp $(APP) $(PREFIX)bin/
	rm -rf $(PREFIX)lib/cxprolog $(PREFIX)share/cxprolog
	mkdir -p $(PREFIX)lib/cxprolog $(PREFIX)share/cxprolog
	cp -pR ../lib/cxprolog/* $(PREFIX)lib/cxprolog
	cp -pR ../examples ../C* ../I* ../M* ../R* $(PREFIX)share/cxprolog

dist: clean
	cd ../.. ; tar cvz $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tgz


##############################################################################
# THE END
##############################################################################
