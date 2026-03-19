## tabstops = 4 ##
##############################################################################

    ######################################################################
  ############                                                 #############
############                  CXPROLOG MAKEFILE                  #############
  ############               (amd@di.fct.unl.pt)               #############
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
# For READLINE SUPPORT, uncomment the following line:
#
#	USE_READLINE=y
#
# Nothing more is required.
##############################################################################

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
#	EXT_OBJ_DIR = src/obj/ext
#	EXT_CFLAGS = -I/usr/include
#	EXT_DEFINES = -DUSE_X=2
#	EXT_LIBS = -L/usr/lib -lcrypt
#	EXT_NAMES = ext/file1 ext/file2
##############################################################################

##############################################################################
# The DIRECTORY where CxProlog is INSTALLED by "make install":
#
PREFIX = /usr/local
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
VERSION = 0.97.1
CC = gcc
CPP = g++
LD=ld
CFLAGS = -Wall -O1
DEFINES = -DPREFIX=\"$(PREFIX)\"
LIBS= -lm
OBJ_DIR = src/obj
DEBUG =
NAMES = Alias Array Arith Atom Attention Boot Buffer CallProlog		\
		Character Clause Clock CmdLine CodeGen Compiler				\
		Contexts1 Contexts2 Contexts3 Contexts4 Consult CxProlog	\
		Debug Dict Disassembler Exception Extra File FileSys Flags	\
		GCollection	Gui GuiEvent IVar Index Instructions InterLine	\
		Java JavaAddOns Locale Machine Memory Mesg Net Number		\
		Operator OSMac OSUnix OSUnknown Predicate PredicateProperty	\
		Process Queue Scratch Stack Stream StreamProperty String	\
		SysTrace Term TermRead TermWrite Thread Ucs2 Unify Unit		\
		Util Utf8 Utf16 Utf32 VarDictionary Version WxWidgets		\
		WxWidgetsAddOns WxWidgetsDemo YourExtensions


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
ZPRE =
ifeq ($(USE_READLINE),y)
	ZDEFINES += -DUSE_READLINE
	ZLIBS += -lreadline -lncurses
	ZPRE +=																	\
	 if ! test -f "/usr/include/readline/readline.h"; then					\
	  echo "\n";															\
	  echo "*************************************************************";	\
	  echo "Missing READLINE dependency:";									\
	  echo "  The developement library \"libreadline-dev\" is missing!!!";	\
	  echo "  For READLINE support, you need to install it yourself.";		\
	  echo "  In Debian-based systems it is enough to issue the command:";	\
	  echo "        sudo apt-get install libreadline-dev";					\
	  echo "************************************************************";	\
	  echo "\n";															\
	  exit 1;																\
	 fi;
endif

ifeq ($(USE_JAVA),y)
	ZDEFINES += -DUSE_JAVA $(shell src/java-config --cxxflags)
	ZLIBS += $(shell src/java-config --libs)
	ZPRE +=																	\
	 if ! type javac >/dev/null 2>&1 ; then									\
	  echo "\n";															\
	  echo "*************************************************************";	\
	  echo "Missing JAVA dependency:";										\
	  echo "  The Java development toolkit from Sun is missing!!!";			\
	  echo "  For JAVA support, you need to install it yourself.";			\
	  echo "  On Debian-based systems is enough to issue a command like:";	\
	  echo "        sudo apt-get install sun-java5-jdk";					\
	  echo "  On other sys you must downld and install a package from:";	\
	  echo "        http://www.java.com/en/download/";						\
	  echo "*************************************************************";	\
	  echo "\n";															\
	  exit 1;																\
	 fi;
	ZPOST +=																\
	  echo "\n";															\
	  echo "*************************************************************";	\
	  echo "Mandatory manual post-instalation for JAVA:";					\
	  echo "    - Run 'make install' to ensure that the Java classes of";	\
	  echo "      CxProlog library are where CxProlog expects them to be.";	\
	  echo "*************************************************************";	\
	  echo "\n";
endif

ifeq ($(USE_WXWIDGETS),y)
	ZDEFINES += -DUSE_WXWIDGETS $(shell wx-config --cxxflags)
	ZLIBS += $(shell wx-config --libs)
	ZPRE +=																	\
	 if		! type wx-config >/dev/null 2>&1								\
		 ||	[ `wx-config --release` != "2.4" ]; then						\
	  echo "\n";															\
	  echo "*************************************************************";	\
	  echo "Missing WXWIDGETS dependency:";									\
	  echo "  The developement library \"libwxgtk2.4-dev\" is missing!!!";	\
	  echo "  For WXWIDGETS support, you need to install it yourself.";		\
	  echo "  In Debian-based systems it is enough to issue the command:";	\
	  echo "        sudo apt-get install libwxgtk2.4-dev";					\
	  echo "*************************************************************";	\
	  echo "\n";															\
	  exit 1;																\
	 fi;
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

$(APP): $(OBJ_DIR) $(OBJ_DIR)/validate $(OBJS)
	$(CPP) -o $(APP) $(OBJS) $(ZLIBS)
	@$(ZPOST)

$(OBJ_DIR)/%.o: src/%.c
	$(CC) $(ZFLAGS) $(ZDEFINES) -c $< -o $@

$(OBJ_DIR)/%.o: src/%.cpp
	$(CPP) $(ZFLAGS) $(ZDEFINES) -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR) $(EXT_OBJ_DIR)

$(OBJ_DIR)/validate:
	@$(ZPRE)
	touch $(OBJ_DIR)/validate


##############################################################################
# Build CxProlog as a shared library
##############################################################################

W = -Wl,
ZLIBS2 = $(subst -pthread,,$(subst $(W),,$(ZLIBS)))

libcxprolog.so: $(OBJ_DIR) $(OBJS)
	$(LD) -o libcxprolog.so -shared $(OBJS) -lc $(ZLIBS2)

# USE_WXWIDGETS must be off, for this to work
cxprolog_shared: libcxprolog.so
	$(CPP) -o cxprolog_shared -L. -lcxprolog

lib: libcxprolog.so

shared: cxprolog_shared

run_shared: cxprolog_shared
	LD_LIBRARY_PATH=. cxprolog_shared


##############################################################################
# Utilities
##############################################################################

clean:
	rm -f $(APP) libcxprolog.so cxprolog_shared
	rm -rf $(OBJ_DIR) core pl/CxCore.pl
	find . -type f -name '*.bak' -exec rm -f {} \;
	find . -type f -name '*.class' -exec rm -f {} \;

test: $(APP)
	./$(APP) --check

install: $(APP)
	rm -rf $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	mkdir -p $(PREFIX) $(PREFIX)/bin/
	mkdir -p $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	cp $(APP) $(PREFIX)/bin/
	cp -pR lib/cxprolog/* $(PREFIX)/lib/cxprolog
	cp -pR examples pl C* I* MA* R* $(PREFIX)/share/cxprolog

uninstall:
	rm -rf $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	rm -f $(PREFIX)/bin/$(APP)

dist: clean
	cd ..; tar cvz $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tgz

distzip: clean
	cd ..; zip -r $(APP)-$(VERSION).zip $(APP)-$(VERSION) > /dev/null


##############################################################################
# THE END
##############################################################################
