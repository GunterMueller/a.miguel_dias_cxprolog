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
#	READLINE=y
#
# Nothing more is required.
##############################################################################

##############################################################################
# For JAVA SUPPORT, uncomment the following line:
#
#	JAVA=y
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
#	WXWIDGETS=y
#
# Nothing more is required.
# You are allowed to activate Java and WXWidgets at the same time.
##############################################################################

##############################################################################
# For PLATFORM INDEPENDENT GUI SUPPORT uncomment ONE of these:
# (* not yet implemented *)
#	GUI = java
#	GUI = wxwidgets
##############################################################################

##############################################################################
# For ALTERNATIVE CONTEXTS SEMANTICS uncomment ONE of these (default is 1):
#	CTX=1
#	CTX=2
#	CTX=3
#	CTX=4
# Some of these alternatives correspond to internal projects and, therefore,
# are not available in the public distribution of CxProlog.
##############################################################################

##############################################################################
# For VALGRIND SUPPORT, uncomment the following line:
#	VALGRIND=y
# The end-used should not use this option. This is only for
# debugging CxProlog itself.
##############################################################################

##############################################################################
# For ADDITIONAL object files, libraries and options concerned with
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
VERSION = 0.97.4
CC = gcc
CPP = g++
LD=ld
CFLAGS = -Wall -W -O1
CEXTRAFLAGS = -Wmissing-prototypes
DEFINES =
LIBS= -lm 
OBJ_DIR = src/obj
DEBUG =
NAMES = Alias Array Arith Atom Attention Boot Buffer CallProlog		\
		Character Clause Clock CmdLine CodeGen Compiler				\
		Contexts1 Contexts2 Contexts3 Contexts4 Consult CxProlog	\
		Debug Dict Disassembler Exception Extra File FileSys Flags	\
		GCollection Gui GuiEvent IVar Index Instructions InterLine	\
		Java JavaAddOns Locale Machine Memory Mesg Net Number		\
		Operator OSUnix OSWin OSUnknown Predicate					\
		PredicateProperty Process Queue Scratch Stack Stream		\
		StreamProperty String SysTrace Table TestLib Term TermRead	\
		TermWrite Thread Ucs2 Unify Unit Util Utf8 Utf16 Utf32		\
		VarDictionary Version WxWidgets WxWidgetsAddOns				\
		WxWidgetsDemo YourExtensions
OS = $(shell uname | cut -c 1-7)
MODEL =


##############################################################################
# Computed configuration
##############################################################################

define OBJS
	$(addprefix $(OBJ_DIR)/,												\
		$(addsuffix .o, $(foreach file, $(NAMES) $(EXT_NAMES), $(file)))	\
	)
endef

ifeq ($(OS),MINGW32)
    ZPREFIX=
else
    ZPREFIX=-DPREFIX=\"$(PREFIX)\"
endif

ZFLAGS = $(DEBUG) $(EXT_CFLAGS)
ZDEFINES = $(MODEL) $(ZPREFIX) -DOS=$(OS) $(DEFINES) $(EXT_DEFINES)
ZLIBS = $(MODEL) $(LIBS) $(EXT_LIBS)
ZPRE =

ifeq ($(READLINE),y)
  ifeq ($(OS),MINGW32) # Do nothing: a READLINE is already built-in in the console

  else # Assume Unix
	ZDEFINES += -DUSE_READLINE
    ifeq ($(OS),Darwin)
	    ZLIBS += -L/opt/local/lib/	
	endif
	ZLIBS += -lreadline -lncurses
	ZPRE +=																	\
	 if ! test -f "/usr/include/readline/readline.h"; then					\
	  echo "\n";															\
	  echo "*************************************************************";	\
	  echo "Missing READLINE dependency:";									\
	  echo "  The development library \"libreadline-dev\" is missing!!!";	\
	  echo "  For READLINE support, you need to install it yourself.";		\
	  echo "  On Debian-based systems it is enough to issue the command:";	\
	  echo "        sudo apt-get install libreadline-dev";					\
	  echo "  On the MacOS X it is enough to issue the command:";			\
	  echo "        sudo port install readline";							\
	  echo "************************************************************";	\
	  echo "\n";															\
	  exit 1;																\
	 fi;
  endif
endif

ifeq ($(JAVA),y)
  ifeq ($(OS),MINGW32)
	ZDEFINES += -DUSE_JAVA $(shell src/java-config-mingw --cxxflags)
	ZLIBS += $(shell src/java-config-mingw --libs)

  else # Assume Unix
	ZDEFINES += -DUSE_JAVA $(shell src/java-config --cxxflags)
	ZLIBS += $(shell src/java-config --libs)
  endif
	
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
	ZPOST_disable +=														\
	  echo "\n";															\
	  echo "*************************************************************";	\
	  echo "Mandatory manual post-installation for JAVA:";					\
	  echo "    - Run 'make install' to ensure that the Java classes of";	\
	  echo "      CxProlog library are where CxProlog expects them to be.";	\
	  echo "*************************************************************";	\
	  echo "\n";
endif

ifeq ($(WXWIDGETS),y)
  ifeq ($(OS),MINGW32)
	ZDEFINES += -DUSE_WXWIDGETS												\
				-mthreads -pipe -fmessage-length=0							\
				-DHAVE_W32API_H -D__WXMSW__ -DWXUSINGDLL -DwxUSE_UNICODE	\
				-IC:/SourceCode/Libraries/wxWidgets2.8/lib/gcc_dll/mswu		\
				-IC:/SourceCode/Libraries/wxWidgets2.8/include
	ZLIBS += -enable-auto-import -export-all-symbols -mthreads				\
				-LC:/SourceCode/Libraries/wxWidgets2.8/lib/gcc_dll -lwxmsw28u

  else # Assume Unix
	ZDEFINES += -DUSE_WXWIDGETS $(shell wx-config --unicode --cxxflags)
	ZLIBS += $(shell wx-config --unicode --libs)
	ZPRE +=																	\
	 if ! type wx-config >/dev/null 2>&1									\
		 || [ `wx-config --release` != "2.8" ]; then						\
	  echo "\n";															\
	  echo "*************************************************************"; \
	  echo "Missing WXWIDGETS dependency:";									\
	  echo "  The developement library \"libwxgtk2.8-dev\" is missing!!!";  \
	  echo "  For WXWIDGETS support, you need to install it yourself.";		\
	  echo "  In Debian-based systems it is enough to issue the command:";  \
	  echo "        sudo apt-get install libwxgtk2.8-dev";					\
	  echo "*************************************************************"; \
	  echo "\n";															\
	  exit 1;																\
	 fi;
  endif
endif

ifeq ($(GUI),java)
	ZDEFINES += -DGUI_JAVA
endif
ifeq ($(GUI),wxwidgets)
	ZDEFINES += -DGUI_WXWIDGETS
endif

ifdef CTX
	ZDEFINES += -DCONTEXTS=$(CTX)
endif

ifeq ($(VALGRIND),y)
	ZDEFINES += -DUSE_VALGRIND
	ZFLAGS += -O0 -g
else
	ZFLAGS += $(CFLAGS)
endif


##############################################################################
# Build CxProlog as an application
##############################################################################

$(APP): $(OBJ_DIR) $(OBJ_DIR)/validate $(OBJS)
	$(CPP) -o $(APP) $(OBJS) $(ZLIBS)
	@$(ZPOST)

$(OBJ_DIR)/%.o: src/%.c
	$(CC) $(ZFLAGS) $(ZDEFINES) $(CEXTRAFLAGS) -c $< -o $@

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

ifeq ($(OS),MINGW32)
  libcxprolog.a cxprolog.dll: $(OBJ_DIR) $(OBJS)
	$(CPP) -shared -o cxprolog.dll $(OBJS) -Wl,--out-implib,libcxprolog.a $(ZLIBS)
	
  cxprolog_shared.exe: libcxprolog.a cxprolog.dll
  ifeq ($(WXWIDGETS),y)
	$(CPP) -o cxprolog_shared.exe -lmingw32 -L./ -lcxprolog
  else
	$(CPP) -o cxprolog_shared.exe -L./ -lcxprolog
  endif
	
  cxprolog_testlib.exe: libcxprolog.a cxprolog.dll
	gcc -DTESTLIB=1 -o src/TestLib.o -c src/TestLib.c
	$(CPP) -enable-auto-import -o cxprolog_testlib src/TestLib.o -L./ -lcxprolog
	rm src/TestLib.o
	
lib: libcxprolog.a cxprolog_shared.exe cxprolog_testlib.exe

else # Assume Unix
  libcxprolog.so: $(OBJ_DIR) $(OBJS)
	$(CPP) -shared -o libcxprolog.so $(OBJS) -lc $(ZLIBS)

  cxprolog_shared: libcxprolog.so
	$(CPP) $(MODEL) -o cxprolog_shared -L./ -lcxprolog

  cxprolog_testlib: libcxprolog.so
	gcc $(MODEL) -DTESTLIB=1 -o src/TestLib.o -c src/TestLib.c
	$(CPP) $(MODEL) -o cxprolog_testlib src/TestLib.o -L./ -lcxprolog
	rm src/TestLib.o

lib: libcxprolog.so cxprolog_shared cxprolog_testlib

endif


##############################################################################
# Utilities
##############################################################################

.PHONY : clean
clean:
	rm -f *$(APP)*
	rm -rf $(OBJ_DIR) core pl/CxCore.pl
	find . -type f \( -name '*.bak' -o -name '*.class' \) -exec rm -f {} \;

test: $(APP)
	./$(APP) --check

install: $(APP)
	rm -rf $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	mkdir -p $(PREFIX) $(PREFIX)/bin/
	mkdir -p $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	cp $(APP) $(PREFIX)/bin/
	cp -pR lib/cxprolog/* $(PREFIX)/lib/cxprolog
	cp -pR examples pl C* I* MA* R* $(PREFIX)/share/cxprolog
ifeq ($(OS),MINGW32)
	cp -a wxmsw28u_gcc.dll $(PREFIX)/bin/
endif

.PHONY : uninstall
uninstall:
	rm -rf $(PREFIX)/lib/cxprolog $(PREFIX)/share/cxprolog
	rm -f $(PREFIX)/bin/$(APP)
  ifeq ($(OS),MINGW32)
	rm -f $(PREFIX)/bin/$(APP).exe $(PREFIX)/bin/wxmsw28u_gcc.dll
  endif

dist: clean
	cd ..; tar cvz $(APP)-$(VERSION) > $(APP)-$(VERSION).src.tgz

distzip: clean
	cd ..; zip -r $(APP)-$(VERSION).zip $(APP)-$(VERSION) > /dev/null

#distdll: 
#  .dll .a .h's
    

all: $(APP) lib

.PHONY : full
full:
	make READLINE=y WXWIDGETS=y JAVA=y all

.PHONY : simple
simple:
	make all


##############################################################################
# THE END
##############################################################################
