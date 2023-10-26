#!/bin/bash

HOME=/home/amd/development/CxProlog
LIB=$HOME/lib/cxprolog/java/prolog.jar
javac -cp $LIB *java
LD_LIBRARY_PATH=$HOME java -cp $LIB:..:. ticktack3.MyJFrame
rm -f *.class
