#!/bin/bash -e

unset LD_LIBRARY_PATH
rm -rf $EXECUTABLE
mkdir $EXECUTABLE
csc -deploy $EXECUTABLE
ls $EXECUTABLE
