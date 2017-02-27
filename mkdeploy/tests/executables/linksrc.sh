#!/bin/bash -e

rm -f *.scm *.o Makefile
ln -s $BUILDDIR/*.scm .
ln -s $BUILDDIR/Makefile .
ls Makefile *.scm 
