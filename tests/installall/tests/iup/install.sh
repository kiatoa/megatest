#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh
# source $PREFIX/setup-chicken4x.sh

export CSCLIBS=`echo $LD_LIBRARY_PATH | sed 's/:/ -L/g'`
CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $PREFIX/bin/chicken-install $PROX -D no-library-checks -feature disable-iup-web iup
# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -feature disable-iup-web -deploy -prefix $DEPLOYTARG iup
# iup:1.0.2 
CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $PREFIX/bin/chicken-install $PROX -D no-library-checks canvas-draw
# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -deploy -prefix $DEPLOYTARG canvas-draw
