#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh

# The so files
cp -f im/lib/Linux26g4/*.so $PREFIX/lib
cp -f cd/lib/Linux26g4/*.so $PREFIX/lib
cp -f iup/lib/Linux26g4/*.so $PREFIX/lib

# The development files
mkdir -p $PREFIX/include/im
cp -fR im/include/*.h $PREFIX/include/im
cp -f im/lib/Linux26g4/*.a $PREFIX/lib

mkdir -p $PREFIX/include/cd
cp -f cd/include/*.h $PREFIX/include/cd
cp -f cd/lib/Linux26g4/*.a $PREFIX/lib

mkdir -p /usr/include/iup
cp -f iup/include/*.h $PREFIX/include/iup             
cp -f iup/lib/Linux26g4/*.a $PREFIX/lib     
