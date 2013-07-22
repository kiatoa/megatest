#!/usr/bin/env bash

# Run your step here

cd chicken-${CHICKEN_VERSION}
make PLATFORM=${PLATFORM} PREFIX=${PREFIX} install

echo "export PATH=$PREFIX/bin:\$PATH" > $PREFIX/setup-chicken4x.sh
echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> $PREFIX/setup-chicken4x.sh

echo "setenv PATH $PREFIX/bin:\$PATH" > $PREFIX/setup-chicken4x.csh
echo "setenv LD_LIBRARY_PATH $LD_LIBRARY_PATH" >> $PREFIX/setup-chicken4x.csh

ls -l ${PREFIX}/bin