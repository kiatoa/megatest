#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

cd ffcall
./configure --prefix=${PREFIX} --enable-shared
make
