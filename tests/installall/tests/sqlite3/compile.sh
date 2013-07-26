#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

cd sqlite-autoconf-$SQLITE3_VERSION
./configure --prefix=$PREFIX

make
