#!/usr/bin/env bash

# Run your step here

cd sqlite-autoconf-$SQLITE3_VERSION
./configure --prefix=$PREFIX

make
