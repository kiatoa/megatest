#!/usr/bin/env bash

# Run your step here
cd ffcall
./configure --prefix=${PREFIX} --enable-shared
make
