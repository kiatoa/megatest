#!/usr/bin/env bash

# Run your step here

CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" $PREFIX/bin/chicken-install $PROX sqlite3
