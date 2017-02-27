#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

lockfile $PREFIX/eggs.lock
$PREFIX/bin/chicken-install $PROX $EGG_NAME
rm -f $PREFIX/eggs.lock
