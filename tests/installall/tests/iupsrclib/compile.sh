#!/usr/bin/env bash

# Run your step here

pkg=$1

source $PREFIX/buildsetup.sh

# for pkg in lua52 im cd iup; do
#    megatest -step $pkg :state start :status running
    (cd $pkg/src;make)
#    megatest -step $pkg :state end :status $?
# done
