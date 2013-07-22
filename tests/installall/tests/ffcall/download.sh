#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

if ! [[ -e ffcall.tar.gz ]] ; then
    wget http://www.kiatoa.com/matt/iup/ffcall.tar.gz 
fi

tar xfvz ffcall.tar.gz

ls -l ffcall