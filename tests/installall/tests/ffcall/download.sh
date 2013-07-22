#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

if ! [[ -e i${DOWNLOADS}/ffcall.tar.gz ]] ; then
    (cd ${DOWNLOADS};wget http://www.kiatoa.com/matt/iup/ffcall.tar.gz )
fi

tar xfvz ${DOWNLOADS}/ffcall.tar.gz

ls -l ffcall
