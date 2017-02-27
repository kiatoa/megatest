#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh
# source $PREFIX

cd chicken-${CHICKEN_VERSION}
make PLATFORM=${PLATFORM} PREFIX=${PREFIX} install

ls -l ${PREFIX}/bin
