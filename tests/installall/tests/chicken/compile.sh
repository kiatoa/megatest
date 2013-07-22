#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

cd chicken-${CHICKEN_VERSION}
make PLATFORM=${PLATFORM} PREFIX=${PREFIX}
