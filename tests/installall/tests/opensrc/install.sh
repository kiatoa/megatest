#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh
cd src/$MODULE_NAME
chicken-install
