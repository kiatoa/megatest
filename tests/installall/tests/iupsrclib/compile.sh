#!/usr/bin/env bash

# Run your step here

pkg=$1

source $PREFIX/buildsetup.sh

export LUA_SUFFIX=
export LUA_INC=$MT_TEST_RUN_DIR/lua52/include

if [[ $pkg == "lua52" ]]; then
    (cd $pkg/src;make $PLATFORM)
else
    (cd $pkg/src;make)
fi

