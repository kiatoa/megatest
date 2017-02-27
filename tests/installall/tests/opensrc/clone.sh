#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh

parentdir=$MT_LINKTREE/$MT_TARGET/$MT_RUNNAME

lockfile $parentdir/clone.lock
if [ ! -e $parentdir/opensrc.fossil ];then
  fossil clone http://www.kiatoa.com/fossils/opensrc $parentdir/opensrc.fossil
fi

if [ ! -e $parentdir/src/dbi ];then
  mkdir -p $parentdir/src
  (cd $parentdir/src;fossil open $parentdir/opensrc.fossil --nested)
else
  (cd $parentdir/src;fossil sync;fossil co trunk;fossil status)
fi
rm -f $parentdir/clone.lock

ln -sf $parentdir/src $MT_TEST_RUN_DIR/src

