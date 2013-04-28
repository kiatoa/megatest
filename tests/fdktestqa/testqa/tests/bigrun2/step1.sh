#!/bin/sh
prev_test=`$MT_MEGATEST -test-paths -target $MT_TARGET :runname $MT_RUNNAME -testpatt bigrun/$NUMBER`
if [ -e $prev_test/testconfig ]; then
  exit 0
else
  exit 1
fi
