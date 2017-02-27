#!/bin/bash -e
targpath=`megatest -test-paths -target $MT_TARGET :runname $MT_RUNNAME -testpatt executables/megatest`
chicken-install -prefix $targpath/megatest -deploy $EGGNAME
echo DONE
