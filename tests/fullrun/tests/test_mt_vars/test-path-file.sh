#!/bin/bash


# get a previous test
export EZFAILPATH=`$MT_MEGATEST -test-files envfile.txt -target $MT_TARGET :runname $MT_RUNNAME -testpatt runfirst/a%`

echo "Found |$EZFAILPATH|"

if [ -e $EZFAILPATH ];then
  echo All good!
else
  echo NOT good!
  exit 1
fi

export EZFAILPATH2=`$MT_MEGATEST -test-paths -target $MT_TARGET :runname $MT_RUNNAME -testpatt runfirst/a%`

echo "Found |$EZFAILPATH2|"

if [ -e $EZFAILPATH2 ];then
  echo All good!
else
  echo NOT good!
  exit 1
fi


exit 0
