#!/bin/bash

set -x

if [[ $DEPLOYTARG == "" ]] ; then
    echo Installing into deploytarg
    export DEPLOYTARG=$PWD/deploytarg
fi

# Make the deploy dir
mkdir -p $DEPLOYTARG

if [[ $proxy == "" ]]; then 
  echo 'Please set the environment variable "proxy" to host.com:port (e.g. foo.com:1234) if you need to use a proxy'
  echo PROX=""
else
  export http_proxy=http://$proxy
  export PROX="-proxy $proxy"
fi

export CHICKENINSTDIR=$(dirname $(dirname $(type -p csi)))

# First copy in the needed iup, sqlite3 and zmq libraries
cp $CHICKENINSTDIR/lib/lib{zmq,uuid}* $DEPLOYTARG
cp $CHICKENINSTDIR/lib/libchicken.* $DEPLOYTARG

# Then run the deploy for all needed # Some eggs are quoted since they are reserved to Bash
for f in matchable readline apropos base64 regex-literals format "regex-case" "test" coops \
         trace csv dot-locking posix-utils posix-extras directory-utils hostinfo tcp rpc \
         csv-xml fmt json md5 iup canvas-draw ; do
  if ! [[ -e $DEPLOYTARG/$f.so ]];then
    chicken-install $PROX -deploy $f -prefix $DEPLOYTARG
    # chicken-install -deploy -prefix $DEPLOYTARG $PROX $f
  else
    echo Skipping install of egg $f as it is already installed
  fi
done

export CSC_OPTIONS="-I$CHICKENINSTDIR/include -L$DEPLOYTARG" 
chicken-install -deploy zmq -prefix $DEPLOYTARG
chicken-install -deploy sqlite3 -prefix $DEPLOYTARG

make $DEPLOYTARG/megatest
make $DEPLOYTARG/dashboard
