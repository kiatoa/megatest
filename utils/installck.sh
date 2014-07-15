#!/bin/bash

myhome=$(dirname $0)

if [[ $proxy == "" ]]; then 
  echo 'Please set the environment variable "proxy" to host.com:port (e.g. foo.com:1234) to use a proxy'
  echo PROX=""
else
  export http_proxy=http://$proxy
  export PROX="-proxy $proxy"
fi

if [[ -z $PREFIX ]];then
  echo "\$PREFIX variable is required"
  exit
fi

export LD_LIBRARY_NAME=$PREFIX/lib

logname=$(basename $PREFIX)

script -c "make -f $myhome/Makefile_latest.installall all" $logname.log
