#!/usr/bin/env bash

# Run your step here

cksetupsh=$PREFIX/setup-chicken4x.sh
cksetupcsh=$PREFIX/setup-chicken4x.csh
setupsh=$PREFIX/buildsetup.sh

mkdir -p $PREFIX

# File for users to source to run chicken
echo "# Source me to setup to to run chicken" > $cksetupsh
echo "export PATH=$PREFIX/bin:\$PATH" > $cksetupsh
echo "export LD_LIBRARY_PATH=$PREFIX/lib" >> $cksetupsh

# tcsh version
echo "setenv PATH $PREFIX/bin:\$PATH" > $cksetupcsh
echo "setenv LD_LIBRARY_PATH $PREFIX/lib" >> $cksetupcsh

# File to source for build process
echo "export PATH=$PREFIX/bin:\$PATH" > $setupsh
echo "export LD_LIBRARY_PATH=$PREFIX/lib" >> $setupsh

if [[ $proxy == "" ]]; then 
  echo 'Please set the environment variable "proxy" to host.com:port (e.g. foo.com:1234) to use a proxy'
else
  echo "export http_proxy=http://$proxy" >> $setupsh
  echo "export PROX=\"-proxy $proxy\""   >> $setupsh
fi

echo "export PREFIX=$PREFIX" >> $setupsh

echo ALL DONE
