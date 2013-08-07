#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh
cd src
if [ $FSLPKG == "logpro" ];then
  chicken-install
elif [ $FSLPKG == "stml" ];then
  cp install.cfg.template install.cfg
  cp requirements.scm.template requirements.scm
  make 
  make install
else
  make
  make install PREFIX=$PREFIX
fi
